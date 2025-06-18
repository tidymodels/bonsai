withr::local_envvar("OMP_THREAD_LIMIT" = 1)

test_that("boost_tree with lightgbm", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("modeldata")

  suppressPackageStartupMessages({
    library(lightgbm)
    library(dplyr)
  })

  data("penguins", package = "modeldata")

  penguins <- penguins[complete.cases(penguins), ]

  expect_snapshot(
    boost_tree() |> set_engine("lightgbm") |> set_mode("regression")
  )
  expect_snapshot(
    boost_tree() |>
      set_engine("lightgbm", nrounds = 100) |>
      set_mode("classification")
  )

  # regression -----------------------------------------------------------------
  expect_no_error({
    pars_fit_1 <-
      boost_tree() |>
      set_engine("lightgbm") |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  })

  expect_no_error({
    pars_preds_1 <-
      predict(pars_fit_1, penguins)
  })

  peng <-
    penguins |>
    mutate(across(where(is.character), \(x) as.factor(x))) |>
    mutate(across(where(is.factor), \(x) as.integer(x) - 1))

  peng_y <- peng$bill_length_mm

  peng_m <- peng |>
    select(-bill_length_mm) |>
    as.matrix()

  peng_x <-
    lgb.Dataset(
      data = peng_m,
      label = peng_y,
      params = list(feature_pre_filter = FALSE),
      categorical_feature = c(1L, 2L, 6L)
    )

  params_1 <- list(
    objective = "regression"
  )

  lgbm_fit_1 <-
    lightgbm::lgb.train(
      data = peng_x,
      params = params_1,
      verbose = -1
    )

  lgbm_preds_1 <- predict(lgbm_fit_1, peng_m)

  expect_equal(pars_preds_1$.pred, lgbm_preds_1)

  # regression, adjusting a primary argument
  expect_no_error({
    pars_fit_2 <-
      boost_tree(trees = 20) |>
      set_engine("lightgbm") |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  })

  expect_no_error({
    pars_preds_2 <-
      predict(pars_fit_2, penguins)
  })

  params_2 <- list(
    objective = "regression",
    num_iterations = 20
  )

  lgbm_fit_2 <-
    lightgbm::lgb.train(
      data = peng_x,
      params = params_2,
      verbose = -1
    )

  lgbm_preds_2 <- predict(lgbm_fit_2, peng_m)

  expect_equal(pars_preds_2$.pred, lgbm_preds_2)

  # regression, adjusting an engine argument
  expect_no_error({
    pars_fit_3 <-
      boost_tree() |>
      set_engine("lightgbm", lambda_l2 = .5) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  })

  expect_no_error({
    pars_preds_3 <-
      predict(pars_fit_3, penguins)
  })

  params_3 <- list(
    objective = "regression",
    lambda_l2 = .5
  )

  lgbm_fit_3 <-
    lightgbm::lgb.train(
      data = peng_x,
      params = params_3,
      verbose = -1
    )

  lgbm_preds_3 <- predict(lgbm_fit_3, peng_m)

  expect_equal(pars_preds_3$.pred, lgbm_preds_3)

  # classification -------------------------------------------------------------

  # multiclass
  expect_no_error({
    pars_fit_4 <-
      boost_tree() |>
      set_engine("lightgbm") |>
      set_mode("classification") |>
      fit(species ~ ., data = penguins)
  })

  expect_no_error({
    pars_preds_4 <-
      predict(pars_fit_4, penguins, type = "prob")
    pars_preds_raw_4 <-
      predict(pars_fit_4, penguins, type = "raw")
  })

  expect_equal(nrow(pars_preds_raw_4), nrow(penguins))
  expect_equal(ncol(pars_preds_raw_4), 3)

  pars_preds_4_mtx <- as.matrix(pars_preds_4)
  dimnames(pars_preds_4_mtx) <- NULL

  peng_y_c <- peng$species

  peng_m_c <- peng |>
    select(-species) |>
    as.matrix()

  peng_x_c <-
    lgb.Dataset(
      data = peng_m_c,
      label = peng_y_c,
      params = list(feature_pre_filter = FALSE),
      categorical_feature = c(1L, 6L),
    )

  params_4 <- list(
    objective = "multiclass",
    num_class = 3
  )

  lgbm_fit_4 <-
    lightgbm::lgb.train(
      data = peng_x_c,
      params = params_4,
      verbose = -1
    )

  lgbm_preds_4 <-
    predict(lgbm_fit_4, peng_m_c) |>
    reshape_lightgbm_multiclass_preds(num_rows = nrow(peng_m_c))

  expect_equal(pars_preds_4_mtx, lgbm_preds_4)

  # check class predictions
  pars_preds_5 <-
    predict(pars_fit_4, penguins, type = "class") |>
    (\(x) x[[".pred_class"]])() |>
    as.character()

  lgbm_preds_5 <- apply(pars_preds_4_mtx, 1, function(x) which.max(x)) |>
    factor(labels = c("Adelie", "Chinstrap", "Gentoo")) |>
    as.character()

  expect_equal(pars_preds_5, lgbm_preds_5)

  # classification on a two-level outcome
  expect_no_error({
    pars_fit_6 <-
      boost_tree() |>
      set_engine("lightgbm") |>
      set_mode("classification") |>
      fit(sex ~ ., data = penguins)
  })

  expect_no_error({
    pars_preds_6 <-
      predict(pars_fit_6, penguins, type = "prob")
    pars_preds_raw_6 <-
      predict(pars_fit_6, penguins, type = "raw")
  })

  expect_equal(length(pars_preds_raw_6), nrow(penguins))
  expect_false(identical(pars_preds_6, pars_preds_raw_6))

  pars_preds_6_b <- pars_preds_6$.pred_male

  peng_y_b <- peng$sex

  peng_m_b <- peng |>
    select(-sex) |>
    as.matrix()

  peng_x_b <-
    lgb.Dataset(
      data = peng_m_b,
      label = peng_y_b,
      params = list(feature_pre_filter = FALSE),
      categorical_feature = c(1L, 2L),
    )

  params_6 <- list(
    objective = "binary",
    num_class = 1
  )

  lgbm_fit_6 <-
    lightgbm::lgb.train(
      data = peng_x_b,
      params = params_6,
      verbose = -1
    )

  lgbm_preds_6 <- predict(lgbm_fit_6, peng_m_b)

  expect_equal(pars_preds_6_b, lgbm_preds_6)
})

test_that("bonsai applies dataset parameters (#77)", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("modeldata")

  suppressPackageStartupMessages({
    library(lightgbm)
    library(dplyr)
  })

  data("penguins", package = "modeldata")

  penguins <- penguins[complete.cases(penguins), ]

  # regression -----------------------------------------------------------------
  expect_no_error({
    pars_fit_1 <-
      boost_tree() |>
      set_engine("lightgbm", linear_tree = TRUE) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  })

  expect_no_error({
    pars_preds_1 <-
      predict(pars_fit_1, penguins)
  })

  peng <-
    penguins |>
    mutate(across(where(is.character), \(x) as.factor(x))) |>
    mutate(across(where(is.factor), \(x) as.integer(x) - 1))

  peng_y <- peng$bill_length_mm

  peng_m <- peng |>
    select(-bill_length_mm) |>
    as.matrix()

  peng_x <-
    lgb.Dataset(
      data = peng_m,
      label = peng_y,
      params = list(feature_pre_filter = FALSE, linear_tree = TRUE),
      categorical_feature = c(1L, 2L, 6L)
    )

  params_1 <- list(
    objective = "regression"
  )

  lgbm_fit_1 <-
    lightgbm::lgb.train(
      data = peng_x,
      params = params_1,
      verbose = -1
    )

  lgbm_preds_1 <- predict(lgbm_fit_1, peng_m)

  expect_equal(pars_preds_1$.pred, lgbm_preds_1)
  expect_true(pars_fit_1$fit$params$linear_tree)
})

test_that("bonsai correctly determines objective when label is a factor", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("modeldata")

  suppressPackageStartupMessages({
    library(lightgbm)
    library(dplyr)
  })

  data("penguins", package = "modeldata")
  penguins <- penguins[complete.cases(penguins), ]

  expect_no_error({
    bst <- train_lightgbm(
      x = penguins[, c("bill_length_mm", "bill_depth_mm")],
      y = penguins[["sex"]],
      num_iterations = 5,
      verbose = -1L
    )
  })
  expect_equal(bst$params$objective, "binary")
  expect_equal(bst$params$num_class, 1)

  expect_no_error({
    bst <- train_lightgbm(
      x = penguins[, c("bill_length_mm", "bill_depth_mm")],
      y = penguins[["species"]],
      num_iterations = 5,
      verbose = -1L
    )
  })
  expect_equal(bst$params$objective, "multiclass")
  expect_equal(bst$params$num_class, 3)
})

test_that("bonsai handles mtry vs mtry_prop gracefully", {
  skip_if_not_installed("modeldata")

  data("penguins", package = "modeldata")

  penguins <- penguins[complete.cases(penguins), ]

  # supply no mtry
  expect_no_error({
    pars_fit_1 <-
      boost_tree() |>
      set_engine("lightgbm") |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  })

  expect_equal(
    extract_fit_engine(pars_fit_1)$params$feature_fraction_bynode,
    1
  )

  # supply mtry = 1 (edge cases)
  expect_no_error({
    pars_fit_2 <-
      boost_tree(mtry = 1) |>
      set_engine("lightgbm", counts = TRUE) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  })

  expect_equal(
    extract_fit_engine(pars_fit_2)$params$feature_fraction_bynode,
    1 / (ncol(penguins) - 1)
  )

  expect_no_error({
    pars_fit_3 <-
      boost_tree(mtry = 1) |>
      set_engine("lightgbm", counts = FALSE) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  })

  expect_equal(
    extract_fit_engine(pars_fit_3)$params$feature_fraction_bynode,
    1
  )

  # supply a count (with default counts = TRUE)
  expect_no_error({
    pars_fit_4 <-
      boost_tree(mtry = 3) |>
      set_engine("lightgbm") |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  })

  expect_equal(
    extract_fit_engine(pars_fit_4)$params$feature_fraction_bynode,
    3 / (ncol(penguins) - 1)
  )

  # supply a proportion when count expected
  expect_snapshot_error({
    pars_fit_5 <-
      boost_tree(mtry = .5) |>
      set_engine("lightgbm") |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  })

  # supply a count when proportion expected
  expect_snapshot_error({
    pars_fit_6 <-
      boost_tree(mtry = 3) |>
      set_engine("lightgbm", counts = FALSE) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  })

  # supply a feature fraction argument rather than mtry
  # TODO: is there any way to extend parsnip's warning here to
  # point users to mtry?
  # will see "The argument `feature_fraction_bynode` cannot be..." (#95)
  suppressWarnings(
    pars_fit_7 <-
      boost_tree() |>
      set_engine("lightgbm", feature_fraction_bynode = .5) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  )

  expect_equal(
    extract_fit_engine(pars_fit_7)$params$feature_fraction_bynode,
    1
  )

  # supply both feature fraction and mtry (#95)
  suppressWarnings(expect_error({
    pars_fit_8 <-
      boost_tree(mtry = .5) |>
      set_engine("lightgbm", feature_fraction_bynode = .5) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  }))

  # will see "The argument `feature_fraction_bynode` cannot be..." (#95)
  suppressWarnings(
    pars_fit_9 <-
      boost_tree(mtry = 2) |>
      set_engine("lightgbm", feature_fraction_bynode = .5) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  )

  expect_equal(
    extract_fit_engine(pars_fit_9)$params$feature_fraction_bynode,
    2 / (ncol(penguins) - 1)
  )
})

test_that("tuning mtry vs mtry_prop", {
  skip_if_not_installed("tune")
  skip_if_not_installed("rsample")
  skip_if_not_installed("modeldata")

  data("penguins", package = "modeldata")

  penguins <- penguins[complete.cases(penguins), ]

  set.seed(1)

  suppressMessages(
    expect_no_error({
      gbm_tune <- tune::tune_grid(
        boost_tree(mtry = tune::tune()) |>
          set_engine("lightgbm") |>
          set_mode("regression"),
        grid = 4,
        preprocessor = bill_length_mm ~ .,
        resamples = rsample::bootstraps(penguins, times = 5)
      )
    })
  )

  mtrys <- unique(tune::collect_metrics(gbm_tune)$mtry)

  expect_equal(length(mtrys), 4)
  expect_true(all(mtrys >= 1))

  # supply tune() without tuning
  expect_snapshot(
    {
      boost_tree(mtry = tune::tune()) |>
        set_engine("lightgbm") |>
        set_mode("regression") |>
        fit(bill_length_mm ~ ., data = penguins)
    },
    error = TRUE
  )
})

test_that("lightgbm warns if user uses `param` argument in set_engine()", {
  skip_if_not_installed("lightgbm")

  mod_spec <- boost_tree() |>
    set_engine("lightgbm", params = list(objective = "multiclassova")) |>
    set_mode("regression")

  expect_snapshot(
    mod_spec |>
      fit(mpg ~ ., mtcars)
  )
})

test_that("training wrapper warns on protected arguments", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("modeldata")

  data("penguins", package = "modeldata")

  penguins <- penguins[complete.cases(penguins), ]

  expect_snapshot(
    .res <- boost_tree() |>
      set_engine("lightgbm", colnames = paste0("X", 1:ncol(penguins))) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  )

  expect_snapshot(
    .res <- boost_tree() |>
      set_engine(
        "lightgbm",
        colnames = paste0("X", 1:ncol(penguins)),
        callbacks = list(p = print)
      ) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  )

  expect_snapshot(
    .res <-
      boost_tree() |>
      set_engine(
        "lightgbm",
        colnames = paste0("X", 1:ncol(penguins))
      ) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  )

  expect_snapshot(
    error = TRUE,
    boost_tree() |>
      set_engine("lightgbm", n_iter = 10) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  )

  expect_snapshot(
    error = TRUE,
    boost_tree() |>
      set_engine("lightgbm", num_tree = 10) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  )

  expect_snapshot(
    error = TRUE,
    boost_tree() |>
      set_engine("lightgbm", min_split_gain = 2) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  )

  expect_snapshot(
    error = TRUE,
    boost_tree() |>
      set_engine("lightgbm", min_split_gain = 2, lambda_l2 = .5) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  )
})

test_that("training wrapper passes stop_iter correctly", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("modeldata")

  data("penguins", package = "modeldata")

  penguins <- penguins[complete.cases(penguins), ]

  expect_no_error(
    pars_fit_1 <-
      boost_tree(stop_iter = 10) |>
      set_engine("lightgbm") |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  )

  # will see "The argument `early_stopping_round` cannot be..." (#95)
  suppressWarnings(
    pars_fit_2 <-
      boost_tree() |>
      set_engine("lightgbm", early_stopping_round = 10) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  )

  expect_no_error(
    pars_fit_3 <-
      boost_tree() |>
      set_engine("lightgbm") |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  )

  expect_no_error(
    pars_fit_4 <-
      boost_tree() |>
      set_engine("lightgbm", validation = .2) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  )

  expect_no_error(
    pars_fit_5 <-
      boost_tree(stop_iter = 10) |>
      set_engine("lightgbm", validation = .2) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  )

  # detect early_stopping round in the model fit
  expect_equal(pars_fit_1$fit$params$early_stopping_round, 10)
  expect_null(pars_fit_2$fit$params$early_stopping_round)
  expect_null(pars_fit_3$fit$params$early_stopping_round)
  expect_null(pars_fit_4$fit$params$early_stopping_round)
  expect_equal(pars_fit_5$fit$params$early_stopping_round, 10)

  # detect validation in the model fit
  expect_true(!is.na(pars_fit_1$fit$best_score))
  expect_true(is.na(pars_fit_2$fit$best_score))
  expect_true(is.na(pars_fit_3$fit$best_score))
  expect_true(!is.na(pars_fit_4$fit$best_score))
  expect_true(!is.na(pars_fit_5$fit$best_score))
})

test_that("training wrapper handles bagging correctly", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("modeldata")

  data("penguins", package = "modeldata")

  penguins <- penguins[complete.cases(penguins), ]

  pars_fit_1 <-
    boost_tree() |>
    set_engine("lightgbm") |>
    set_mode("regression") |>
    fit(bill_length_mm ~ ., data = penguins)

  pars_fit_2 <-
    boost_tree(sample_size = .5) |>
    set_engine("lightgbm") |>
    set_mode("regression") |>
    fit(bill_length_mm ~ ., data = penguins)

  pars_fit_3 <-
    boost_tree(sample_size = .5) |>
    set_engine("lightgbm", bagging_freq = 2) |>
    set_mode("regression") |>
    fit(bill_length_mm ~ ., data = penguins)

  expect_equal(pars_fit_1$fit$params$bagging_fraction, 1)
  expect_null(pars_fit_1$fit$params$bagging_freq)

  expect_equal(pars_fit_2$fit$params$bagging_fraction, .5)
  expect_equal(pars_fit_2$fit$params$bagging_freq, 1)

  expect_equal(pars_fit_3$fit$params$bagging_fraction, .5)
  expect_equal(pars_fit_3$fit$params$bagging_freq, 2)
})

test_that("multi_predict() predicts classes if 'type' not given ", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("modeldata")

  suppressPackageStartupMessages({
    library(lightgbm)
    library(dplyr)
  })

  data("penguins", package = "modeldata")
  penguins <- penguins[complete.cases(penguins), ]
  penguins_subset <- penguins[1:10, ]
  penguins_subset_numeric <-
    penguins_subset |>
    mutate(across(where(is.character), \(x) as.factor(x))) |>
    mutate(across(where(is.factor), \(x) as.integer(x) - 1))

  num_iterations <- 5

  # classification (multiclass) ------------------------------------------------
  expect_no_error({
    clf_multiclass_fit <-
      boost_tree(trees = num_iterations) |>
      set_engine("lightgbm") |>
      set_mode("classification") |>
      fit(species ~ ., data = penguins)
  })
  expect_equal(clf_multiclass_fit$fit$current_iter(), num_iterations)

  new_data <-
    penguins_subset_numeric |>
    select(-species) |>
    as.matrix()

  multi_preds <-
    multi_predict(
      clf_multiclass_fit,
      new_data = new_data[1, , drop = FALSE],
      trees = seq_len(num_iterations)
    )

  # should be a tibble
  pred_tbl <- multi_preds$.pred[[1]]
  expect_s3_class(pred_tbl, "tbl_df")

  # should look like class predictions
  expect_named(pred_tbl, c("trees", ".pred_class"))
  expect_s3_class(pred_tbl[[".pred_class"]], "factor")
  expect_true(all(
    as.character(pred_tbl[[".pred_class"]]) %in% levels(penguins[["species"]])
  ))

  # classification (binary) ------------------------------------------------
  expect_no_error({
    clf_binary_fit <-
      boost_tree(trees = num_iterations) |>
      set_engine("lightgbm") |>
      set_mode("classification") |>
      fit(sex ~ ., data = penguins)
  })
  expect_equal(clf_binary_fit$fit$current_iter(), num_iterations)

  new_data <-
    penguins_subset_numeric |>
    select(-sex) |>
    as.matrix()

  multi_preds <-
    multi_predict(
      clf_binary_fit,
      new_data = new_data[1, , drop = FALSE],
      trees = seq_len(num_iterations)
    )

  # should be a tibble
  pred_tbl <- multi_preds$.pred[[1]]
  expect_s3_class(pred_tbl, "tbl_df")

  # should look like class predictions
  expect_named(pred_tbl, c("trees", ".pred_class"))
  expect_s3_class(pred_tbl[[".pred_class"]], "factor")
  expect_true(all(
    as.character(pred_tbl[[".pred_class"]]) %in% levels(penguins[["sex"]])
  ))
})

test_that("lightgbm with case weights", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("modeldata")

  suppressPackageStartupMessages({
    library(lightgbm)
    library(dplyr)
  })

  data("penguins", package = "modeldata")

  penguins <- penguins[complete.cases(penguins), ]

  set.seed(1)
  penguins_wts <- runif(nrow(penguins))

  # regression -----------------------------------------------------------------
  expect_no_error({
    pars_fit_1 <-
      boost_tree() |>
      set_engine("lightgbm") |>
      set_mode("regression") |>
      fit(
        bill_length_mm ~ .,
        data = penguins,
        case_weights = importance_weights(penguins_wts)
      )
  })

  pars_preds_1 <- predict(pars_fit_1, penguins)

  peng <-
    penguins |>
    mutate(across(where(is.character), \(x) as.factor(x))) |>
    mutate(across(where(is.factor), \(x) as.integer(x) - 1))

  peng_y <- peng$bill_length_mm

  peng_m <- peng |>
    select(-bill_length_mm) |>
    as.matrix()

  peng_x <-
    lgb.Dataset(
      data = peng_m,
      label = peng_y,
      params = list(feature_pre_filter = FALSE),
      categorical_feature = c(1L, 2L, 6L),
      weight = penguins_wts
    )

  params_1 <- list(
    objective = "regression"
  )

  lgbm_fit_1 <-
    lightgbm::lgb.train(
      data = peng_x,
      params = params_1,
      verbose = -1
    )

  lgbm_preds_1 <- predict(lgbm_fit_1, peng_m)

  expect_equal(pars_preds_1$.pred, lgbm_preds_1)
})

test_that("sparse data with lightgbm", {
  skip_on_cran()
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("Matrix")

  library(Matrix)
  library(dplyr)

  hep <- modeldata::hepatic_injury_qsar

  lgb_spec <- boost_tree() |>
    set_mode("classification") |>
    set_engine("lightgbm")

  # ------------------------------------------------------------------------------

  hepatic_x_sp <- as.matrix(hep[, -1])
  hepatic_x_sp <- as(hepatic_x_sp, "sparseMatrix")

  sprs_fit <- fit_xy(lgb_spec, hepatic_x_sp, hep$class)
  sprs_prob <- predict(sprs_fit, new_data = hepatic_x_sp, type = "prob")
  sprs_cls <- predict(sprs_fit, new_data = hepatic_x_sp, type = "class")

  # ------------------------------------------------------------------------------

  hepatic_x <- hep[, -1]

  dens_fit <- fit_xy(lgb_spec, hepatic_x, hep$class)
  dens_prob <- predict(dens_fit, new_data = hepatic_x, type = "prob")
  dens_cls <- predict(dens_fit, new_data = hepatic_x, type = "class")

  # ------------------------------------------------------------------------------

  # very small differences in lightgbm probabilities
  expect_equal(sprs_prob, dens_prob, tolerance = .001)
  expect_equal(sprs_cls, dens_cls)
})
