test_that("boost_tree with catboost", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("modeldata")

  suppressPackageStartupMessages({
    library(catboost)
    library(dplyr)
  })

  data("penguins", package = "modeldata")

  penguins <- penguins[complete.cases(penguins), ]

  expect_snapshot(
    boost_tree() |> set_engine("catboost") |> set_mode("regression")
  )
  expect_snapshot(
    boost_tree() |>
      set_engine("catboost", iterations = 100) |>
      set_mode("classification")
  )

  # regression -----------------------------------------------------------------
  expect_no_error({
    pars_fit_1 <-
      boost_tree() |>
      set_engine("catboost", random_seed = 1) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  })

  expect_no_error({
    pars_preds_1 <-
      predict(pars_fit_1, penguins)
  })

  peng_y <- penguins$bill_length_mm

  peng_x <- penguins |>
    select(-bill_length_mm) |>
    catboost::catboost.load_pool(
      label = peng_y
    )

  params_1 <- list(
    verbose = 0,
    allow_writing_files = FALSE,
    random_seed = 1,
    learning_rate = 0.03
  )

  cat_fit_1 <- catboost::catboost.train(
    learn_pool = peng_x,
    params = params_1
  )

  cat_preds_1 <- catboost::catboost.predict(cat_fit_1, peng_x)

  expect_equal(pars_preds_1$.pred, cat_preds_1)

  # regression, adjusting a primary argument
  expect_no_error({
    pars_fit_2 <-
      boost_tree(trees = 20) |>
      set_engine("catboost", random_seed = 1) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  })

  expect_no_error({
    pars_preds_2 <-
      predict(pars_fit_2, penguins)
  })

  params_2 <- list(
    verbose = 0,
    allow_writing_files = FALSE,
    random_seed = 1,
    learning_rate = 0.03,
    iterations = 20
  )

  cat_fit_2 <- catboost::catboost.train(
    learn_pool = peng_x,
    params = params_2
  )

  cat_preds_2 <- catboost::catboost.predict(cat_fit_2, peng_x)

  expect_equal(pars_preds_2$.pred, cat_preds_2)

  # regression, adjusting an engine argument
  expect_no_error({
    pars_fit_3 <-
      boost_tree() |>
      set_engine("catboost", random_seed = 1, l2_leaf_reg = 16) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  })

  expect_no_error({
    pars_preds_3 <-
      predict(pars_fit_3, penguins)
  })

  params_3 <- list(
    verbose = 0,
    allow_writing_files = FALSE,
    random_seed = 1,
    learning_rate = 0.03,
    l2_leaf_reg = 16
  )

  cat_fit_3 <- catboost::catboost.train(
    learn_pool = peng_x,
    params = params_3
  )

  cat_preds_3 <- catboost::catboost.predict(cat_fit_3, peng_x)

  expect_equal(pars_preds_3$.pred, cat_preds_3)

  # classification -------------------------------------------------------------

  # multiclass
  expect_no_error({
    pars_fit_4 <-
      boost_tree() |>
      set_engine("catboost", random_seed = 1) |>
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

  peng_y_c <- penguins$species

  peng_m_c <- penguins |>
    select(-species)

  peng_x_c <- catboost::catboost.load_pool(
    data = peng_m_c,
    label = peng_y_c
  )

  params_4 <- list(
    objective = "MultiClass",
    verbose = 0,
    allow_writing_files = FALSE,
    random_seed = 1,
    learning_rate = 0.03
  )

  cat_fit_4 <- catboost::catboost.train(
    learn_pool = peng_x_c,
    params = params_4
  )

  cat_preds_4 <- catboost::catboost.predict(
    cat_fit_4,
    peng_x_c,
    prediction_type = "Probability"
  )

  expect_equal(pars_preds_4_mtx, cat_preds_4)

  # check class predictions
  pars_preds_5 <-
    predict(pars_fit_4, penguins, type = "class") |>
    (\(x) x[[".pred_class"]])() |>
    as.character()

  cat_preds_5 <- apply(pars_preds_4_mtx, 1, function(x) which.max(x)) |>
    factor(labels = c("Adelie", "Chinstrap", "Gentoo")) |>
    as.character()

  expect_equal(pars_preds_5, cat_preds_5)

  # classification on a two-level outcome
  expect_no_error({
    pars_fit_6 <-
      boost_tree() |>
      set_engine("catboost", random_seed = 1) |>
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

  peng_y_b <- penguins$sex

  peng_m_b <- penguins |>
    select(-sex)

  peng_x_b <- catboost::catboost.load_pool(
    data = peng_m_b,
    label = peng_y_b
  )

  params_6 <- list(
    objective = "Logloss",
    verbose = 0,
    allow_writing_files = FALSE,
    random_seed = 1,
    learning_rate = 0.03
  )

  cat_fit_6 <-
    catboost::catboost.train(
      learn_pool = peng_x_b,
      params = params_6
    )

  cat_preds_6 <- catboost::catboost.predict(
    cat_fit_6,
    peng_x_b,
    prediction_type = "Probability"
  )

  expect_equal(pars_preds_6_b, cat_preds_6)
})

test_that("bonsai correctly determines loss_function when label is a factor", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("modeldata")

  suppressPackageStartupMessages({
    library(catboost)
    library(dplyr)
  })

  data("penguins", package = "modeldata")
  penguins <- penguins[complete.cases(penguins), ]

  expect_no_error({
    bst <- train_catboost(
      x = penguins[, c("bill_length_mm", "bill_depth_mm")],
      y = penguins[["sex"]],
      iterations = 5,
      allow_writing_files = FALSE
    )
  })

  expect_equal(
    catboost::catboost.get_model_params(bst)$flat_params$loss_function,
    "Logloss"
  )

  expect_no_error({
    bst <- train_catboost(
      x = penguins[, c("bill_length_mm", "bill_depth_mm")],
      y = penguins[["species"]],
      iterations = 5,
      allow_writing_files = FALSE
    )
  })
  expect_equal(
    catboost::catboost.get_model_params(bst)$flat_params$loss_function,
    "MultiClass"
  )
})

test_that("catboost warns if user uses `param` argument in set_engine()", {
  skip_if_not_installed("catboost")

  mod_spec <- boost_tree() |>
    set_engine("catboost", params = list(loss_function = "MAPE")) |>
    set_mode("regression")

  expect_snapshot(
    mod_spec |>
      fit(mpg ~ ., mtcars)
  )
})

test_that("catboost with case weights", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("modeldata")

  suppressPackageStartupMessages({
    library(catboost)
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
      set_engine("catboost", random_seed = 1) |>
      set_mode("regression") |>
      fit(
        bill_length_mm ~ .,
        data = penguins,
        case_weights = importance_weights(penguins_wts)
      )
  })

  pars_preds_1 <- predict(pars_fit_1, penguins)

  peng_y <- penguins$bill_length_mm

  peng_m <- penguins |>
    select(-bill_length_mm)

  peng_x <-
    catboost::catboost.load_pool(
      data = peng_m,
      label = peng_y,
      weight = penguins_wts
    )

  params_1 <- list(
    verbose = 0,
    allow_writing_files = FALSE,
    random_seed = 1,
    learning_rate = 0.03
  )

  cat_fit_1 <-
    catboost::catboost.train(
      learn_pool = peng_x,
      params = params_1
    )

  cat_preds_1 <- catboost::catboost.predict(cat_fit_1, peng_x)

  expect_equal(pars_preds_1$.pred, cat_preds_1)
})

test_that("catboost predict functions pass ... to catboost.predict", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("modeldata")

  suppressPackageStartupMessages({
    library(catboost)
    library(dplyr)
  })

  data("penguins", package = "modeldata")
  penguins <- penguins[complete.cases(penguins), ]

  num_trees <- 10

  # regression ----------------------------------------------------------------
  reg_fit <-
    boost_tree(trees = num_trees) |>
    set_engine("catboost", random_seed = 1) |>
    set_mode("regression") |>
    fit(bill_length_mm ~ ., data = penguins)

  new_data <- penguins[1:5, ]

  # predictions with all trees

  preds_all <- predict_catboost_regression_numeric(reg_fit, new_data)

  # predictions with ntree_end = 3 (uses first 3 trees)
  preds_subset <- predict_catboost_regression_numeric(
    reg_fit,
    new_data,
    ntree_end = 3
  )

  # should be different (proving ntree_end was passed through)
  expect_false(identical(preds_all, preds_subset))

  # verify against direct catboost call
  pool <- catboost::catboost.load_pool(data = new_data)
  cat_preds_subset <- catboost::catboost.predict(
    reg_fit$fit,
    pool,
    ntree_end = 3
  )
  expect_equal(preds_subset, cat_preds_subset)

  # classification ------------------------------------------------------------
  clf_fit <-
    boost_tree(trees = num_trees) |>
    set_engine("catboost", random_seed = 1) |>
    set_mode("classification") |>
    fit(species ~ ., data = penguins)

  new_data_clf <- penguins[1:5, ]

  # class predictions
  preds_class_all <- predict_catboost_classification_class(
    clf_fit,
    new_data_clf
  )
  preds_class_subset <- predict_catboost_classification_class(
    clf_fit,
    new_data_clf,
    ntree_end = 3
  )

  # prob predictions
  preds_prob_all <- predict_catboost_classification_prob(clf_fit, new_data_clf)
  preds_prob_subset <- predict_catboost_classification_prob(
    clf_fit,
    new_data_clf,
    ntree_end = 3
  )

  # should be different
  expect_false(identical(preds_prob_all, preds_prob_subset))

  # verify prob predictions against direct catboost call
  pool_clf <- catboost::catboost.load_pool(data = new_data_clf)
  cat_preds_prob_subset <- catboost::catboost.predict(
    clf_fit$fit,
    pool_clf,
    prediction_type = "Probability",
    ntree_end = 3
  )
  colnames(cat_preds_prob_subset) <- clf_fit$lvl
  expect_equal(as.matrix(preds_prob_subset), cat_preds_prob_subset)
})

test_that("multi_predict() works for catboost regression", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("modeldata")

  suppressPackageStartupMessages({
    library(catboost)
    library(dplyr)
  })

  data("penguins", package = "modeldata")
  penguins <- penguins[complete.cases(penguins), ]

  num_trees <- 5

  expect_no_error({
    reg_fit <-
      boost_tree(trees = num_trees) |>
      set_engine("catboost", random_seed = 1) |>
      set_mode("regression") |>
      fit(bill_length_mm ~ ., data = penguins)
  })

  new_data <- penguins[1:3, ]

  multi_preds <-
    multi_predict(
      reg_fit,
      new_data = new_data,
      trees = seq_len(num_trees)
    )

  # should have one row per observation

  expect_equal(nrow(multi_preds), nrow(new_data))

  # each .pred should be a tibble with num_trees rows
  pred_tbl <- multi_preds$.pred[[1]]
  expect_s3_class(pred_tbl, "tbl_df")
  expect_equal(nrow(pred_tbl), num_trees)

  # should look like regression predictions
  expect_named(pred_tbl, c("trees", ".pred"))
  expect_type(pred_tbl[[".pred"]], "double")

  # predictions should vary by number of trees
  expect_false(all(pred_tbl$.pred == pred_tbl$.pred[1]))
})

test_that("multi_predict() works for catboost classification", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("modeldata")

  suppressPackageStartupMessages({
    library(catboost)
    library(dplyr)
  })

  data("penguins", package = "modeldata")
  penguins <- penguins[complete.cases(penguins), ]

  num_trees <- 5

  # multiclass ----------------------------------------------------------------
  expect_no_error({
    clf_multiclass_fit <-
      boost_tree(trees = num_trees) |>
      set_engine("catboost", random_seed = 1) |>
      set_mode("classification") |>
      fit(species ~ ., data = penguins)
  })

  new_data <- penguins[1:3, ]

  # test class predictions (default type)
  multi_preds_class <-
    multi_predict(
      clf_multiclass_fit,
      new_data = new_data,
      trees = seq_len(num_trees)
    )

  expect_equal(nrow(multi_preds_class), nrow(new_data))

  pred_tbl <- multi_preds_class$.pred[[1]]
  expect_s3_class(pred_tbl, "tbl_df")
  expect_equal(nrow(pred_tbl), num_trees)
  expect_named(pred_tbl, c("trees", ".pred_class"))
  expect_s3_class(pred_tbl[[".pred_class"]], "factor")
  expect_true(all(
    as.character(pred_tbl[[".pred_class"]]) %in% levels(penguins[["species"]])
  ))

  # test probability predictions
  multi_preds_prob <-
    multi_predict(
      clf_multiclass_fit,
      new_data = new_data,
      trees = seq_len(num_trees),
      type = "prob"
    )

  pred_tbl_prob <- multi_preds_prob$.pred[[1]]
  expect_s3_class(pred_tbl_prob, "tbl_df")
  expect_equal(nrow(pred_tbl_prob), num_trees)
  expect_true("trees" %in% names(pred_tbl_prob))
  expect_true(all(
    paste0(".pred_", levels(penguins$species)) %in% names(pred_tbl_prob)
  ))

  # binary --------------------------------------------------------------------
  expect_no_error({
    clf_binary_fit <-
      boost_tree(trees = num_trees) |>
      set_engine("catboost", random_seed = 1) |>
      set_mode("classification") |>
      fit(sex ~ ., data = penguins)
  })

  multi_preds_binary <-
    multi_predict(
      clf_binary_fit,
      new_data = new_data,
      trees = seq_len(num_trees)
    )

  pred_tbl_binary <- multi_preds_binary$.pred[[1]]
  expect_s3_class(pred_tbl_binary, "tbl_df")
  expect_named(pred_tbl_binary, c("trees", ".pred_class"))
  expect_s3_class(pred_tbl_binary[[".pred_class"]], "factor")
  expect_true(all(
    as.character(pred_tbl_binary[[".pred_class"]]) %in%
      levels(penguins[["sex"]])
  ))
})
