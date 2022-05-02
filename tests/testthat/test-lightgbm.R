test_that("boost_tree with lightgbm",{
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("modeldata")

  library(lightgbm)
  library(modeldata)
  library(dplyr)

  data("penguins", package = "modeldata")

  penguins <- penguins[complete.cases(penguins),]

  expect_snapshot(
    boost_tree() %>% set_engine("lightgbm") %>% set_mode("regression")
  )
  expect_snapshot(
    boost_tree() %>% set_engine("lightgbm", nrounds = 100) %>% set_mode("classification")
  )

  # regression -----------------------------------------------------------------
  expect_error_free({
    pars_fit_1 <-
      boost_tree() %>%
      set_engine("lightgbm") %>%
      set_mode("regression") %>%
      fit(bill_length_mm ~ ., data = penguins)
  })

  expect_error_free({
    pars_preds_1 <-
      predict(pars_fit_1, penguins)
  })

  peng <-
    penguins %>%
    mutate(across(where(is.character), ~as.factor(.x))) %>%
    mutate(across(where(is.factor), ~as.integer(.x) - 1))

  peng_y <- peng$bill_length_mm

  peng_m <- peng %>%
    select(-bill_length_mm) %>%
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
  expect_error_free({
    pars_fit_2 <-
      boost_tree(trees = 20) %>%
      set_engine("lightgbm") %>%
      set_mode("regression") %>%
      fit(bill_length_mm ~ ., data = penguins)
  })

  expect_error_free({
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
  expect_error_free({
    pars_fit_3 <-
      boost_tree() %>%
      set_engine("lightgbm", lambda_l2 = .5) %>%
      set_mode("regression") %>%
      fit(bill_length_mm ~ ., data = penguins)
  })

  expect_error_free({
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
  expect_error_free({
    pars_fit_4 <-
      boost_tree() %>%
      set_engine("lightgbm") %>%
      set_mode("classification") %>%
      fit(species ~ ., data = penguins)
  })

  expect_error_free({
    pars_preds_4 <-
      predict(pars_fit_4, penguins, type = "prob")
  })

  pars_preds_4_mtx <- as.matrix(pars_preds_4)
  dimnames(pars_preds_4_mtx) <- NULL

  peng_y_c <- peng$species

  peng_m_c <- peng %>%
    select(-species) %>%
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

  lgbm_preds_4 <- predict(lgbm_fit_4, peng_m_c, reshape = TRUE)

  expect_equal(pars_preds_4_mtx, lgbm_preds_4)

  # check class predictions
  pars_preds_5 <-
    predict(pars_fit_4, penguins, type = "class") %>%
    `[[`(".pred_class") %>%
    as.character()

  lgbm_preds_5 <- apply(pars_preds_4_mtx, 1, function(x) which.max(x)) %>%
    factor(labels = c("Adelie", "Chinstrap", "Gentoo")) %>%
    as.character()

  expect_equal(pars_preds_5, lgbm_preds_5)

  # classification on a two-level outcome
  expect_error_free({
    pars_fit_6 <-
      boost_tree() %>%
      set_engine("lightgbm") %>%
      set_mode("classification") %>%
      fit(sex ~ ., data = penguins)
  })

  expect_error_free({
    pars_preds_6 <-
      predict(pars_fit_6, penguins, type = "prob")
  })

  pars_preds_6_b <- pars_preds_6$.pred_male

  peng_y_b <- peng$sex

  peng_m_b <- peng %>%
    select(-sex) %>%
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


