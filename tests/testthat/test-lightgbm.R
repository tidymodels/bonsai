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

  # ----------------------------------------------------------------------------
  # regression

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

  # ----------------------------------------------------------------------------
  # classification



})


