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

  params <- list(
    objective = "regression"
  )

  lgbm_fit_1 <-
    lightgbm::lgb.train(
      data = peng_x,
      params = params,
      verbose = -1
    )

  lgbm_preds_1 <- predict(lgbm_fit_1, peng_m)

  expect_equal(pars_preds_1$.pred, lgbm_preds_1)


  # do the same with primary argument

  # do the same with engine argument

  # ----------------------------------------------------------------------------
  # classification

  # do the same with classification

})


