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

  lgbm_fit_1 <- catboost::catboost.train(
    learn_pool = peng_x,
    params = params_1
  )

  lgbm_preds_1 <- catboost::catboost.predict(lgbm_fit_1, peng_x)

  expect_equal(pars_preds_1$.pred, lgbm_preds_1)

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

  lgbm_fit_2 <- catboost::catboost.train(
    learn_pool = peng_x,
    params = params_2
  )

  lgbm_preds_2 <- catboost::catboost.predict(lgbm_fit_2, peng_x)

  expect_equal(pars_preds_2$.pred, lgbm_preds_2)

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

  lgbm_fit_3 <- catboost::catboost.train(
    learn_pool = peng_x,
    params = params_3
  )

  lgbm_preds_3 <- catboost::catboost.predict(lgbm_fit_3, peng_x)

  expect_equal(pars_preds_3$.pred, lgbm_preds_3)
})
