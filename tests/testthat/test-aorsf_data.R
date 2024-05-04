library(testthat)

test_that("regression model object", {

  skip_if_not_installed("aorsf", "0.1.3")

  set.seed(1234)
  aorsf_regr_fit <- aorsf::orsf(
    # everyone's favorite
    data = mtcars, formula = mpg ~ .,
    # faster
    n_tree = 10,
    # requested default from tidymodels
    n_thread = 1
  )

  # formula method
  regr_spec <- rand_forest(trees = 10) %>%
    set_engine("aorsf") %>%
    set_mode("regression")

  # not sure if the expect_error is needed, but copying from censored
  set.seed(1234)
  expect_error(
    bonsai_regr_fit <- fit(regr_spec, data = mtcars, mpg ~ .),
    NA
  )

  expect_equal(
    bonsai_regr_fit$fit,
    aorsf_regr_fit,
    ignore_formula_env = TRUE
  )

})

test_that("classification model object", {

  skip_if_not_installed("aorsf", "0.1.3")

  mtcars$vs <- factor(mtcars$vs)

  set.seed(1234)
  aorsf_clsf_fit <- aorsf::orsf(
    # everyone's favorite
    data = mtcars, formula = vs ~ .,
    # faster
    n_tree = 10,
    # requested default from tidymodels
    n_thread = 1
  )

  # formula method
  clsf_spec <- rand_forest(trees = 10) %>%
    set_engine("aorsf") %>%
    set_mode("classification")

  # not sure if the expect_error is needed, but copying from censored
  set.seed(1234)
  expect_error(
    bonsai_clsf_fit <- fit(clsf_spec, data = mtcars, vs ~ .),
    NA
  )

  expect_equal(
    bonsai_clsf_fit$fit,
    aorsf_clsf_fit,
    ignore_formula_env = TRUE
  )

})

