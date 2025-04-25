library(testthat)

withr::local_envvar("OMP_THREAD_LIMIT" = 1)

mtcars_orsf <- mtcars
mtcars_orsf$vs <- factor(mtcars_orsf$vs)
mtcars_na <- mtcars_orsf
mtcars_na$cyl[1] <- NA

test_that("regression model object", {
  skip_if_not_installed("aorsf")

  set.seed(321)
  wts <- sample(0:5, size = nrow(mtcars_orsf), replace = TRUE)

  set.seed(1234)
  aorsf_regr_fit <-
    aorsf::orsf(
      # everyone's favorite
      data = mtcars_orsf,
      formula = mpg ~ .,
      # faster
      n_tree = 10,
      # requested default from tidymodels
      n_thread = 1
    )

  set.seed(1234)
  aorsf_regr_fit_wtd <- aorsf::orsf_update(aorsf_regr_fit, weights = wts)

  # formula method
  regr_spec <-
    rand_forest(trees = 10) |>
    set_engine("aorsf") |>
    set_mode("regression")

  set.seed(1234)
  expect_no_condition(
    bonsai_regr_fit <-
      fit(
        regr_spec,
        data = mtcars_orsf,
        formula = mpg ~ .
      )
  )

  set.seed(1234)
  expect_no_condition(
    bonsai_regr_fit_wtd <-
      fit(
        regr_spec,
        data = mtcars_orsf,
        formula = mpg ~ .,
        case_weights = importance_weights(wts)
      )
  )

  expect_equal(
    bonsai_regr_fit$fit,
    aorsf_regr_fit,
    ignore_formula_env = TRUE
  )

  expect_equal(
    bonsai_regr_fit_wtd$fit,
    aorsf_regr_fit_wtd,
    ignore_formula_env = TRUE
  )
})

test_that("classification model object", {
  skip_if_not_installed("aorsf")

  set.seed(321)
  wts <- sample(0:5, size = nrow(mtcars_orsf), replace = TRUE)

  set.seed(1234)
  aorsf_clsf_fit <-
    aorsf::orsf(
      data = mtcars_orsf,
      formula = vs ~ .,
      n_tree = 10,
      n_thread = 1
    )

  set.seed(1234)
  aorsf_clsf_fit_wtd <- aorsf::orsf_update(aorsf_clsf_fit, weights = wts)

  # formula method
  clsf_spec <- rand_forest(trees = 10) |>
    set_engine("aorsf") |>
    set_mode("classification")

  set.seed(1234)
  expect_no_condition(
    bonsai_clsf_fit <-
      fit(
        clsf_spec,
        data = mtcars_orsf,
        formula = vs ~ .
      )
  )

  set.seed(1234)
  expect_no_condition(
    bonsai_clsf_fit_wtd <-
      fit(
        clsf_spec,
        data = mtcars_orsf,
        formula = vs ~ .,
        case_weights = importance_weights(wts)
      )
  )

  expect_equal(
    bonsai_clsf_fit$fit,
    aorsf_clsf_fit,
    ignore_formula_env = TRUE
  )

  expect_equal(
    bonsai_clsf_fit_wtd$fit,
    aorsf_clsf_fit_wtd,
    ignore_formula_env = TRUE
  )
})

test_that("regression predictions", {
  skip_if_not_installed("aorsf")

  set.seed(1234)
  aorsf_regr_fit <-
    aorsf::orsf(
      data = mtcars_orsf,
      formula = mpg ~ .,
      n_tree = 10
    )

  aorsf_regr_pred <-
    predict(
      aorsf_regr_fit,
      new_data = mtcars_na,
      na_action = 'pass'
    )

  # formula method
  regr_spec <-
    rand_forest(trees = 10) |>
    set_engine("aorsf") |>
    set_mode("regression")

  set.seed(1234)
  bonsai_regr_fit <- fit(regr_spec, mpg ~ ., data = mtcars_orsf)
  bonsai_regr_pred <- predict(bonsai_regr_fit, new_data = mtcars_na)

  expect_s3_class(bonsai_regr_pred, "tbl_df")
  expect_true(all(names(bonsai_regr_pred) == ".pred"))
  expect_equal(bonsai_regr_pred$.pred, as.vector(aorsf_regr_pred))
  expect_equal(nrow(bonsai_regr_pred), nrow(mtcars_orsf))

  # single observation
  pred_1row <- predict(bonsai_regr_fit, mtcars_orsf[2, ])
  expect_identical(nrow(pred_1row), 1L)
})

test_that("classification predictions", {
  skip_if_not_installed("aorsf")

  set.seed(1234)
  aorsf_clsf_fit <-
    aorsf::orsf(
      data = mtcars_orsf,
      formula = vs ~ .,
      n_tree = 10
    )

  aorsf_clsf_pred <-
    predict(
      aorsf_clsf_fit,
      new_data = mtcars_na,
      pred_type = 'prob',
      na_action = 'pass'
    )

  aorsf_probs <- aorsf_clsf_pred

  # see #78--do not expect predictions to align exactly
  aorsf_class <- colnames(aorsf_probs)[apply(
    aorsf_clsf_pred[-1, ],
    1,
    which.max
  )]
  # inserting the NA from first row
  aorsf_class <- c(NA_character_, aorsf_class)

  # formula method
  clsf_spec <- rand_forest(trees = 10) |>
    set_engine("aorsf") |>
    set_mode("classification")

  set.seed(1234)
  bonsai_clsf_fit <- fit(clsf_spec, vs ~ ., data = mtcars_orsf)

  bonsai_clsf_pred_prob <-
    predict(
      bonsai_clsf_fit,
      new_data = mtcars_na,
      type = 'prob'
    )

  bonsai_clsf_pred_class <-
    predict(
      bonsai_clsf_fit,
      new_data = mtcars_na,
      type = 'class'
    )

  expect_s3_class(bonsai_clsf_pred_prob, "tbl_df")
  expect_s3_class(bonsai_clsf_pred_class, "tbl_df")

  expect_true(all(names(bonsai_clsf_pred_prob) == c(".pred_0", ".pred_1")))
  expect_true(all(names(bonsai_clsf_pred_class) == ".pred_class"))

  expect_equal(bonsai_clsf_pred_prob$.pred_0, as.vector(aorsf_probs[, 1]))
  expect_equal(bonsai_clsf_pred_prob$.pred_1, as.vector(aorsf_probs[, 2]))

  expect_equal(bonsai_clsf_pred_class$.pred_class, factor(aorsf_class))

  expect_equal(nrow(bonsai_clsf_pred_prob), nrow(mtcars_orsf))
  expect_equal(nrow(bonsai_clsf_pred_class), nrow(mtcars_orsf))

  # single observation
  pred_1row <- predict(bonsai_clsf_fit, mtcars_orsf[2, ])
  expect_identical(nrow(pred_1row), 1L)
})
