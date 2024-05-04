library(testthat)

mtcars_orsf <- mtcars

mtcars_orsf$vs <- factor(mtcars_orsf$vs)


test_that("regression model object", {

  skip_if_not_installed("aorsf", "0.1.3")

  set.seed(1234)
  aorsf_regr_fit <- aorsf::orsf(
    # everyone's favorite
    data = mtcars_orsf, formula = mpg ~ .,
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
    bonsai_regr_fit <- fit(regr_spec, data = mtcars_orsf, mpg ~ .),
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

  set.seed(1234)
  aorsf_clsf_fit <- aorsf::orsf(
    # everyone's favorite
    data = mtcars_orsf, formula = vs ~ .,
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
    bonsai_clsf_fit <- fit(clsf_spec, data = mtcars_orsf, vs ~ .),
    NA
  )

  expect_equal(
    bonsai_clsf_fit$fit,
    aorsf_clsf_fit,
    ignore_formula_env = TRUE
  )

})

test_that("regression predictions", {

  skip_if_not_installed("aorsf", "0.1.3")

  set.seed(1234)
  aorsf_regr_fit <- aorsf::orsf(
    # everyone's favorite
    data = mtcars_orsf, formula = mpg ~ .,
    # faster
    n_tree = 10
  )

  aorsf_regr_pred <- predict(
    aorsf_regr_fit,
    new_data = mtcars_orsf
  )

  # formula method
  regr_spec <- rand_forest(trees = 10) %>%
    set_engine("aorsf") %>%
    set_mode("regression")

  set.seed(1234)
  bonsai_regr_fit <- fit(regr_spec, mpg ~ ., data = mtcars_orsf)
  bonsai_regr_pred <- predict(bonsai_regr_fit, new_data = mtcars_orsf)

  expect_s3_class(bonsai_regr_pred, "tbl_df")
  expect_true(all(names(bonsai_regr_pred) == ".pred"))
  expect_equal(bonsai_regr_pred$.pred, as.vector(aorsf_regr_pred))
  expect_equal(nrow(bonsai_regr_pred), nrow(mtcars_orsf))

  # single observation
  pred_1row <- predict(bonsai_regr_fit, mtcars_orsf[2,])
  expect_identical(nrow(pred_1row), 1L)

})

test_that("classification predictions", {

  skip_if_not_installed("aorsf", "0.1.3")

  set.seed(1234)
  aorsf_clsf_fit <- aorsf::orsf(
    # everyone's favorite
    data = mtcars_orsf, formula = vs ~ .,
    # faster
    n_tree = 10
  )

  aorsf_clsf_pred <- predict(
    aorsf_clsf_fit,
    new_data = mtcars_orsf,
    pred_type = 'prob'
  )

  aorsf_probs <- aorsf_clsf_pred

  # note: the class predict method in aorsf uses the standard 'each tree
  # gets one vote' approach, which is usually consistent with probability
  # but not all the time. I opted to make predicted probability totally
  # consistent with predicted class in the parsnip bindings for aorsf b/c
  # I think it's really confusing when predicted probs do not align with
  # predicted classes. I'm fine with this in aorsf but in bonsai I want
  # to minimize confusion.
  aorsf_class <- colnames(aorsf_probs)[apply(aorsf_clsf_pred, 1, which.max)]

  # formula method
  clsf_spec <- rand_forest(trees = 10) %>%
    set_engine("aorsf") %>%
    set_mode("classification")

  set.seed(1234)
  bonsai_clsf_fit <- fit(clsf_spec, vs ~ ., data = mtcars_orsf)

  bonsai_clsf_pred_prob <- predict(bonsai_clsf_fit,
                                   new_data = mtcars_orsf,
                                   type = 'prob')

  bonsai_clsf_pred_class <- predict(bonsai_clsf_fit,
                                    new_data = mtcars_orsf,
                                    type = 'class')

  expect_s3_class(bonsai_clsf_pred_prob, "tbl_df")
  expect_s3_class(bonsai_clsf_pred_class, "tbl_df")

  expect_true(all(names(bonsai_clsf_pred_prob) == c(".pred_0", ".pred_1")))
  expect_true(all(names(bonsai_clsf_pred_class) == ".pred_class"))

  expect_equal(bonsai_clsf_pred_prob$.pred_0, as.vector(aorsf_probs[,1]))
  expect_equal(bonsai_clsf_pred_prob$.pred_1, as.vector(aorsf_probs[,2]))

  expect_equal(bonsai_clsf_pred_class$.pred_class, factor(aorsf_class))

  expect_equal(nrow(bonsai_clsf_pred_prob), nrow(mtcars_orsf))
  expect_equal(nrow(bonsai_clsf_pred_class), nrow(mtcars_orsf))

  # single observation
  pred_1row <- predict(bonsai_clsf_fit, mtcars_orsf[2,])
  expect_identical(nrow(pred_1row), 1L)

})

