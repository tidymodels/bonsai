#' Boosted trees with catboost
#'
#' `train_catboost` is a wrapper for `catboost` tree-based models
#' where all of the model arguments are in the main function.
#'
#' This is an internal function, not meant to be directly called by the user.
#'
#' @param x A data frame of predictors.
#' @param y A vector (factor or numeric) or matrix (numeric) of outcome data.
#' @param weights A numeric vector of sample weights, defaults to `NULL`.
#' @param iterations The maximum number of trees that can be built when solving
#' machine learning problems. Default to 1000.
#' @param learning_rate A positive numeric value for the learning rate. Defaults
#' to 0.03.
#' @param depth An integer for the depth of the trees. Default to 6.
#' @param l2_leaf_reg A numeric value for the L2 regularization coefficient.
#' Used for leaf value calculation. Defaults to 3.
#' @param random_strength The amount of randomness to use for scoring splits
#' when the tree structure is selected. Use this parameter to avoid overfitting
#' the model. Defaults to 1.
#' @param bagging_temperature A numeric value, controls intensity of Bayesian
#' bagging. The higher the temperature the more aggressive bagging is. Defaults
#' to 1.
#' @param rsm A numeric value between 0 and 1, random subspace method. The
#' percentage of features to use at each iteration of building trees. At each
#' iteration, features are selected over again at random. Defaults to 1.
#' @param quiet A logical; should logging by [catboost::catboost.train()] be
#' muted?
#' @param ... Other options to pass to [catboost::catboost.train()]. Arguments
#' will be correctly routed to the `param` argument, or as a main argument,
#' depending on their name.
#'
#' @source \url{https://catboost.ai/docs/en/references/training-parameters/}.
#'
#' @return A fitted `catboost.Model` object.
#' @keywords internal
#' @export
train_catboost <- function(
  x,
  y,
  weights = NULL,
  iterations = 1000,
  learning_rate = 0.03,
  depth = 6,
  l2_leaf_reg = 3,
  random_strength = 1,
  bagging_temperature = 1,
  rsm = 1,
  quiet = TRUE,
  ...
) {
  force(x)
  force(y)

  call <- call2("fit")

  check_number_whole(iterations, call = call)
  check_number_decimal(learning_rate, call = call)
  check_number_whole(depth, call = call)
  check_number_decimal(l2_leaf_reg, call = call)
  check_number_decimal(random_strength, call = call)
  check_number_decimal(bagging_temperature, call = call)
  check_number_decimal(rsm, call = call)
  check_bool(quiet, call = call)

  arg_params <- list(
    iterations = iterations,
    learning_rate = learning_rate,
    depth = depth,
    l2_leaf_reg = l2_leaf_reg,
    random_strength = random_strength,
    bagging_temperature = bagging_temperature,
    rsm = rsm,
    ...
  )

  if (!is.null(arg_params$params) && is.list(arg_params$params)) {
    cli::cli_warn(c(
      "Arguments passed in through {.arg params} as a list will be ignored.",
      "Instead pass the arguments directly to the {.code ...}."
    ))
  }

  learn_pool <- rlang::call2(
    "catboost.load_pool",
    data = x,
    label = y,
    weight = weights,
    .ns = "catboost"
  )

  args <- list(
    learn_pool = learn_pool,
    params = arg_params
  )

  call <- rlang::call2("catboost.train", !!!args, .ns = "catboost")

  if (quiet) {
    junk <- utils::capture.output(
      res <- rlang::eval_tidy(call, env = rlang::current_env())
    )
  } else {
    res <- rlang::eval_tidy(call, env = rlang::current_env())
  }

  res
}

#' Internal functions
#'
#' Not intended for direct use.
#'
#' @keywords internal
#' @export
#' @rdname catboost_helpers
predict_catboost_regression_numeric <- function(object, new_data, ...) {
  pool <- rlang::eval_tidy(rlang::call2(
    "catboost.load_pool",
    data = new_data,
    .ns = "catboost"
  ))

  p <- rlang::eval_tidy(rlang::call2(
    "catboost.predict",
    model = object$fit,
    pool = pool,
    .ns = "catboost"
  ))
  p
}
