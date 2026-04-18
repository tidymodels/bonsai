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
#' @param validation The _proportion_ of the training data that are used for
#' performance assessment and potential early stopping.
#' @param quiet A logical; should logging by catboost::catboost.train() be
#' muted?
#' @param ... Other options to pass to catboost::catboost.train(). Arguments
#' will be correctly routed to the `param` argument, or as a main argument,
#' depending on their name.
#'
#' @source https://catboost.ai/docs/en/references/training-parameters/.
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
  validation = 0,
  quiet = TRUE,
  ...
) {
  force(x)
  force(y)

  missing_validation <- missing(validation)

  call <- call2("fit")

  check_number_whole(iterations, call = call)
  check_number_decimal(learning_rate, call = call)
  check_number_whole(depth, call = call)
  check_number_decimal(l2_leaf_reg, call = call)
  check_number_decimal(random_strength, call = call)
  check_number_decimal(bagging_temperature, call = call)
  check_number_decimal(rsm, call = call)
  check_number_decimal(validation, call = call)
  check_bool(quiet, call = call)

  check_catboost_aliases(...)

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

  arg_params <- process_loss_function(arg_params, y)

  if (!is.null(arg_params$params) && is.list(arg_params$params)) {
    cli::cli_warn(c(
      "Arguments passed in through {.arg params} as a list will be ignored.",
      "Instead pass the arguments directly to the {.code ...}."
    ))
    arg_params$params <- NULL
  }

  n <- nrow(x)
  needs_validation <- !is.null(arg_params$early_stopping_rounds)

  if (!needs_validation) {
    trn_index <- seq_len(n)
    val_index <- NULL
  } else if (missing_validation) {
    trn_index <- seq_len(n)
    val_index <- trn_index
  } else {
    m <- min(floor(n * (1 - validation)) + 1, n - 1)
    trn_index <- sample(seq_len(n), size = max(m, 2))
    val_index <- setdiff(seq_len(n), trn_index)
  }

  learn_pool <- rlang::eval_tidy(
    rlang::call2(
      "catboost.load_pool",
      data = x[trn_index, , drop = FALSE],
      label = y[trn_index],
      weight = if (is.null(weights)) NULL else weights[trn_index],
      .ns = "catboost"
    ),
    env = rlang::current_env()
  )

  test_pool <- NULL
  if (!is.null(val_index)) {
    test_pool <- rlang::eval_tidy(
      rlang::call2(
        "catboost.load_pool",
        data = x[val_index, , drop = FALSE],
        label = y[val_index],
        weight = if (is.null(weights)) NULL else weights[val_index],
        .ns = "catboost"
      ),
      env = rlang::current_env()
    )
  }

  args <- list(
    learn_pool = learn_pool,
    test_pool = test_pool,
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
    !!!list(...),
    .ns = "catboost"
  ))
  p
}

#' @keywords internal
#' @export
#' @rdname catboost_helpers
predict_catboost_classification_class <- function(object, new_data, ...) {
  pool <- rlang::eval_tidy(rlang::call2(
    "catboost.load_pool",
    data = new_data,
    .ns = "catboost"
  ))

  p <- rlang::eval_tidy(rlang::call2(
    "catboost.predict",
    model = object$fit,
    pool = pool,
    prediction_type = "Class",
    !!!list(...),
    .ns = "catboost"
  ))

  object$lvl[p + 1]
}

#' @keywords internal
#' @export
#' @rdname catboost_helpers
predict_catboost_classification_prob <- function(object, new_data, ...) {
  pool <- rlang::eval_tidy(rlang::call2(
    "catboost.load_pool",
    data = new_data,
    .ns = "catboost"
  ))

  p <- rlang::eval_tidy(rlang::call2(
    "catboost.predict",
    model = object$fit,
    pool = pool,
    prediction_type = "Probability",
    !!!list(...),
    .ns = "catboost"
  ))

  if (is.vector(p)) {
    p <- tibble::tibble(p1 = 1 - p, p2 = p)
  }

  colnames(p) <- object$lvl

  tibble::as_tibble(p)
}

#' @keywords internal
#' @export
#' @rdname catboost_helpers
predict_catboost_classification_raw <- function(object, new_data, ...) {
  pool <- rlang::eval_tidy(rlang::call2(
    "catboost.load_pool",
    data = new_data,
    .ns = "catboost"
  ))

  p <- rlang::eval_tidy(rlang::call2(
    "catboost.predict",
    model = object$fit,
    pool = pool,
    !!!list(...),
    .ns = "catboost"
  ))
  p
}

# https://catboost.ai/docs/en/concepts/loss-functions
process_loss_function <- function(args, y) {
  lvl <- levels(y)
  lvls <- length(lvl)
  # set the "loss_function" param argument, clear it out from main args
  if (!any(names(args) %in% c("loss_function", "objective"))) {
    if (is.numeric(y)) {
      args$loss_function <- "RMSE"
    } else {
      if (lvls == 2) {
        args$loss_function <- "Logloss"
      } else {
        args$loss_function <- "MultiClass"
      }
    }
  }

  args
}

#' @keywords internal
#' @export
#' @rdname catboost_helpers
`multi_predict._catboost.Model` <- function(
  object,
  new_data,
  type = NULL,
  trees = NULL,
  ...
) {
  if (any(names(rlang::enquos(...)) == "newdata")) {
    cli::cli_abort(
      "Did you mean to use {.code new_data} instead of {.code newdata}?"
    )
  }

  trees <- sort(trees)

  res <- purrr::map_df(
    trees,
    catboost_by_tree,
    object = object,
    new_data = new_data,
    type = type
  )
  res <- dplyr::arrange(res, .row, trees)
  res <- split(res[, -1], res$.row)
  names(res) <- NULL

  tibble::tibble(.pred = res)
}

check_catboost_aliases <- function(...) {
  dots <- rlang::list2(...)

  for (param in names(dots)) {
    uses_alias <- catboost_aliases$alias %in% param
    if (any(uses_alias)) {
      main <- catboost_aliases$catboost[uses_alias]
      parsnip_arg <- catboost_aliases$parsnip[uses_alias]
      cli::cli_abort(
        c(
          "!" = "The {.var {param}} argument passed to \\
             {.help [`set_engine()`](parsnip::set_engine)} is an alias for \\
             a main model argument.",
          "i" = "Please instead pass this argument via the {.var {parsnip_arg}} \\
             argument to {.help [`boost_tree()`](parsnip::boost_tree)}."
        ),
        call = rlang::call2("fit")
      )
    }
  }

  invisible(TRUE)
}

catboost_aliases <- tibble::tribble(
  ~parsnip,      ~catboost,               ~alias,
  "trees",       "iterations",            "n_estimators",
  "trees",       "iterations",            "num_boost_round",
  "trees",       "iterations",            "num_trees",
  "trees",       "iterations",            "num_round",
  "trees",       "iterations",            "n_iter",
  "learn_rate",  "learning_rate",         "eta",
  "tree_depth",  "depth",                 "max_depth",
  "mtry",        "rsm",                   "colsample_bylevel",
  "min_n",       "min_data_in_leaf",      "min_child_samples",
  "min_n",       "min_data_in_leaf",      "min_samples_leaf",
  "sample_size", "subsample",             "bagging_fraction",
  "stop_iter",   "early_stopping_rounds", "early_stopping",
  "stop_iter",   "early_stopping_rounds", "od_wait"
)

catboost_by_tree <- function(tree, object, new_data, type = NULL) {
  # switch based on prediction type
  if (object$spec$mode == "regression") {
    pred <- predict_catboost_regression_numeric(
      object,
      new_data,
      ntree_end = tree
    )

    pred <- tibble::tibble(.pred = pred)

    nms <- names(pred)
  } else {
    if (is.null(type) || type == "class") {
      pred <- predict_catboost_classification_class(
        object,
        new_data,
        ntree_end = tree
      )

      pred <- tibble::tibble(.pred_class = factor(pred, levels = object$lvl))
    } else {
      pred <- predict_catboost_classification_prob(
        object,
        new_data,
        ntree_end = tree
      )

      names(pred) <- paste0(".pred_", names(pred))
    }

    nms <- names(pred)
  }

  pred[["trees"]] <- tree
  pred[[".row"]] <- 1:nrow(new_data)
  pred[, c(".row", "trees", nms)]
}
