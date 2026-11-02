#' Boosted trees with catboost
#'
#' `train_catboost` is a wrapper for `catboost` tree-based models
#' where all of the model arguments are in the main function.
#'
#' This is an internal function, not meant to be directly called by the user.
#'
#' @inheritParams train_lightgbm validation
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
#' to 1. Only used with Bayesian bootstrapping; it is only passed to catboost
#' when supplied.
#' @param rsm A numeric value between 0 and 1, random subspace method. The
#' percentage of features to use at each iteration of building trees. At each
#' iteration, features are selected over again at random. Defaults to 1.
#' @param quiet A logical; should logging by catboost::catboost.train() be
#' muted?
#' @param ... Other options to pass to catboost::catboost.train(). Arguments
#' will be correctly routed to the `param` argument, or as a main argument,
#' depending on their name.
#'
#' @details
#' Some catboost parameters are only used for specific values of other
#' parameters. `train_catboost()` checks for these combinations before
#' fitting:
#'
#' - `max_leaves` requires `grow_policy = "Lossguide"` (error).
#' - `min_data_in_leaf` (`min_n`) is ignored when `grow_policy` is
#'   `"SymmetricTree"`, the default (warning).
#' - `subsample` (`sample_size`) cannot be used with Bayesian bootstrapping,
#'   which is the default for multiclass outcomes, or with
#'   `bootstrap_type = "No"` (error).
#' - `bagging_temperature` cannot be used with `bootstrap_type` values of
#'   `"Bernoulli"`, `"Poisson"`, or `"No"` (error) and is ignored with
#'   `"MVS"`, the default for other outcomes (warning).
#' - `one_hot_max_size` has no effect when there are no factor predictors
#'   (warning).
#'
#' When `task_type = "GPU"` and `bootstrap_type` is not set, the default
#' bootstrap type is not inferred and the bootstrap checks are skipped.
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

  check_number_whole(iterations, min = 1, allow_infinite = FALSE, call = call)
  check_number_decimal(
    learning_rate,
    min = 0,
    allow_infinite = FALSE,
    call = call
  )
  check_number_whole(
    depth,
    min = 1,
    max = 16,
    allow_infinite = FALSE,
    call = call
  )
  check_number_decimal(
    l2_leaf_reg,
    min = 0,
    allow_infinite = FALSE,
    call = call
  )
  check_number_decimal(
    random_strength,
    min = 0,
    allow_infinite = FALSE,
    call = call
  )
  check_number_decimal(
    bagging_temperature,
    min = 0,
    allow_infinite = FALSE,
    call = call
  )
  check_number_decimal(
    rsm,
    min = 0,
    max = 1,
    allow_infinite = FALSE,
    call = call
  )
  check_number_decimal(
    validation,
    min = 0,
    max = 0.999,
    allow_infinite = FALSE,
    call = call
  )
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

  arg_params <- check_catboost_constraints(
    arg_params,
    x = x,
    user_bagging_temperature = !missing(bagging_temperature),
    call = call
  )

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

  trn_weights <- NULL
  val_weights <- NULL
  if (!is.null(weights)) {
    trn_weights <- weights[trn_index]
    val_weights <- weights[val_index]
  }

  learn_pool <- rlang::eval_tidy(
    rlang::call2(
      "catboost.load_pool",
      data = x[trn_index, , drop = FALSE],
      label = y[trn_index],
      weight = trn_weights,
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
        weight = val_weights,
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

check_catboost_constraints <- function(
  params,
  x,
  user_bagging_temperature,
  call = rlang::caller_env()
) {
  grow_policy <- params$grow_policy
  if (is.null(grow_policy)) {
    grow_policy <- "SymmetricTree"
  }

  if (!is.null(params$max_leaves) && grow_policy != "Lossguide") {
    cli::cli_abort(
      c(
        "!" = "{.arg max_leaves} can only be used when
               {.code grow_policy = \"Lossguide\"}.",
        "i" = "The grow policy is {.val {grow_policy}}."
      ),
      call = call
    )
  }

  if (!is.null(params$min_data_in_leaf) && grow_policy == "SymmetricTree") {
    cli::cli_warn(
      c(
        "!" = "{.arg min_n} ({.arg min_data_in_leaf} in catboost) is ignored
               when the grow policy is {.val SymmetricTree}.",
        "i" = "Set {.arg grow_policy} to {.val Depthwise} or
               {.val Lossguide} for it to have an effect."
      ),
      call = call
    )
  }

  bootstrap <- catboost_bootstrap_type(params)
  no_bagging <- c("Bernoulli", "Poisson", "No")

  if (!is.null(params$subsample) && bootstrap %in% c("Bayesian", "No")) {
    cli::cli_abort(
      c(
        "!" = "{.arg sample_size} ({.arg subsample} in catboost) cannot be
               used when the bootstrap type is {.val {bootstrap}}.",
        "i" = "Set {.arg bootstrap_type} to {.val MVS} or {.val Bernoulli}
               to subsample the data."
      ),
      call = call
    )
  }

  # catboost's default bagging_temperature is 1, so it only needs to be passed
  # when supplied; passing it errors for some bootstrap types.
  if (!user_bagging_temperature) {
    params$bagging_temperature <- NULL
  } else if (bootstrap %in% no_bagging) {
    cli::cli_abort(
      c(
        "!" = "{.arg bagging_temperature} cannot be used when the bootstrap
               type is {.val {bootstrap}}.",
        "i" = "It is only used with {.code bootstrap_type = \"Bayesian\"}."
      ),
      call = call
    )
  } else if (identical(bootstrap, "MVS")) {
    cli::cli_warn(
      c(
        "!" = "{.arg bagging_temperature} is ignored when the bootstrap type
               is {.val MVS}.",
        "i" = "It is only used with {.code bootstrap_type = \"Bayesian\"}."
      ),
      call = call
    )
  }

  if (!is.null(params$one_hot_max_size)) {
    has_factors <- any(vapply(x, is.factor, logical(1)))
    if (!has_factors) {
      cli::cli_warn(
        c(
          "!" = "{.arg one_hot_max_size} has no effect since there are no
                 factor predictors."
        ),
        call = call
      )
    }
  }

  params
}

# Mirrors catboost's CPU defaults; returns NA when the default can't be
# determined (GPU training).
catboost_bootstrap_type <- function(params) {
  if (!is.null(params$bootstrap_type)) {
    return(params$bootstrap_type)
  }
  if (identical(params$task_type, "GPU")) {
    return(NA_character_)
  }

  loss <- params$loss_function
  if (is.null(loss)) {
    loss <- params$objective
  }
  if (is.character(loss) && startsWith(loss, "MultiClass")) {
    return("Bayesian")
  }
  "MVS"
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
