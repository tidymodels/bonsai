#' Boosted trees with lightgbm
#'
#' `train_lightgbm` is a wrapper for `lightgbm` tree-based models
#' where all of the model arguments are in the main function.
#'
#' This is an internal function, not meant to be directly called by the user.
#'
#' @param x A data frame or matrix of predictors
#' @param y A vector (factor or numeric) or matrix (numeric) of outcome data.
#' @param weights A numeric vector of sample weights.
#' @param max_depth An integer for the maximum depth of the tree.
#' @param num_iterations An integer for the number of boosting iterations.
#' @param learning_rate A numeric value between zero and one to control the learning rate.
#' @param feature_fraction_bynode Fraction of predictors that will be randomly sampled
#' at each split.
#' @param min_data_in_leaf A numeric value for the minimum sum of instances needed
#'  in a child to continue to split.
#' @param min_gain_to_split A number for the minimum loss reduction required to make a
#'  further partition on a leaf node of the tree.
#' @param bagging_fraction Subsampling proportion of rows. Setting this argument
#'  to a non-default value will also set `bagging_freq = 1`. See the Bagging
#'  section in `?details_boost_tree_lightgbm` for more details.
#' @param early_stopping_round Number of iterations without an improvement in
#' the objective function occur before training should be halted.
#' @param validation The _proportion_ of the training data that are used for
#' performance assessment and potential early stopping.
#' @param counts A logical; should `feature_fraction_bynode` be interpreted as the
#' _number_ of predictors that will be randomly sampled at each split?
#' `TRUE` indicates that `mtry` will be interpreted in its sense as a _count_,
#' `FALSE` indicates that the argument will be interpreted in its sense as a
#' _proportion_.
#' @param quiet A logical; should logging by [lightgbm::lgb.train()] be muted?
#' @param ... Other options to pass to [lightgbm::lgb.train()]. Arguments
#' will be correctly routed to the `param` argument, or as a main argument,
#' depending on their name.
#' @return A fitted `lightgbm.Model` object.
#' @keywords internal
#' @export
train_lightgbm <- function(
  x,
  y,
  weights = NULL,
  max_depth = -1,
  num_iterations = 100,
  learning_rate = 0.1,
  feature_fraction_bynode = 1,
  min_data_in_leaf = 20,
  min_gain_to_split = 0,
  bagging_fraction = 1,
  early_stopping_round = NULL,
  validation = 0,
  counts = TRUE,
  quiet = FALSE,
  ...
) {
  force(x)
  force(y)

  call <- call2("fit")

  check_number_whole(max_depth, call = call)
  check_number_whole(num_iterations, call = call)
  check_number_decimal(learning_rate, call = call)
  check_number_decimal(feature_fraction_bynode, call = call)
  check_number_whole(min_data_in_leaf, call = call)
  check_number_decimal(min_gain_to_split, call = call)
  check_number_decimal(bagging_fraction, call = call)
  check_number_decimal(early_stopping_round, allow_null = TRUE, call = call)
  check_bool(counts, call = call)
  check_bool(quiet, call = call)

  feature_fraction_bynode <-
    process_mtry(
      feature_fraction_bynode = feature_fraction_bynode,
      counts = counts,
      x = x,
      is_missing = missing(feature_fraction_bynode)
    )

  check_lightgbm_aliases(...)

  # bonsai should be able to differentiate between
  # 1) main arguments to `lgb.train()` (as in `names(formals(lgb.train))` other
  #    than `params`),
  # 2) main arguments to `lgb.Dataset()` (as in `names(formals(lgb.Dataset))`
  #    other than `params`), and
  # 3) arguments to pass to `lgb.train(params)` OR `lgb.Dataset(params)`.
  #    arguments to the `params` argument of either function can be concatenated
  #    together and passed to both (#77).
  args <-
    list(
      num_iterations = num_iterations,
      learning_rate = learning_rate,
      max_depth = max_depth,
      feature_fraction_bynode = feature_fraction_bynode,
      min_data_in_leaf = min_data_in_leaf,
      min_gain_to_split = min_gain_to_split,
      bagging_fraction = bagging_fraction,
      early_stopping_round = early_stopping_round,
      ...
    )

  if (!is.null(args$params) && is.list(args$params)) {
    cli::cli_warn(c(
      "Arguments passed in through {.arg params} as a list will be ignored.",
      "Instead pass the arguments directly to the {.code ...}."
    ))
  }

  args <- process_bagging(args)
  args <- process_objective_function(args, y)

  args <- sort_args(args)

  if (!is.numeric(y)) {
    y <- as.numeric(y) - 1
  }

  args <- process_data(args, x, y, weights, validation, missing(validation))

  compacted <- c(list(params = args$params), args$main_args_train)

  call <- rlang::call2("lgb.train", !!!compacted, .ns = "lightgbm")

  if (quiet) {
    junk <- utils::capture.output(
      res <- rlang::eval_tidy(call, env = rlang::current_env())
    )
  } else {
    res <- rlang::eval_tidy(call, env = rlang::current_env())
  }

  res
}

process_mtry <- function(
  feature_fraction_bynode,
  counts,
  x,
  is_missing,
  call = call2("fit")
) {
  check_bool(counts, call = call)

  ineq <- if (counts) {
    "greater"
  } else {
    "less"
  }
  interp <- if (counts) {
    "count"
  } else {
    "proportion"
  }
  opp <- if (!counts) {
    "count"
  } else {
    "proportion"
  }

  if (
    (feature_fraction_bynode < 1 & counts) |
      (feature_fraction_bynode > 1 & !counts)
  ) {
    cli::cli_abort(
      c(
        "{.arg mtry} must be {ineq} than or equal to 1, not {feature_fraction_bynode}.",
        "i" = "{.arg mtry} is currently being interpreted as a {interp}
               rather than a {opp}.",
        "i" = "Supply {.code counts = {!counts}} to {.fn set_engine} to supply
               this argument as a {opp} rather than a {interp}.",
        "i" = "See {.help train_lightgbm} for more details."
      ),
      call = call
    )
  }

  if (counts && !is_missing) {
    feature_fraction_bynode <- feature_fraction_bynode / ncol(x)
  }

  feature_fraction_bynode
}

# https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters
process_objective_function <- function(args, y) {
  lvl <- levels(y)
  lvls <- length(lvl)
  # set the "objective" param argument, clear it out from main args
  if (!any(names(args) %in% c("objective"))) {
    if (is.numeric(y)) {
      args$objective <- "regression"
    } else {
      if (lvls == 2) {
        args$objective <- "binary"
      } else {
        args$objective <- "multiclass"
      }
    }
  }

  if (args$objective == "binary" && is.null(args$num_class)) {
    args$num_class <- 1L
  }

  multiclass_obj <- c(
    "multiclass",
    "softmax",
    "multiclassova",
    "multiclass_ova",
    "ova",
    "ovr"
  )

  if (args$objective %in% multiclass_obj && is.null(args$num_class)) {
    args$num_class <- lvls
  }

  args
}

process_bagging <- function(args) {
  if (
    args$bagging_fraction != 1 &&
      (!"bagging_freq" %in% names(args))
  ) {
    args$bagging_freq <- 1
  }

  args
}

process_data <- function(args, x, y, weights, validation, missing_validation) {
  #                                           trn_index       | val_index
  #                                         ----------------------------------
  #  needs_validation &  missing_validation | 1:n               1:n
  #  needs_validation & !missing_validation | sample(1:n, m)    setdiff(trn_index, 1:n)
  # !needs_validation &  missing_validation | 1:n               NULL
  # !needs_validation & !missing_validation | sample(1:n, m)    setdiff(trn_index, 1:n)

  n <- nrow(x)
  needs_validation <- !is.null(args$params$early_stopping_round)
  if (!needs_validation) {
    # If early_stopping_round isn't set, clear it from arguments actually
    # passed to LightGBM.
    args$params$early_stopping_round <- NULL
  }

  if (missing_validation) {
    trn_index <- 1:n
    if (needs_validation) {
      val_index <- trn_index
    } else {
      val_index <- NULL
    }
  } else {
    m <- min(floor(n * (1 - validation)) + 1, n - 1)
    trn_index <- sample(1:n, size = max(m, 2))
    val_index <- setdiff(1:n, trn_index)
  }

  data_args <-
    c(
      list(
        data = prepare_df_lgbm(x[trn_index, , drop = FALSE]),
        label = y[trn_index],
        categorical_feature = categorical_columns(x[trn_index, , drop = FALSE]),
        params = c(list(feature_pre_filter = FALSE), args$params),
        weight = weights[trn_index]
      ),
      args$main_args_dataset
    )

  args$main_args_train$data <-
    rlang::eval_bare(
      rlang::call2("lgb.Dataset", !!!data_args, .ns = "lightgbm")
    )

  if (!is.null(val_index)) {
    valids_args <-
      c(
        list(
          data = prepare_df_lgbm(x[val_index, , drop = FALSE]),
          label = y[val_index],
          categorical_feature = categorical_columns(x[
            val_index,
            ,
            drop = FALSE
          ]),
          params = list(feature_pre_filter = FALSE, args$params),
          weight = weights[val_index]
        ),
        args$main_args_dataset
      )

    args$main_args_train$valids <-
      list(
        validation = rlang::eval_bare(
          rlang::call2("lgb.Dataset", !!!valids_args, .ns = "lightgbm")
        )
      )
  }

  args
}

# identifies supplied arguments as destined for `lgb.Dataset()`, `lgb.train()`,
# or the `params` argument to both of the above (#77).
sort_args <- function(args) {
  # warn on arguments that won't be passed along
  protected <- c(
    "obj",
    "init_model",
    "colnames",
    "categorical_feature",
    "callbacks",
    "reset_data"
  )

  if (any(names(args) %in% protected)) {
    protected_args <- names(args[names(args) %in% protected])

    cli::cli_warn(
      "The following argument{?s} {?is/are} guarded by bonsai and will not be 
      passed to LightGBM: {protected_args}."
    )

    args[protected_args] <- NULL
  }

  main_args_dataset <- main_args(lightgbm::lgb.Dataset)
  main_args_train <- main_args(lightgbm::lgb.train)

  args <-
    list(
      main_args_dataset = args[names(args) %in% main_args_dataset],
      main_args_train = args[names(args) %in% main_args_train],
      params = args[!names(args) %in% c(main_args_dataset, main_args_train)]
    )

  args
}

main_args <- function(fn) {
  res <- names(formals(fn))
  res[res != "params"]
}

# in lightgbm <= 3.3.2, predict() for multiclass classification produced a single
# vector of length num_observations * num_classes, in row-major order
#
# in versions after that release, lightgbm produces a numeric matrix with shape
# [num_observations, num_classes]
#
# this function ensures that multiclass classification predictions are always
# returned as a [num_observations, num_classes] matrix, regardless of lightgbm version
reshape_lightgbm_multiclass_preds <- function(preds, num_rows) {
  n_preds_per_case <- length(preds) / num_rows
  if (is.vector(preds) && n_preds_per_case > 1) {
    preds <- matrix(preds, ncol = n_preds_per_case, byrow = TRUE)
  }
  preds
}

#' Internal functions
#'
#' Not intended for direct use.
#'
#' @keywords internal
#' @export
#' @rdname lightgbm_helpers
predict_lightgbm_classification_prob <- function(object, new_data, ...) {
  p <- stats::predict(object$fit, prepare_df_lgbm(new_data), ...)
  p <- reshape_lightgbm_multiclass_preds(preds = p, num_rows = nrow(new_data))

  if (is.vector(p)) {
    p <- tibble::tibble(p1 = 1 - p, p2 = p)
  }

  colnames(p) <- object$lvl

  tibble::as_tibble(p)
}

#' @keywords internal
#' @export
#' @rdname lightgbm_helpers
predict_lightgbm_classification_class <- function(object, new_data, ...) {
  p <- predict_lightgbm_classification_prob(
    object,
    prepare_df_lgbm(new_data),
    ...
  )

  q <- apply(p, 1, function(x) which.max(x))

  names(p)[q]
}

#' @keywords internal
#' @export
#' @rdname lightgbm_helpers
predict_lightgbm_classification_raw <- function(object, new_data, ...) {
  if (using_newer_lightgbm_version()) {
    p <- stats::predict(
      object$fit,
      prepare_df_lgbm(new_data),
      type = "raw",
      ...
    )
  } else {
    p <- stats::predict(
      object$fit,
      prepare_df_lgbm(new_data),
      rawscore = TRUE,
      ...
    )
  }
  reshape_lightgbm_multiclass_preds(preds = p, num_rows = nrow(new_data))
}

#' @keywords internal
#' @export
#' @rdname lightgbm_helpers
predict_lightgbm_regression_numeric <- function(object, new_data, ...) {
  p <-
    stats::predict(
      object$fit,
      prepare_df_lgbm(new_data),
      params = list(predict_disable_shape_check = TRUE),
      ...
    )
  p
}


#' @keywords internal
#' @export
#' @rdname lightgbm_helpers
multi_predict._lgb.Booster <- function(
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

  res <- map_df(
    trees,
    lightgbm_by_tree,
    object = object,
    new_data = new_data,
    type = type
  )
  res <- dplyr::arrange(res, .row, trees)
  res <- split(res[, -1], res$.row)
  names(res) <- NULL

  tibble::tibble(.pred = res)
}

lightgbm_by_tree <- function(tree, object, new_data, type = NULL) {
  # switch based on prediction type
  if (object$spec$mode == "regression") {
    pred <- predict_lightgbm_regression_numeric(
      object,
      new_data,
      num_iteration = tree
    )

    pred <- tibble::tibble(.pred = pred)

    nms <- names(pred)
  } else {
    if (is.null(type) || type == "class") {
      pred <- predict_lightgbm_classification_class(
        object,
        new_data,
        num_iteration = tree
      )

      pred <- tibble::tibble(.pred_class = factor(pred, levels = object$lvl))
    } else {
      pred <- predict_lightgbm_classification_prob(
        object,
        new_data,
        num_iteration = tree
      )

      names(pred) <- paste0(".pred_", names(pred))
    }

    nms <- names(pred)
  }

  pred[["trees"]] <- tree
  pred[[".row"]] <- 1:nrow(new_data)
  pred[, c(".row", "trees", nms)]
}

prepare_df_lgbm <- function(x, y = NULL) {
  categorical_cols <- categorical_columns(x)

  x <- categorical_features_to_int(x, categorical_cols)

  x <- parsnip::maybe_matrix(x)

  return(x)
}

categorical_columns <- function(x) {
  categorical_cols <- NULL
  if (inherits(x, c("matrix", "Matrix"))) {
    return(categorical_cols)
  }
  for (i in seq_along(x)) {
    if (is.factor(x[[i]])) {
      categorical_cols <- c(categorical_cols, i)
    }
  }
  categorical_cols
}

categorical_features_to_int <- function(x, cat_indices) {
  if (inherits(x, c("matrix", "Matrix"))) {
    return(x)
  }
  for (i in cat_indices) {
    x[[i]] <- as.integer(x[[i]]) - 1
  }
  x
}

check_lightgbm_aliases <- function(...) {
  dots <- rlang::list2(...)

  for (param in names(dots)) {
    uses_alias <- lightgbm_aliases$alias %in% param
    if (any(uses_alias)) {
      main <- lightgbm_aliases$lightgbm[uses_alias]
      parsnip <- lightgbm_aliases$parsnip[uses_alias]
      cli::cli_abort(
        c(
          "!" = "The {.var {param}} argument passed to \\
             {.help [`set_engine()`](parsnip::set_engine)} is an alias for \\
             a main model argument.",
          "i" = "Please instead pass this argument via the {.var {parsnip}} \\
             argument to {.help [`boost_tree()`](parsnip::boost_tree)}."
        ),
        call = rlang::call2("fit")
      )
    }
  }

  invisible(TRUE)
}

lightgbm_aliases <- tibble::tribble(
  ~parsnip,         ~lightgbm,                 ~alias,
  # note that "tree_depth" -> "max_depth" has no aliases
  "trees",          "num_iterations",          "num_iteration",
  "trees",          "num_iterations",          "n_iter",
  "trees",          "num_iterations",          "num_tree",
  "trees",          "num_iterations",          "num_trees",
  "trees",          "num_iterations",          "num_round",
  "trees",          "num_iterations",          "num_rounds",
  "trees",          "num_iterations",          "nrounds",
  "trees",          "num_iterations",          "num_boost_round",
  "trees",          "num_iterations",          "n_estimators",
  "trees",          "num_iterations",          "max_iter",
  "learn_rate",     "learning_rate",           "shrinkage_rate",
  "learn_rate",     "learning_rate",           "eta",
  "mtry",           "feature_fraction_bynode", "sub_feature_bynode",
  "mtry",           "feature_fraction_bynode", "colsample_bynode",
  "min_n",          "min_data_in_leaf",        "min_data_per_leaf",
  "min_n",          "min_data_in_leaf",        "min_data",
  "min_n",          "min_data_in_leaf",        "min_child_samples",
  "min_n",          "min_data_in_leaf",        "min_samples_leaf",
  "loss_reduction", "min_gain_to_split",       "min_split_gain",
  "sample_size",    "bagging_fraction",        "sub_row",
  "sample_size",    "bagging_fraction",        "subsample",
  "sample_size",    "bagging_fraction",        "bagging",
  "stop_iter",      "early_stopping_round",    "early_stopping_rounds",
  "stop_iter",      "early_stopping_round",    "early_stopping",
  "stop_iter",      "early_stopping_round",    "n_iter_no_change"
)
