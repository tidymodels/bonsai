#' Boosted trees with lightgbm
#'
#' `train_lightgbm` is a wrapper for `lightgbm` tree-based models
#' where all of the model arguments are in the main function.
#'
#' This is an internal function, not meant to be directly called by the user.
#'
#' @param x A data frame or matrix of predictors
#' @param y A vector (factor or numeric) or matrix (numeric) of outcome data.
#' @param max_depth An integer for the maximum depth of the tree.
#' @param num_iterations An integer for the number of boosting iterations.
#' @param learning_rate A numeric value between zero and one to control the learning rate.
#' @param feature_fraction Subsampling proportion of columns.
#' @param min_data_in_leaf A numeric value for the minimum sum of instances needed
#'  in a child to continue to split.
#' @param min_gain_to_split A number for the minimum loss reduction required to make a
#'  further partition on a leaf node of the tree.
#' @param bagging_fraction Subsampling proportion of rows.
#' @param quiet A logical; should logging by [lightgbm::lgb.train()] be muted?
#' @param ... Other options to pass to [lightgbm::lgb.train()].
#' @return A fitted `lightgbm.Model` object.
#' @keywords internal
#' @export
train_lightgbm <- function(x, y, max_depth = -1, num_iterations = 100, learning_rate = 0.1,
                           feature_fraction = 1, min_data_in_leaf = 20,
                           min_gain_to_split = 0, bagging_fraction = 1,
                           quiet = FALSE, ...) {

  force(x)
  force(y)
  others <- list(...)
  if (!is.logical(quiet)) {
    rlang::abort("'quiet' should be a logical value.")
  }

  if (!any(names(others) %in% c("objective"))) {
    if (is.numeric(y)) {
      #others$num_class <- 1
      others$objective <- "regression"
    } else {
      lvl <- levels(y)
      lvls <- length(lvl)
      y <- as.numeric(y) - 1
      if (lvls == 2) {
        others$num_class <- 1
        others$objective <- "binary"
      } else {
        others$num_class <- lvls
        others$objective <- "multiclass"
      }
    }
  }

  arg_list <- list(
    num_iterations = num_iterations,
    learning_rate = learning_rate,
    max_depth = max_depth,
    feature_fraction = feature_fraction,
    min_data_in_leaf = min_data_in_leaf,
    min_gain_to_split = min_gain_to_split,
    bagging_fraction = bagging_fraction
  )

  others <- others[!(names(others) %in% c("data", names(arg_list)))]

  # parallelism should be explicitly specified by the user
  if(all(sapply(others[c("num_threads", "num_thread", "nthread", "nthreads", "n_jobs")], is.null))) others$num_threads <- 1L

  arg_list <- purrr::compact(c(arg_list, others))

  if ("verbose" %in% names(arg_list)) {
    verbose <- arg_list$verbose
    arg_list[["verbose"]] <- NULL
  } else {
    verbose <- 1L
  }

  d <- lightgbm::lgb.Dataset(
    data = prepare_df_lgbm(x),
    label = y,
    categorical_feature = categorical_columns(x),
    params = list(feature_pre_filter = FALSE)
  )

  main_args <- list(
    data = quote(d),
    params = arg_list,
    verbose = verbose
  )

  call <- parsnip::make_call(fun = "lgb.train", ns = "lightgbm", main_args)

  if (quiet) {
    junk <- utils::capture.output(res <- rlang::eval_tidy(call, env = rlang::current_env()))
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
#' @rdname lightgbm_helpers
predict_lightgbm_classification_prob <- function(object, new_data, ...) {
  p <- stats::predict(object$fit, prepare_df_lgbm(new_data), reshape = TRUE, ...)

  if(is.vector(p)) {
    p <- tibble::tibble(p1 = 1 - p, p2 = p)
  }

  colnames(p) <- object$lvl

  tibble::as_tibble(p)
}

#' @keywords internal
#' @export
#' @rdname lightgbm_helpers
predict_lightgbm_classification_class <- function(object, new_data, ...) {
  p <- predict_lightgbm_classification_prob(object, prepare_df_lgbm(new_data), ...)

  q <- apply(p, 1, function(x) which.max(x))

  names(p)[q]
}

#' @keywords internal
#' @export
#' @rdname lightgbm_helpers
predict_lightgbm_classification_raw <- function(object, new_data, ...) {
  stats::predict(object$fit, prepare_df_lgbm(new_data), reshape = TRUE, rawscore = TRUE, ...)
}

#' @keywords internal
#' @export
#' @rdname lightgbm_helpers
predict_lightgbm_regression_numeric <- function(object, new_data, ...) {
  p <-
    stats::predict(
      object$fit,
      prepare_df_lgbm(new_data),
      reshape = TRUE,
      params = list(predict_disable_shape_check = TRUE),
      ...
    )
  p
}



#' @keywords internal
#' @export
#' @rdname lightgbm_helpers
multi_predict._lgb.Booster <- function(object, new_data, type = NULL, trees = NULL, ...) {
  if (any(names(rlang::enquos(...)) == "newdata")) {
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
  }

  trees <- sort(trees)

  res <- map_df(trees, lightgbm_by_tree,
                object = object, new_data = new_data, type = type)
  res <- dplyr::arrange(res, .row, trees)
  res <- split(res[, -1], res$.row)
  names(res) <- NULL

  tibble::tibble(.pred = res)

}

lightgbm_by_tree <- function(tree, object, new_data, type = NULL) {
  # switch based on prediction type
  if (object$spec$mode == "regression") {
    pred <- predict_lightgbm_regression_numeric(object, new_data, num_iteration = tree)

    pred <- tibble::tibble(.pred = pred)

    nms <- names(pred)
  } else {
    if (type == "class") {
      pred <- predict_lightgbm_classification_class(object, new_data, num_iteration = tree)

      pred <- tibble::tibble(.pred_class = factor(pred, levels = object$lvl))
    } else {
      pred <- predict_lightgbm_classification_prob(object, new_data, num_iteration = tree)

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

  x <- as.matrix(x)

  return(x)
}

categorical_columns <- function(x){
  categorical_cols <- NULL
  for (i in seq_along(x)) {
    if (is.factor(x[[i]])) {
      categorical_cols <- c(categorical_cols, i)
    }
  }
  categorical_cols
}

categorical_features_to_int <- function(x, cat_indices){
  for (i in cat_indices){
    x[[i]] <- as.integer(x[[i]]) -1
  }
  x
}
