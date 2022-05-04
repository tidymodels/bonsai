# nocov start

make_boost_tree_gbm <- function() {
  parsnip::set_model_engine(
    model = "boost_tree",
    mode = "regression",
    eng = "gbm"
  )

  parsnip::set_model_engine(
    model = "boost_tree",
    mode = "classification",
    eng = "gbm"
  )

  parsnip::set_dependency(
    model = "boost_tree",
    eng = "gbm",
    pkg = "gbm",
    mode = "regression"
  )

  parsnip::set_dependency(
    model = "boost_tree",
    eng = "gbm",
    pkg = "bonsai",
    mode = "regression"
  )

  parsnip::set_dependency(
    model = "boost_tree",
    eng = "gbm",
    pkg = "gbm",
    mode = "classification"
  )

  parsnip::set_dependency(
    model = "boost_tree",
    eng = "gbm",
    pkg = "bonsai",
    mode = "classification"
  )
  parsnip::set_fit(
    model = "boost_tree",
    eng = "gbm",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "gbm", fun = "gbm.fit"),
      defaults = list(verbose = FALSE, distribution = "gaussian", keep.data = FALSE)
    )
  )

  parsnip::set_encoding(
    model = "boost_tree",
    mode = "regression",
    eng = "gbm",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "gbm",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata =quote(new_data),
        n.trees = quote(object$fit$n.trees),
        type = "response"
      )
    )
  )

  parsnip::set_fit(
    model = "boost_tree",
    eng = "gbm",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "bonsai", fun = "gbm_class_fit"),
      defaults = list(verbose = FALSE, distribution = "bernoulli", keep.data = FALSE)
    )
  )

  parsnip::set_encoding(
    model = "boost_tree",
    mode = "classification",
    eng = "gbm",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "gbm",
    mode = "classification",
    type = "class",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      object = quote(object),
      newdata =quote(new_data),
      n.trees = quote(object$n.trees),
      type = "response"
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "gbm",
    mode = "classification",
    type = "prob",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      object = quote(object),
      newdata = quote(new_data)
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "gbm",
    mode = "classification",
    type = "raw",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      object = quote(object),
      newdata = quote(new_data)
    )
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "gbm",
    parsnip = "tree_depth",
    original = "interaction.depth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "gbm",
    parsnip = "trees",
    original = "n.trees",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = TRUE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "gbm",
    parsnip = "learn_rate",
    original = "shrinkage",
    func = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "gbm",
    parsnip = "min_n",
    original = "n.minobsinnode",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "gbm",
    parsnip = "loss_reduction",
    original = "min_gain_to_split",
    func = list(pkg = "dials", fun = "loss_reduction"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "gbm",
    parsnip = "sample_prop",
    original = "bag.fraction",
    func = list(pkg = "dials", fun = "sample_prop"),
    has_submodel = FALSE
  )
}

# nocov end

#' @export
gbm_class_fit <- function(x, y, ...) {
  lvls <- levels(y)
  if (length(lvls) != 2) {
    rlang::abort("Per the gbm NEWS file, `gbm()` support for multinomial outcomes is 'problematic' and not supported.")
  } else {
    y <- as.numeric(y) - 1
  }

  x <- as.data.frame(x)
  cl <- rlang::call2("gbm.fit", .ns = "gbm", x = quote(x), y = quote(y), ...)
  res <- rlang::eval_tidy(cl)
  if (inherits(res, "gbm")) {
    res$.lvls <- lvls
  }
  res
}



#' @export
#' @rdname multi_predict
multi_predict._cbm <-
  function(object, new_data, type = NULL, trees = NULL, ...) {
    if (any(names(enquos(...)) == "newdata"))
      rlang::abort("Did you mean to use `new_data` instead of `newdata`?")

    if (is.null(trees)) {
      trees <- object$n.tree
    }
    trees <- sort(trees)

    if (is.null(type)) {
      if (object$num.classes == 1) {
        type <- "class"
      } else {
        type <- "numeric"
      }
    }

    res <- predict(object, new_data, type = "response", n.trees = trees)
    if (type == "numeric") {
      res <- as.data.frame(t(res))
      res$trees <- trees
      res <-
        tidyr::pivot_longer(res, cols = c(-trees), names_to = ".row", values_to = ".pred") %>%
        dplyr::group_nest(.row, .key = ".pred") %>%
        dplyr::select(-.row)
    }


    res

  }

