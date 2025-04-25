# nocov start

make_rand_forest_aorsf <- function() {
  parsnip::set_model_engine("rand_forest", "classification", "aorsf")
  parsnip::set_model_engine("rand_forest", "regression", "aorsf")

  parsnip::set_dependency(
    "rand_forest",
    "aorsf",
    "aorsf",
    mode = "classification"
  )
  parsnip::set_dependency(
    "rand_forest",
    "aorsf",
    "bonsai",
    mode = "classification"
  )

  parsnip::set_dependency("rand_forest", "aorsf", "aorsf", mode = "regression")
  parsnip::set_dependency("rand_forest", "aorsf", "bonsai", mode = "regression")

  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "aorsf",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "aorsf",
    parsnip = "trees",
    original = "n_tree",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "aorsf",
    parsnip = "min_n",
    original = "leaf_min_obs",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "aorsf",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "rand_forest",
    eng = "aorsf",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "aorsf", fun = "orsf"),
      defaults = list(
        n_thread = 1,
        verbose_progress = FALSE
      )
    )
  )

  parsnip::set_encoding(
    model = "rand_forest",
    eng = "aorsf",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "rand_forest",
    eng = "aorsf",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "aorsf", fun = "orsf"),
      defaults = list(
        n_thread = 1,
        verbose_progress = FALSE
      )
    )
  )

  parsnip::set_encoding(
    model = "rand_forest",
    eng = "aorsf",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "aorsf",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      # makes prob preds consistent with class ones.
      # note: the class predict method in aorsf uses the standard 'each tree
      # gets one vote' approach, which is usually consistent with probability
      # but not all the time. I opted to make predicted probability totally
      # consistent with predicted class in the parsnip bindings for aorsf b/c
      # I think it's really confusing when predicted probs do not align with
      # predicted classes. I'm fine with this in aorsf but in bonsai I want
      # to minimize confusion (#78).
      post = function(results, object) {
        missings <- apply(results, 1, function(x) any(is.na(x)))

        if (!any(missings)) {
          return(colnames(results)[apply(results, 1, which.max)])
        }

        obs <- which(!missings)

        out <- rep(NA_character_, nrow(results))
        out[obs] <- colnames(results)[apply(results[obs, ], 1, which.max)]
        out
      },
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        pred_type = "prob",
        verbose_progress = FALSE,
        na_action = 'pass'
      )
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "aorsf",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = function(x, object) {
        as_tibble(x)
      },
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        pred_type = 'prob',
        verbose_progress = FALSE,
        na_action = 'pass'
      )
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "aorsf",
    mode = "classification",
    type = "raw",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        verbose_progress = FALSE,
        na_action = 'pass'
      )
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "aorsf",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = as.numeric,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        pred_type = "mean",
        verbose_progress = FALSE,
        na_action = 'pass'
      )
    )
  )

  parsnip::set_pred(
    model = "rand_forest",
    eng = "aorsf",
    mode = "regression",
    type = "raw",
    value = list(
      pre = NULL,
      post = as.numeric,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        pred_type = "mean",
        verbose_progress = FALSE,
        na_action = 'pass'
      )
    )
  )
}

# nocov end
