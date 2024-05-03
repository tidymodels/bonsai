# nocov start

make_rand_forest_aorsf <- function(){


  parsnip::set_model_engine("rand_forest", "classification", "aorsf")
  parsnip::set_model_engine("rand_forest", "regression", "aorsf")
  parsnip::set_dependency("rand_forest", "aorsf", "aorsf", mode = "classification")
  parsnip::set_dependency("rand_forest", "aorsf", "aorsf", mode = "regression")

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
      defaults =
        list(
          n_thread = 1,
          verbose_progress = FALSE,
          tree_seeds = expr(sample.int(10 ^ 5, 1))
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
      defaults =
        list(
          n_thread = 1,
          verbose_progress = FALSE,
          tree_seeds = expr(sample.int(10 ^ 5, 1))
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
      # makes prob preds consistent with class ones
      post = function(results, object)
        colnames(results)[apply(results, 1, which.max)],
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          pred_type = "prob",
          verbose_progress = FALSE
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
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          pred_type = 'prob',
          verbose_progress = FALSE
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
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          verbose_progress = FALSE
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
      post = function(results, object)
        as.numeric(results),
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          pred_type = "mean",
          verbose_progress = FALSE
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
      post = function(results, object)
        as.numeric(results),
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          pred_type = "mean",
          verbose_progress = FALSE
        )
    )
  )


}

# nocov end
