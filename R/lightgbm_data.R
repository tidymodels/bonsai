# nocov start

make_boost_tree_lightgbm <- function() {
  parsnip::set_model_engine(
    model = "boost_tree",
    mode = "regression",
    eng = "lightgbm"
  )

  parsnip::set_model_engine(
    model = "boost_tree",
    mode = "classification",
    eng = "lightgbm"
  )

  parsnip::set_dependency(
    model = "boost_tree",
    eng = "lightgbm",
    pkg = "lightgbm",
    mode = "regression"
  )

  parsnip::set_dependency(
    model = "boost_tree",
    eng = "lightgbm",
    pkg = "bonsai",
    mode = "regression"
  )

  parsnip::set_dependency(
    model = "boost_tree",
    eng = "lightgbm",
    pkg = "lightgbm",
    mode = "classification"
  )

  parsnip::set_dependency(
    model = "boost_tree",
    eng = "lightgbm",
    pkg = "bonsai",
    mode = "classification"
  )
  parsnip::set_fit(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "bonsai", fun = "train_lightgbm"),
      defaults = list(
        verbose = -1,
        num_threads = 0,
        seed = quote(sample.int(10^5, 1)),
        deterministic = TRUE
      )
    )
  )

  parsnip::set_encoding(
    model = "boost_tree",
    mode = "regression",
    eng = "lightgbm",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = TRUE
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "bonsai", fun = "predict_lightgbm_regression_numeric"),
      args = list(
        object = quote(object),
        new_data = quote(new_data)
      )
    )
  )

  parsnip::set_fit(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "bonsai", fun = "train_lightgbm"),
      defaults = list(
        verbose = -1,
        num_threads = 0,
        seed = quote(sample.int(10^5, 1)),
        deterministic = TRUE
      )
    )
  )

  parsnip::set_encoding(
    model = "boost_tree",
    mode = "classification",
    eng = "lightgbm",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = TRUE
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "classification",
    type = "class",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = NULL,
      func = c(pkg = "bonsai", fun = "predict_lightgbm_classification_class"),
      object = quote(object),
      new_data = quote(new_data)
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "classification",
    type = "prob",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = NULL,
      func = c(pkg = "bonsai", fun = "predict_lightgbm_classification_prob"),
      object = quote(object),
      new_data = quote(new_data)
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "lightgbm",
    mode = "classification",
    type = "raw",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = NULL,
      func = c(pkg = "bonsai", fun = "predict_lightgbm_classification_raw"),
      object = quote(object),
      new_data = quote(new_data)
    )
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "tree_depth",
    original = "max_depth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "trees",
    original = "num_iterations",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = TRUE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "learn_rate",
    original = "learning_rate",
    func = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "mtry",
    original = "feature_fraction_bynode",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "min_n",
    original = "min_data_in_leaf",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "loss_reduction",
    original = "min_gain_to_split",
    func = list(pkg = "dials", fun = "loss_reduction"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "sample_size",
    original = "bagging_fraction",
    func = list(pkg = "dials", fun = "sample_size"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "lightgbm",
    parsnip = "stop_iter",
    original = "early_stopping_round",
    func = list(pkg = "dials", fun = "stop_iter"),
    has_submodel = FALSE
  )
}

# nocov end
