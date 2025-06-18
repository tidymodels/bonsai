# nocov start

make_boost_tree_catboost <- function() {
  parsnip::set_model_engine(
    model = "boost_tree",
    mode = "regression",
    eng = "catboost"
  )

  parsnip::set_model_engine(
    model = "boost_tree",
    mode = "classification",
    eng = "catboost"
  )

  parsnip::set_dependency(
    model = "boost_tree",
    eng = "catboost",
    pkg = "catboost",
    mode = "regression"
  )

  parsnip::set_dependency(
    model = "boost_tree",
    eng = "catboost",
    pkg = "bonsai",
    mode = "regression"
  )

  parsnip::set_dependency(
    model = "boost_tree",
    eng = "catboost",
    pkg = "catboost",
    mode = "classification"
  )

  parsnip::set_dependency(
    model = "boost_tree",
    eng = "catboost",
    pkg = "bonsai",
    mode = "classification"
  )
  parsnip::set_fit(
    model = "boost_tree",
    eng = "catboost",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "bonsai", fun = "train_catboost"),
      defaults = list(
        thread_count = 1,
        allow_writing_files = FALSE,
        random_seed = quote(sample.int(10^5, 1))
      )
    )
  )

  parsnip::set_encoding(
    model = "boost_tree",
    mode = "regression",
    eng = "catboost",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "catboost",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = "bonsai", fun = "predict_catboost_regression_numeric"),
      args = list(
        object = quote(object),
        new_data = quote(new_data)
      )
    )
  )

  parsnip::set_fit(
    model = "boost_tree",
    eng = "catboost",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "bonsai", fun = "train_catboost"),
      defaults = list(
        thread_count = 1,
        allow_writing_files = FALSE,
        random_seed = quote(sample.int(10^5, 1))
      )
    )
  )

  parsnip::set_encoding(
    model = "boost_tree",
    mode = "classification",
    eng = "catboost",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "catboost",
    mode = "classification",
    type = "class",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = NULL,
      func = c(pkg = "bonsai", fun = "predict_catboost_classification_class"),
      object = quote(object),
      new_data = quote(new_data)
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "catboost",
    mode = "classification",
    type = "prob",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = NULL,
      func = c(pkg = "bonsai", fun = "predict_catboost_classification_prob"),
      object = quote(object),
      new_data = quote(new_data)
    )
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng = "catboost",
    mode = "classification",
    type = "raw",
    value = parsnip::pred_value_template(
      pre = NULL,
      post = NULL,
      func = c(pkg = "bonsai", fun = "predict_catboost_classification_raw"),
      object = quote(object),
      new_data = quote(new_data)
    )
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "catboost",
    parsnip = "tree_depth",
    original = "depth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "catboost",
    parsnip = "trees",
    original = "iterations",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = TRUE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "catboost",
    parsnip = "learn_rate",
    original = "learning_rate",
    func = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "boost_tree",
    eng = "catboost",
    parsnip = "mtry_prop",
    original = "rsm",
    func = list(pkg = "dials", fun = "mtry_prop"),
    has_submodel = FALSE
  )
}

# nocov end
