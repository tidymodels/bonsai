make_decision_tree_tree <- function() {
  parsnip::set_model_engine(
    model = "decision_tree",
    mode = "regression",
    eng = "tree"
  )

  parsnip::set_dependency(
    model = "decision_tree",
    eng = "tree",
    pkg = "tree"
  )

  parsnip::set_dependency(
    model = "decision_tree",
    eng = "tree",
    pkg = "bonsai"
  )

  parsnip::set_encoding(
    model = "decision_tree",
    eng = "tree",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "decision_tree",
    eng = "tree",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "bonsai", fun = "tree_fit"),
      defaults = list()
    )
  )

  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "tree",
    parsnip = "min_n",
    original = "minsize",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "decision_tree",
    eng = "tree",
    parsnip = "cost_complexity",
    original = "mindev",
    func = list(pkg = "dials", fun = "cost_complexity"),
    has_submodel = FALSE
  )

  num_info <- list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = rlang::expr(object$fit),
        newdata = rlang::expr(new_data)
      )
  )

  parsnip::set_pred(
    model = "decision_tree",
    eng = "tree",
    mode = "regression",
    type = "numeric",
    value = num_info
  )
}
