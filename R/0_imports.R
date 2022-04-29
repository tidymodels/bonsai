#' @keywords internal
"_PACKAGE"

#' @importFrom rlang enquo call2 eval_tidy new_quosure empty_env enquos expr
#' @importFrom purrr map_dfr map_df
#' @importFrom tibble as_tibble tibble
#' @importFrom parsnip multi_predict set_mode set_engine
#' @importFrom stats predict

# ------------------------------------------------------------------------------

utils::globalVariables(
  c(
    "categorical_columns", "categorical_features_to_int",
    "new_data", "object"
  )
)


