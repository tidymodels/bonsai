#' @importFrom rlang enquo call2 eval_tidy new_quosure empty_env enquos expr
#' @importFrom purrr map_dfr map_df
#' @importFrom tibble as_tibble tibble
#' @importFrom parsnip multi_predict set_mode set_engine fit
#' @importFrom parsnip decision_tree boost_tree rand_forest
#' @importFrom stats predict
#' @importFrom utils packageVersion

# ------------------------------------------------------------------------------

#' @importFrom parsnip %>%
#' @export
parsnip::`%>%`

# quiet R CMD CHECK warning re: declared Imports
#' @importFrom dials min_n
dials::min_n()

# ------------------------------------------------------------------------------

utils::globalVariables(
  c(
    "categorical_columns",
    "categorical_features_to_int",
    "new_data",
    "object"
  )
)

# ------------------------------------------------------------------------------

# lightgbm had significant breaking changes following release v3.3.2.
# this function is used by patches that make bonsai backward-compatible with
# older lightgbm versions
using_newer_lightgbm_version <- function() {
  utils::packageVersion("lightgbm") > package_version("3.3.2")
}
