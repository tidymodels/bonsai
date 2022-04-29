#' @keywords internal
"_PACKAGE"

#' @importFrom rlang enquo call2 abort eval_tidy warn new_quosure empty_env
#' @importFrom rlang enquos expr
#' @importFrom purrr map_dfr
#' @importFrom tibble is_tibble as_tibble tibble
#' @importFrom parsnip set_new_model
#' @importFrom withr with_options
#' @importFrom stats predict

# ------------------------------------------------------------------------------

utils::globalVariables(
  c(
    "new_data", "object"
  )
)


