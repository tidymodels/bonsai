#' Internal function wrappers
#'
#' These functions are not intended for package users.
#'
#' @export
#' @keywords internal
#' @rdname bonsai_internal
tree_fit <- function(formula, data, cost_complexity = .01,
                     tree_depth = 30L, min_n = 2L, ...) {
  ctrl <-
    tree::tree.control(
      nobs = nrow(data),
      mincut = 0,
      minsize = min_n,
      mindev = cost_complexity
    )

  ctrl$nmax <- tree_depth

  mod_fit <-
    tree::tree(
      formula = formula,
      data = data,
      control = ctrl,
      ...
    )

  mod_fit
}
