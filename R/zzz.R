# nocov start

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # Define the model functions in the parsnip model database
  make_decision_tree_tree()
}

# nocov end
