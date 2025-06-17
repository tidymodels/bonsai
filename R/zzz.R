# nocov start

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  make_boost_tree_lightgbm()

  make_boost_tree_catboost()

  make_decision_tree_partykit()
  make_rand_forest_partykit()

  make_rand_forest_aorsf()
}

# nocov end
