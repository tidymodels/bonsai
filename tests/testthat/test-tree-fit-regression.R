test_that("tree regression basics", {
  skip_if_not_installed("modeldata")
  library(modeldata)

  data(penguins)

  set.seed(1)

  tree_fit <-
    tree::tree(body_mass_g ~ bill_length_mm + island, data = penguins)

  tree_preds <-
    predict(tree_fit, penguins)

  bonsai_spec <- decision_tree() %>%
    set_mode("regression") %>%
    set_engine("tree")

  bonsai_fit <- bonsai_spec %>%
    fit(body_mass_g ~ bill_length_mm + island, data = penguins)

  bonsai_preds <-
    predict(bonsai_fit, penguins)

  expect_equal(
    unname(tree_preds),
    bonsai_preds$.pred,
    tolerance = .01
  )
})
