# boost_tree with lightgbm

    Code
      set_mode(set_engine(boost_tree(), "lightgbm"), "regression")
    Output
      Boosted Tree Model Specification (regression)
      
      Computational engine: lightgbm 
      

---

    Code
      set_mode(set_engine(boost_tree(), "lightgbm", nrounds = 100), "classification")
    Output
      Boosted Tree Model Specification (classification)
      
      Engine-Specific Arguments:
        nrounds = 100
      
      Computational engine: lightgbm 
      

# bonsai handles mtry vs mtry_prop gracefully

    `mtry` must be greater than or equal to 1, not 0.5.
    i `mtry` is currently being interpreted as a count rather than a proportion.
    i Supply `counts = FALSE` to `set_engine()` to supply this argument as a proportion rather than a count.
    i See `?train_lightgbm()` for more details.

---

    `mtry` must be less than or equal to 1, not 3.
    i `mtry` is currently being interpreted as a proportion rather than a count.
    i Supply `counts = TRUE` to `set_engine()` to supply this argument as a count rather than a proportion.
    i See `?train_lightgbm()` for more details.

# tuning mtry vs mtry_prop

    Code
      fit(set_mode(set_engine(boost_tree(mtry = tune::tune()), "lightgbm"),
      "regression"), bill_length_mm ~ ., data = penguins)
    Condition
      Error in `fit()`:
      ! `feature_fraction_bynode` must be a number, not a call.

# lightgbm warns if user uses `param` argument in set_engine()

    Code
      fit(mod_spec, mpg ~ ., mtcars)
    Condition
      Warning:
      Arguments passed in through `params` as a list will be ignored.
      Instead pass the arguments directly to the `...`.
    Output
      parsnip model object
      
      LightGBM Model (1 tree)
      Objective: regression
      Fitted to dataset with 10 columns

# training wrapper warns on protected arguments

    Code
      .res <- fit(set_mode(set_engine(boost_tree(), "lightgbm", colnames = paste0("X",
        1:ncol(penguins))), "regression"), bill_length_mm ~ ., data = penguins)
    Condition
      Warning:
      The following argument is guarded by bonsai and will not be passed to LightGBM: colnames.

---

    Code
      .res <- fit(set_mode(set_engine(boost_tree(), "lightgbm", colnames = paste0("X",
        1:ncol(penguins)), callbacks = list(p = print)), "regression"),
      bill_length_mm ~ ., data = penguins)
    Condition
      Warning:
      The following arguments are guarded by bonsai and will not be passed to LightGBM: colnames and callbacks.

---

    Code
      .res <- fit(set_mode(set_engine(boost_tree(), "lightgbm", colnames = paste0("X",
        1:ncol(penguins))), "regression"), bill_length_mm ~ ., data = penguins)
    Condition
      Warning:
      The following argument is guarded by bonsai and will not be passed to LightGBM: colnames.

---

    Code
      fit(set_mode(set_engine(boost_tree(), "lightgbm", n_iter = 10), "regression"),
      bill_length_mm ~ ., data = penguins)
    Condition
      Error in `fit()`:
      ! The `n_iter` argument passed to `set_engine()` (`?parsnip::set_engine()`) is an alias for a main model argument.
      i Please instead pass this argument via the `trees` argument to `boost_tree()` (`?parsnip::boost_tree()`).

---

    Code
      fit(set_mode(set_engine(boost_tree(), "lightgbm", num_tree = 10), "regression"),
      bill_length_mm ~ ., data = penguins)
    Condition
      Error in `fit()`:
      ! The `num_tree` argument passed to `set_engine()` (`?parsnip::set_engine()`) is an alias for a main model argument.
      i Please instead pass this argument via the `trees` argument to `boost_tree()` (`?parsnip::boost_tree()`).

---

    Code
      fit(set_mode(set_engine(boost_tree(), "lightgbm", min_split_gain = 2),
      "regression"), bill_length_mm ~ ., data = penguins)
    Condition
      Error in `fit()`:
      ! The `min_split_gain` argument passed to `set_engine()` (`?parsnip::set_engine()`) is an alias for a main model argument.
      i Please instead pass this argument via the `loss_reduction` argument to `boost_tree()` (`?parsnip::boost_tree()`).

---

    Code
      fit(set_mode(set_engine(boost_tree(), "lightgbm", min_split_gain = 2,
      lambda_l2 = 0.5), "regression"), bill_length_mm ~ ., data = penguins)
    Condition
      Error in `fit()`:
      ! The `min_split_gain` argument passed to `set_engine()` (`?parsnip::set_engine()`) is an alias for a main model argument.
      i Please instead pass this argument via the `loss_reduction` argument to `boost_tree()` (`?parsnip::boost_tree()`).

