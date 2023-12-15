# boost_tree with lightgbm

    Code
      boost_tree() %>% set_engine("lightgbm") %>% set_mode("regression")
    Output
      Boosted Tree Model Specification (regression)
      
      Computational engine: lightgbm 
      

---

    Code
      boost_tree() %>% set_engine("lightgbm", nrounds = 100) %>% set_mode(
        "classification")
    Output
      Boosted Tree Model Specification (classification)
      
      Engine-Specific Arguments:
        nrounds = 100
      
      Computational engine: lightgbm 
      

# bonsai handles mtry vs mtry_prop gracefully

    The supplied argument `mtry = 0.5` must be greater than or equal to 1. 
    
    `mtry` is currently being interpreted as a count rather than a proportion. Supply `counts = FALSE` to `set_engine` to supply this argument as a proportion rather than a count. 
    
    See `?train_lightgbm` for more details.

---

    The supplied argument `mtry = 3` must be less than or equal to 1. 
    
    `mtry` is currently being interpreted as a proportion rather than a count. Supply `counts = TRUE` to `set_engine` to supply this argument as a count rather than a proportion. 
    
    See `?train_lightgbm` for more details.

---

    Code
      pars_fit_8 <- boost_tree(mtry = 0.5) %>% set_engine("lightgbm",
        feature_fraction_bynode = 0.5) %>% set_mode("regression") %>% fit(
        bill_length_mm ~ ., data = penguins)
    Condition
      Warning:
      The following arguments cannot be manually modified and were removed: feature_fraction_bynode.
      Error:
      ! The supplied argument `mtry = 0.5` must be greater than or equal to 1. 
      
      `mtry` is currently being interpreted as a count rather than a proportion. Supply `counts = FALSE` to `set_engine` to supply this argument as a proportion rather than a count. 
      
      See `?train_lightgbm` for more details.

# tuning mtry vs mtry_prop

    Code
      boost_tree(mtry = tune::tune()) %>% set_engine("lightgbm") %>% set_mode(
        "regression") %>% fit(bill_length_mm ~ ., data = penguins)
    Condition
      Error:
      ! The supplied `mtry` parameter is a call to `tune`. Did you forget to optimize hyperparameters with a tuning function like `tune::tune_grid`?

# training wrapper warns on protected arguments

    Code
      boost_tree() %>% set_engine("lightgbm", n_iter = 10) %>% set_mode("regression") %>%
        fit(bill_length_mm ~ ., data = penguins)
    Condition
      Error in `fit()`:
      ! The `n_iter` argument passed to `set_engine()` (`?parsnip::set_engine()`) is an alias for a main model argument.
      i Please instead pass this argument via the `trees` argument to `boost_tree()` (`?parsnip::boost_tree()`).

---

    Code
      boost_tree() %>% set_engine("lightgbm", num_tree = 10) %>% set_mode(
        "regression") %>% fit(bill_length_mm ~ ., data = penguins)
    Condition
      Error in `fit()`:
      ! The `num_tree` argument passed to `set_engine()` (`?parsnip::set_engine()`) is an alias for a main model argument.
      i Please instead pass this argument via the `trees` argument to `boost_tree()` (`?parsnip::boost_tree()`).

---

    Code
      boost_tree() %>% set_engine("lightgbm", min_split_gain = 2) %>% set_mode(
        "regression") %>% fit(bill_length_mm ~ ., data = penguins)
    Condition
      Error in `fit()`:
      ! The `min_split_gain` argument passed to `set_engine()` (`?parsnip::set_engine()`) is an alias for a main model argument.
      i Please instead pass this argument via the `loss_reduction` argument to `boost_tree()` (`?parsnip::boost_tree()`).

---

    Code
      boost_tree() %>% set_engine("lightgbm", min_split_gain = 2, lambda_l2 = 0.5) %>%
        set_mode("regression") %>% fit(bill_length_mm ~ ., data = penguins)
    Condition
      Error in `fit()`:
      ! The `min_split_gain` argument passed to `set_engine()` (`?parsnip::set_engine()`) is an alias for a main model argument.
      i Please instead pass this argument via the `loss_reduction` argument to `boost_tree()` (`?parsnip::boost_tree()`).

