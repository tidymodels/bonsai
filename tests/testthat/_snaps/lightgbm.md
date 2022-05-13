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
        feature_fraction = 0.5) %>% set_mode("regression") %>% fit(bill_length_mm ~ .,
      data = penguins)
    Warning <rlang_warning>
      The following arguments cannot be manually modified and were removed: feature_fraction.
    Error <rlang_error>
      The supplied argument `mtry = 0.5` must be greater than or equal to 1. 
      
      `mtry` is currently being interpreted as a count rather than a proportion. Supply `counts = FALSE` to `set_engine` to supply this argument as a proportion rather than a count. 
      
      See `?train_lightgbm` for more details.

# tuning mtry vs mtry_prop

    Code
      boost_tree(mtry = tune::tune()) %>% set_engine("lightgbm") %>% set_mode(
        "regression") %>% fit(bill_length_mm ~ ., data = penguins)
    Error <rlang_error>
      The supplied `mtry` parameter is a call to `tune`. Did you forget to optimize hyperparameters with a tuning function like `tune::tune_grid`?

