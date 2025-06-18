# boost_tree with catboost

    Code
      set_mode(set_engine(boost_tree(), "catboost"), "regression")
    Output
      Boosted Tree Model Specification (regression)
      
      Computational engine: catboost 
      

---

    Code
      set_mode(set_engine(boost_tree(), "catboost", iterations = 100),
      "classification")
    Output
      Boosted Tree Model Specification (classification)
      
      Engine-Specific Arguments:
        iterations = 100
      
      Computational engine: catboost 
      

# catboost warns if user uses `param` argument in set_engine()

    Code
      fit(mod_spec, mpg ~ ., mtcars)
    Condition
      Warning:
      Arguments passed in through `params` as a list will be ignored.
      Instead pass the arguments directly to the `...`.
    Output
      parsnip model object
      
      CatBoost model (1000 trees)
      Loss function: RMSE
      Fit to 10 feature(s)

