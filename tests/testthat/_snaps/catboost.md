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

# train_catboost() validates argument ranges

    Code
      train_catboost(x, y, iterations = 0)
    Condition
      Error in `fit()`:
      ! `iterations` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      train_catboost(x, y, iterations = Inf)
    Condition
      Error in `fit()`:
      ! `iterations` must be a whole number, not `Inf`.

---

    Code
      train_catboost(x, y, learning_rate = -1)
    Condition
      Error in `fit()`:
      ! `learning_rate` must be a number larger than or equal to 0, not the number -1.

---

    Code
      train_catboost(x, y, depth = 0)
    Condition
      Error in `fit()`:
      ! `depth` must be a whole number between 1 and 16, not the number 0.

---

    Code
      train_catboost(x, y, depth = 17)
    Condition
      Error in `fit()`:
      ! `depth` must be a whole number between 1 and 16, not the number 17.

---

    Code
      train_catboost(x, y, l2_leaf_reg = -1)
    Condition
      Error in `fit()`:
      ! `l2_leaf_reg` must be a number larger than or equal to 0, not the number -1.

---

    Code
      train_catboost(x, y, random_strength = -1)
    Condition
      Error in `fit()`:
      ! `random_strength` must be a number larger than or equal to 0, not the number -1.

---

    Code
      train_catboost(x, y, bagging_temperature = -1)
    Condition
      Error in `fit()`:
      ! `bagging_temperature` must be a number larger than or equal to 0, not the number -1.

---

    Code
      train_catboost(x, y, rsm = 1.5)
    Condition
      Error in `fit()`:
      ! `rsm` must be a number between 0 and 1, not the number 1.5.

---

    Code
      train_catboost(x, y, validation = 1)
    Condition
      Error in `fit()`:
      ! `validation` must be a number between 0 and 0.999, not the number 1.

# train_catboost() checks parameter constraints

    Code
      train_catboost(x, y, iterations = 2, max_leaves = 8)
    Condition
      Error in `fit()`:
      ! `max_leaves` can only be used when `grow_policy = "Lossguide"`.
      i The grow policy is "SymmetricTree".

---

    Code
      train_catboost(x, y, iterations = 2, max_leaves = 8, grow_policy = "Depthwise")
    Condition
      Error in `fit()`:
      ! `max_leaves` can only be used when `grow_policy = "Lossguide"`.
      i The grow policy is "Depthwise".

---

    ! `min_n` (`min_data_in_leaf` in catboost) is ignored when the grow policy is "SymmetricTree".
    i Set `grow_policy` to "Depthwise" or "Lossguide" for it to have an effect.

---

    Code
      train_catboost(iris_x, iris_y, iterations = 2, subsample = 0.5)
    Condition
      Error in `fit()`:
      ! `sample_size` (`subsample` in catboost) cannot be used when the bootstrap type is "Bayesian".
      i Set `bootstrap_type` to "MVS" or "Bernoulli" to subsample the data.

---

    Code
      train_catboost(x, y, iterations = 2, subsample = 0.5, bootstrap_type = "No")
    Condition
      Error in `fit()`:
      ! `sample_size` (`subsample` in catboost) cannot be used when the bootstrap type is "No".
      i Set `bootstrap_type` to "MVS" or "Bernoulli" to subsample the data.

---

    Code
      train_catboost(x, y, iterations = 2, bagging_temperature = 2, bootstrap_type = "Bernoulli")
    Condition
      Error in `fit()`:
      ! `bagging_temperature` cannot be used when the bootstrap type is "Bernoulli".
      i It is only used with `bootstrap_type = "Bayesian"`.

---

    ! `bagging_temperature` is ignored when the bootstrap type is "MVS".
    i It is only used with `bootstrap_type = "Bayesian"`.

---

    ! `one_hot_max_size` has no effect since there are no factor predictors.

