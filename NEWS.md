# bonsai (development version)

# bonsai 0.4.0

## New Engines

* Added catboost engine to `boost_tree()` (#70).

## Improvements

* The lightgbm engine now warns that arguments passed to `params` argument in `set_engine()` are ignored (#110).

* Automatic handling of `num_classes` argument when specifying a multiclass classification objective for the lightgbm engine (#109).

* Increased the minimum R version to R 4.1.

## Bug Fixes

* Fixed bug where `num_threads` argument were ignored for lightgbm engine (#105).

# bonsai 0.3.2

* Resolves a test failure ahead of an upcoming parsnip release (#95).

* lightgbm models can now accept sparse matrices for training and prediction (#91).

# bonsai 0.3.1

* Fixed bug where `"aorsf"` models would not successfully fit in socket cluster workers (i.e. with `plan(multisession)`) unless another engine requiring bonsai had been fitted in the worker (#85).

# bonsai 0.3.0

* Introduced support for accelerated oblique random forests for the `"classification"` and `"regression"` modes using the new [`"aorsf"` engine](https://github.com/ropensci/aorsf) (#78 by `@bcjaeger`). 

* Enabled passing [Dataset Parameters](https://lightgbm.readthedocs.io/en/latest/Parameters.html#dataset-parameters) to the `"lightgbm"` engine. To pass an argument that would be usually passed as an element to the `param` argument in `lightgbm::lgb.Dataset()`, pass the argument directly through the ellipses in `set_engine()`, e.g. `boost_tree() %>% set_engine("lightgbm", linear_tree = TRUE)` (#77).

* Enabled case weights with the `"lightgbm"` engine (#72 by `@p-schaefer`).

* Fixed issues in metadata for the `"partykit"` engine for `rand_forest()` where some engine arguments were mistakenly protected (#74).

* Addressed type check error when fitting lightgbm model specifications with arguments mistakenly left as `tune()` (#79).

# bonsai 0.2.1

* The most recent dials and parsnip releases introduced tuning integration for the lightgbm `num_leaves` engine argument! The `num_leaves` parameter sets the maximum number of nodes per tree, and is an [important tuning parameter for lightgbm](https://lightgbm.readthedocs.io/en/latest/Parameters-Tuning.html) ([tidymodels/dials#256](https://github.com/tidymodels/dials/pull/256), [tidymodels/parsnip#838](https://github.com/tidymodels/parsnip/pull/838)). With the newest version of each of dials, parsnip, and bonsai installed, tune this argument by marking the `num_leaves` engine argument for tuning when defining your model specification:

``` r
boost_tree() %>% set_engine("lightgbm", num_leaves = tune())
```

* Fixed a bug where lightgbm's parallelism argument `num_threads` was overridden when passed via `param` rather than as a main argument. By default, then, lightgbm will fit sequentially rather than with `num_threads = foreach::getDoParWorkers()`. The user can still set `num_threads` via engine arguments with `engine = "lightgbm"`:

``` r
boost_tree() %>% set_engine("lightgbm", num_threads = x)
```

Note that, when tuning hyperparameters with the tune package, detection of parallel backend will still work [as usual](https://tune.tidymodels.org/articles/extras/optimizations.html).

* The `boost_tree` argument `stop_iter` now maps to the `lightgbm:::lgb.train()` argument `early_stopping_round` rather than its alias `early_stopping_rounds`. This does not affect parsnip's interface to lightgbm (i.e. via `boost_tree() %>% set_engine("lightgbm")`), though will introduce errors for code that uses the `train_lightgbm()` wrapper directly and sets the `lightgbm::lgb.train()` argument `early_stopping_round` by its alias `early_stopping_rounds` via `train_lightgbm()`'s `...`.

* Disallowed passing main model arguments as engine arguments to `set_engine("lightgbm", ...)` via aliases. That is, if a main argument is marked for tuning and a lightgbm alias is supplied as an engine argument, bonsai will now error, rather than supplying both to lightgbm and allowing the package to handle aliases. Users can still interface with non-main `boost_tree()` arguments via their lightgbm aliases ([#53](https://github.com/tidymodels/bonsai/issues/53)).

# bonsai 0.2.0

* Enabled bagging with lightgbm via the `sample_size` argument to `boost_tree` 
  (#32 and tidymodels/parsnip#768). The following docs now available in
  `?details_boost_tree_lightgbm` describe the interface in detail:
  
> The `sample_size` argument is translated to the `bagging_fraction` parameter in the `param` argument of `lgb.train`. The argument is interpreted by lightgbm as a _proportion_ rather than a count, so bonsai internally reparameterizes the `sample_size` argument with [dials::sample_prop()] during tuning.  
>   
> To effectively enable bagging, the user would also need to set the `bagging_freq` argument to lightgbm. `bagging_freq` defaults to 0, which means bagging is disabled, and a `bagging_freq` argument of `k` means that the booster will perform bagging at every `k`th boosting iteration. Thus, by default, the `sample_size` argument would be ignored without setting this argument manually. Other boosting libraries, like xgboost, do not have an analogous argument to `bagging_freq` and use `k = 1` when the analogue to `bagging_fraction` is in $(0, 1)$. _bonsai will thus automatically set_ `bagging_freq = 1` _in_ `set_engine("lightgbm", ...)` if `sample_size` (i.e. `bagging_fraction`) is not equal to 1 and no `bagging_freq` value is supplied. This default can be overridden by setting the `bagging_freq` argument to `set_engine()` manually. 

* Corrected mapping of the `mtry` argument in `boost_tree` with the lightgbm 
  engine. `mtry` previously mapped to the `feature_fraction` argument to
  `lgb.train` but was documented as mapping to an argument more closely 
  resembling `feature_fraction_bynode`. `mtry` now maps 
  to `feature_fraction_bynode`.
  
  This means that code that set `feature_fraction_bynode` as an argument to
  `set_engine()` will now error, and the user can now pass `feature_fraction`
  to `set_engine()` without raising an error. 

* Fixed error in lightgbm with engine argument `objective = "tweedie"` and 
  response values less than 1. 
  
* A number of documentation improvements, increases in testing coverage, and 
  changes to internals in anticipation of the 4.0.0 release of the lightgbm 
  package. Thank you to `@jameslamb` for the effort and expertise!

# bonsai 0.1.0

Initial release!
