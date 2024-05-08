# bonsai 0.2.1.9000 (developmental version)

v0.2.1.9000 is a developmental version of the bonsai package.

* Addressed type check error when fitting lightgbm model specifications with arguments mistakenly left as `tune()` (#79).

* Fixed issues in metadata for the `"partykit"` engine for `rand_forest()` where some engine arguments were mistakenly protected (#74).

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
