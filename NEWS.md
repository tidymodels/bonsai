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
