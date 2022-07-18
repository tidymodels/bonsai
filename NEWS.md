# bonsai 0.1.0.9000 (developmental version)

To be released as 0.1.1.

* Corrected mapping of the `mtry` argument in `boost_tree` with the lightgbm 
  engine. `mtry` previously mapped to the `feature_fraction` argument to
  `lgb.train` but was documented as mapping to an argument more closely 
  resembling `feature_fraction_bynode`. `mtry` now maps 
  to `feature_fraction_bynode`.
  
  This means that code that set `feature_fraction_bynode` as an argument to
  `set_engine()` will now error, and the user can now pass `feature_fraction`
  to `set_engine()` without raising an error. 
  

# bonsai 0.1.0

Initial release!
