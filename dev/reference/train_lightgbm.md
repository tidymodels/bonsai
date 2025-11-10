# Boosted trees with lightgbm

`train_lightgbm` is a wrapper for `lightgbm` tree-based models where all
of the model arguments are in the main function.

## Usage

``` r
train_lightgbm(
  x,
  y,
  weights = NULL,
  max_depth = -1,
  num_iterations = 100,
  learning_rate = 0.1,
  feature_fraction_bynode = 1,
  min_data_in_leaf = 20,
  min_gain_to_split = 0,
  bagging_fraction = 1,
  early_stopping_round = NULL,
  validation = 0,
  counts = TRUE,
  quiet = FALSE,
  ...
)
```

## Arguments

- x:

  A data frame or matrix of predictors

- y:

  A vector (factor or numeric) or matrix (numeric) of outcome data.

- weights:

  A numeric vector of sample weights.

- max_depth:

  An integer for the maximum depth of the tree.

- num_iterations:

  An integer for the number of boosting iterations.

- learning_rate:

  A numeric value between zero and one to control the learning rate.

- feature_fraction_bynode:

  Fraction of predictors that will be randomly sampled at each split.

- min_data_in_leaf:

  A numeric value for the minimum sum of instances needed in a child to
  continue to split.

- min_gain_to_split:

  A number for the minimum loss reduction required to make a further
  partition on a leaf node of the tree.

- bagging_fraction:

  Subsampling proportion of rows. Setting this argument to a non-default
  value will also set `bagging_freq = 1`. See the Bagging section in
  `?details_boost_tree_lightgbm` for more details.

- early_stopping_round:

  Number of iterations without an improvement in the objective function
  occur before training should be halted.

- validation:

  The *proportion* of the training data that are used for performance
  assessment and potential early stopping.

- counts:

  A logical; should `feature_fraction_bynode` be interpreted as the
  *number* of predictors that will be randomly sampled at each split?
  `TRUE` indicates that `mtry` will be interpreted in its sense as a
  *count*, `FALSE` indicates that the argument will be interpreted in
  its sense as a *proportion*.

- quiet:

  A logical; should logging by
  [`lightgbm::lgb.train()`](https://rdrr.io/pkg/lightgbm/man/lgb.train.html)
  be muted?

- ...:

  Other options to pass to
  [`lightgbm::lgb.train()`](https://rdrr.io/pkg/lightgbm/man/lgb.train.html).
  Arguments will be correctly routed to the `param` argument, or as a
  main argument, depending on their name.

## Value

A fitted `lightgbm.Model` object.

## Details

This is an internal function, not meant to be directly called by the
user.
