# Boosted trees with catboost

`train_catboost` is a wrapper for `catboost` tree-based models where all
of the model arguments are in the main function.

## Usage

``` r
train_catboost(
  x,
  y,
  weights = NULL,
  iterations = 1000,
  learning_rate = 0.03,
  depth = 6,
  l2_leaf_reg = 3,
  random_strength = 1,
  bagging_temperature = 1,
  rsm = 1,
  quiet = TRUE,
  ...
)
```

## Source

https://catboost.ai/docs/en/references/training-parameters/.

## Arguments

- x:

  A data frame of predictors.

- y:

  A vector (factor or numeric) or matrix (numeric) of outcome data.

- weights:

  A numeric vector of sample weights, defaults to `NULL`.

- iterations:

  The maximum number of trees that can be built when solving machine
  learning problems. Default to 1000.

- learning_rate:

  A positive numeric value for the learning rate. Defaults to 0.03.

- depth:

  An integer for the depth of the trees. Default to 6.

- l2_leaf_reg:

  A numeric value for the L2 regularization coefficient. Used for leaf
  value calculation. Defaults to 3.

- random_strength:

  The amount of randomness to use for scoring splits when the tree
  structure is selected. Use this parameter to avoid overfitting the
  model. Defaults to 1.

- bagging_temperature:

  A numeric value, controls intensity of Bayesian bagging. The higher
  the temperature the more aggressive bagging is. Defaults to 1.

- rsm:

  A numeric value between 0 and 1, random subspace method. The
  percentage of features to use at each iteration of building trees. At
  each iteration, features are selected over again at random. Defaults
  to 1.

- quiet:

  A logical; should logging by catboost::catboost.train() be muted?

- ...:

  Other options to pass to catboost::catboost.train(). Arguments will be
  correctly routed to the `param` argument, or as a main argument,
  depending on their name.

## Value

A fitted `catboost.Model` object.

## Details

This is an internal function, not meant to be directly called by the
user.
