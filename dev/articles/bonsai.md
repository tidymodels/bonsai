# Introduction to bonsai

The goal of bonsai is to provide bindings for additional tree-based
model engines for use with the {parsnip} package.

If you’re not familiar with parsnip, you can read more about the package
on it’s [website](https://parsnip.tidymodels.org).

To get started, load bonsai with:

``` r
library(bonsai)
#> Loading required package: parsnip
```

To illustrate how to use the package, we’ll fit some models to a dataset
containing measurements on 3 different species of penguins. Loading in
that data and checking it out:

``` r
library(modeldata)
#> 
#> Attaching package: 'modeldata'
#> The following object is masked from 'package:datasets':
#> 
#>     penguins

data(penguins)

str(penguins)
#> tibble [344 × 7] (S3: tbl_df/tbl/data.frame)
#>  $ species          : Factor w/ 3 levels "Adelie","Chinstrap",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  $ island           : Factor w/ 3 levels "Biscoe","Dream",..: 3 3 3 3 3 3 3 3 3 3 ...
#>  $ bill_length_mm   : num [1:344] 39.1 39.5 40.3 NA 36.7 39.3 38.9 39.2 34.1 42 ...
#>  $ bill_depth_mm    : num [1:344] 18.7 17.4 18 NA 19.3 20.6 17.8 19.6 18.1 20.2 ...
#>  $ flipper_length_mm: int [1:344] 181 186 195 NA 193 190 181 195 193 190 ...
#>  $ body_mass_g      : int [1:344] 3750 3800 3250 NA 3450 3650 3625 4675 3475 4250 ...
#>  $ sex              : Factor w/ 2 levels "female","male": 2 1 1 NA 1 2 1 2 NA NA ...
```

Specifically, making use of our knowledge of which island that they live
on and measurements on their flipper length, we will predict their
species using a decision tree. We’ll first do so using the engine
`"rpart"`, which is supported with parsnip alone:

``` r
# set seed for reproducibility
set.seed(1)

# specify and fit model
dt_mod <- 
  decision_tree() |>
  set_engine(engine = "rpart") |>
  set_mode(mode = "classification") |>
  fit(
    formula = species ~ flipper_length_mm + island, 
    data = penguins
  )

dt_mod
#> parsnip model object
#> 
#> n= 344 
#> 
#> node), split, n, loss, yval, (yprob)
#>       * denotes terminal node
#> 
#>  1) root 344 192 Adelie (0.441860465 0.197674419 0.360465116)  
#>    2) flipper_length_mm< 206.5 214  64 Adelie (0.700934579 0.294392523 0.004672897)  
#>      4) island=Biscoe,Torgersen 96   1 Adelie (0.989583333 0.000000000 0.010416667) *
#>      5) island=Dream 118  55 Chinstrap (0.466101695 0.533898305 0.000000000)  
#>       10) flipper_length_mm< 192.5 59  20 Adelie (0.661016949 0.338983051 0.000000000) *
#>       11) flipper_length_mm>=192.5 59  16 Chinstrap (0.271186441 0.728813559 0.000000000) *
#>    3) flipper_length_mm>=206.5 130   7 Gentoo (0.015384615 0.038461538 0.946153846)  
#>      6) island=Dream,Torgersen 7   2 Chinstrap (0.285714286 0.714285714 0.000000000) *
#>      7) island=Biscoe 123   0 Gentoo (0.000000000 0.000000000 1.000000000) *
```

From this output, we can see that the model generally first looks to
`island` to determine species, and then makes use of a mix of flipper
length and island to ultimately make a species prediction.

A benefit of using parsnip and bonsai is that, to use a different
implementation of decision trees, we simply change the engine argument
to `set_engine`; all other elements of the interface stay the same. For
instance, using `"partykit"`—which implements a type of decision tree
called a *conditional inference tree*—as our backend instead:

``` r
decision_tree() |>
  set_engine(engine = "partykit") |>
  set_mode(mode = "classification") |>
  fit(
    formula = species ~ flipper_length_mm + island, 
    data = penguins
  )
#> parsnip model object
#> 
#> 
#> Model formula:
#> species ~ flipper_length_mm + island
#> 
#> Fitted party:
#> [1] root
#> |   [2] island in Biscoe
#> |   |   [3] flipper_length_mm <= 203
#> |   |   |   [4] flipper_length_mm <= 196: Adelie (n = 38, err = 0.0%)
#> |   |   |   [5] flipper_length_mm > 196: Adelie (n = 7, err = 14.3%)
#> |   |   [6] flipper_length_mm > 203: Gentoo (n = 123, err = 0.0%)
#> |   [7] island in Dream, Torgersen
#> |   |   [8] island in Dream
#> |   |   |   [9] flipper_length_mm <= 192: Adelie (n = 59, err = 33.9%)
#> |   |   |   [10] flipper_length_mm > 192: Chinstrap (n = 65, err = 26.2%)
#> |   |   [11] island in Torgersen: Adelie (n = 52, err = 0.0%)
#> 
#> Number of inner nodes:    5
#> Number of terminal nodes: 6
```

This model, unlike the first, relies on recursive conditional inference
to generate its splits. As such, we can see it generates slightly
different results. Read more about this implementation of decision trees
in
[`?details_decision_tree_partykit`](https://parsnip.tidymodels.org/reference/details_decision_tree_partykit.html).

One generalization of a decision tree is a *random forest*, which fits a
large number of decision trees, each independently of the others. The
fitted random forest model combines predictions from the individual
decision trees to generate its predictions.

bonsai introduces support for random forests using the `partykit`
engine, which implements an algorithm called a *conditional random
forest*. Conditional random forests are a type of random forest that
uses conditional inference trees (like the one we fit above!) for its
constituent decision trees.

To fit a conditional random forest with partykit, our code looks pretty
similar to that which we we needed to fit a conditional inference tree.
Just switch out
[`decision_tree()`](https://parsnip.tidymodels.org/reference/decision_tree.html)
with
[`rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
and remember to keep the engine set as `"partykit"`:

``` r
rf_mod <- 
  rand_forest() |>
  set_engine(engine = "partykit") |>
  set_mode(mode = "classification") |>
  fit(
    formula = species ~ flipper_length_mm + island, 
    data = penguins
  )
```

Read more about this implementation of random forests in
[`?details_rand_forest_partykit`](https://parsnip.tidymodels.org/reference/details_rand_forest_partykit.html).

Another generalization of a decision tree is a series of decision trees
where *each tree depends on the results of previous trees*—this is
called a *boosted tree*. bonsai implements an additional parsnip engine
for this model type called `lightgbm`. To make use of it, start out with
a `boost_tree` model spec and set `engine = "lightgbm"`:

``` r
bt_mod <- 
  boost_tree() |>
  set_engine(engine = "lightgbm") |>
  set_mode(mode = "classification") |>
  fit(
    formula = species ~ flipper_length_mm + island, 
    data = penguins
  )

bt_mod
#> parsnip model object
#> 
#> LightGBM Model (100 trees)
#> Objective: multiclass (3 classes)
#> Fitted to dataset with 2 columns
```

Read more about this implementation of boosted trees in
[`?details_boost_tree_lightgbm`](https://parsnip.tidymodels.org/reference/details_boost_tree_lightgbm.html).

Each of these model specs and engines have several arguments and tuning
parameters that affect user experience and results greatly. We recommend
reading about each of these parameters and tuning them when you find
them relevant for your modeling use case.
