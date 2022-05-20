
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bonsai

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/bonsai)](https://CRAN.R-project.org/package=bonsai)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/bonsai/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/bonsai?branch=main)
[![R-CMD-check](https://github.com/tidymodels/bonsai/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/bonsai/actions)
<!-- badges: end -->

The goal of bonsai is to provide bindings for additional tree-based
model engines for use with the ‘parsnip’ package. This package is based
off of the work done in the [treesnip
repository](https://github.com/curso-r/treesnip) by Athos Damiani,
Daniel Falbel, and Roel Hogervorst. bonsai will be the official version
that will go to CRAN; new development will reside here.  
*Note that this package is in very early stages of development, and is
not yet minimally functional.*

## Installation

You can install the development version of bonsai from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("tidymodels/bonsai")
```

## Available Engines

The bonsai package provides additional engines for the models in the
following table:

| model         | engine   | mode           |
|:--------------|:---------|:---------------|
| boost_tree    | lightgbm | regression     |
| boost_tree    | lightgbm | classification |
| decision_tree | partykit | regression     |
| decision_tree | partykit | classification |
| rand_forest   | partykit | regression     |
| rand_forest   | partykit | classification |

## Code of Conduct

Please note that the bonsai project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.