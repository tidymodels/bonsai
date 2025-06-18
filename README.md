
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bonsai <img src="man/figures/logo.png" align="right" width=280 />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/bonsai)](https://CRAN.R-project.org/package=bonsai)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/bonsai/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/bonsai?branch=main)
[![R-CMD-check](https://github.com/tidymodels/bonsai/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/bonsai/actions)
<!-- badges: end -->

bonsai provides bindings for additional tree-based model engines for use
with the [parsnip](https://parsnip.tidymodels.org/) package.

This package is based off of the work done in the [treesnip
repository](https://github.com/curso-r/treesnip) by Athos Damiani,
Daniel Falbel, and Roel Hogervorst. bonsai is the official CRAN version
of the package; new development will reside here.

## Installation

You can install the most recent official release of bonsai with:

``` r
install.packages("bonsai")
```

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
| boost_tree    | catboost | regression     |
| boost_tree    | catboost | classification |
| decision_tree | partykit | regression     |
| decision_tree | partykit | classification |
| rand_forest   | partykit | regression     |
| rand_forest   | partykit | classification |
| rand_forest   | aorsf    | classification |
| rand_forest   | aorsf    | regression     |

Note that the catboost engine package isn’t available on CRAN, and
should thus be installed according to [their
documentation](https://catboost.ai/docs/en/installation/r-installation-binary-installation).

## Code of Conduct

Please note that the bonsai project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
