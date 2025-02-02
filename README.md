
<!-- README.md is generated from README.Rmd. Please edit that file -->

# compak: Conway-Maxwell-Poisson Associated Kernal

# compak <img src='man/figures/logo.svg' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test
coverage](https://codecov.io/gh/thomas-fung/compak/branch/main/graph/badge.svg)](https://codecov.io/gh/thomas-fung/compak?branch=main)
[![R-CMD-check](https://github.com/thomas-fung/compak/workflows/R-CMD-check/badge.svg)](https://github.com/thomas-fung/compak/actions)
<!-- badges: end -->

The `compak` package provided a discrete associated kernel smoother for
estimating probability mass functions using the mean-parametrised
Conway-Maxwell-Poisson (COMP) distribution of [Huang
(2017)](https://doi.org/10.1177%2F1471082X17697749). Two automated
bandwidth selection approaches, one based on minimizing Kullback-Leidler
divergence and another based on cross-validation, are also implemented.

## Installation

### Stable release on CRAN

`compak` is not available on [CRAN](https://CRAN.R-project.org) yet.

### Development version on Github

You can use the **devtools** package to install the development version
of **compak** from [GitHub](https://github.com/thomas-fung/compak):

``` r
# install.packages("devtools")
devtools::install_github("thomas-fung/compak")
library(compak)
```

## Usage

A reference manual and some examples are available at
[thomas-fung.github.io/compak](https://thomas-fung.github.io/compak/).

## Citation

If you use this package to analyse your data, please use the following
citation:

-   Sippel, L., Fung, T. and Huang, A. (2020). compak:
    Conway-Maxwell-Poisson associated kernel. R package version
    0.0.0.9000.

From R you can use:

``` r
citation("compak")
toBibtex(citation("compak"))
```
