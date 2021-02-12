
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ERforResearch

This package implements the functionality to use the expected ranks (ER)
in the context of research evaluation as presented in [this recent
preprint](...). For a demonstration of the major functions check out the
vignette (Demo ERforResearch).

## Installation

The package can be downloaded from github using the `devtools`-package:

``` r
# First, you might have to install devtools from CRAN
install.packages("devtools")

# Then install ERforResearch
devtools::install_github("snsf-data/ERforResearch")
```

## Requirement

The Bayesian Hierarchical Models for the ER are fitted in JAGS using
`rjags`. Therefore you have to install the `rjags` from CRAN and JAGS
following the instructions from
[here](http://mcmc-jags.sourceforge.net/).
