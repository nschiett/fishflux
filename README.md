<!-- README.md is generated from README.Rmd. Please edit that file -->
fishflux: A tool to model nutrient fluxes in fish
=================================================

[![Build Status](https://api.travis-ci.org/nschiett/fishflux.png?branch=master)](https://travis-ci.org/nschiett/fishflux)

<img src="man/figures/fishflux.png" width = 120 alt="fishflux logo"/>

Overview
--------

The `fishflux` package provides a tool to model fluxes of C (carbon), N
(nitrogen) and P (phosphorus) in fish. It combines basic priciples from
elemental stoichiometry and metabolic theory. The package offers a
userfriendly interface to make nutrient dynamic modelling available for
anyone. `fishflux` is mostly targeted towards fish ecologists, wishing
to predict nutrient ingestion, egestion and excretion to study fluxes of
nutrients and energy.

Main assets:

-   Provides functions to model fluxes of Carbon, Nitrogen and
    Phosphorus for fish with or without the MCMC sampler provided by
    stan.
-   Provides some tools to find the right parameters as inputs into the
    model
-   Provides a plotting function to illustrate results

Theoretical framework
---------------------

For more information on the theoretical framework behind `cnp_model`,
check out the paper (add link to paper).

Installing and loading fishflux
-------------------------------

`fishflux` uses Markov Chain Monte Carlo simulations provided by
[stan](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).
Therefore, the first step is to install
[stan](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started).

### GitHub

The best way to install the latest development version of `fishflux` is
to instal it from GitHub.

``` r
install.packages("devtools")
devtools::install_github("nschiett/fishflux", dependencies=TRUE)
library(fishflux)
```

### CRAN

`fishflux` will be available on CRAN in the future:

``` r
install.packages("fishflux")
library(fishflux)
```

### Downloaded package file

Another option is to download the source file available on github
[here](https://github.com/nschiett/fishflux).

``` r
install.packages(path_to_fishflux_file, repos = NULL, type="source")
library(fishflux)
```

Documentation
-------------

add link to introduction

Citation
--------

Future releases
---------------
