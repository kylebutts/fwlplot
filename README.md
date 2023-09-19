
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fwlplot

<!-- badges: start -->
<!-- badges: end -->

This is a super simple package to help make scatter plots of two
variables after residualizing by covariates. This package uses `fixest`
so things are super fast.

## Installation

You can install the development version of fwlplot like so:

``` r
devtools::install_github("kylebutts/fwlplot")
```

## Example

Here’s a simple example with fixed effects removed by `fixest`.

``` r
library(fwlplot)
library(fixest)

fwl_plot(mpg ~ hp + wt | cyl, data = mtcars)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

`ggplot` works too!

``` r
library(ggplot2)
fwl_plot(mpg ~ hp + wt | cyl, data = mtcars, ggplot = TRUE) + 
  theme_bw(base_size = 14)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
