
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fwlplot

<!-- badges: start -->
<!-- badges: end -->

This is a super simple package to help make scatter plots of two
variables after residualizing by covariates. This package uses `fixest`
so things are super fast. This is meant to (as much as possible) be a
drop in replacement for `fixest::feols`. You should be able to replace
`feols` with `fwl_plot` and get a plot.

## Installation

The stable version of `fwlplot` is available on CRAN.

``` r
install.packages("fwlplot")
```

Or, you can grab the latest development version from GitHub.

``` r
# install.packages("remotes")
remotes::install_github("kylebutts/fwlplot")
```

## Example

Here’s a simple example with fixed effects removed by `fixest`.

``` r
library(fwlplot)
library(fixest)

flights <- data.table::fread("https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv")
flights[, long_distance := distance > 2000]
# Sample 10000 rows
sample <- flights[sample(.N, 10000)]
```

``` r
# Without covariates = scatterplot
fwl_plot(dep_delay ~ air_time, data = sample)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
# With covariates = FWL'd scatterplot
fwl_plot(
  dep_delay ~ air_time | origin + dest,
  data = sample, vcov = "hc1"
)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

### Plot random sample

If you have a large dataset, we can plot a sample of points with the
`n_sample` argument. This determines the number of points *per plot*
(see multiple estimation below).

``` r
fwl_plot(
  dep_delay ~ air_time | origin + dest,
  # Full dataset for estimation, 1000 obs. for plotting
  data = flights, n_sample = 1000
)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

### Full `feols` compatability

This is meant to be a 1:1 drop-in replacement with fixest, so everything
should work by just replacing `feols` with

``` r
feols(
  dep_delay ~ air_time | origin + dest,
  data = sample, subset = ~long_distance, cluster = ~origin
)
#> OLS estimation, Dep. Var.: dep_delay
#> Observations: 1,738
#> Subset: long_distance
#> Fixed-effects: origin: 2,  dest: 15
#> Standard-errors: Clustered (origin) 
#>          Estimate Std. Error  t value Pr(>|t|) 
#> air_time 0.013261   0.122372 0.108369  0.93128 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> RMSE: 43.6     Adj. R2: -0.001756
#>              Within R2:  2.333e-5
```

``` r
fwl_plot(
  dep_delay ~ air_time | origin + dest,
  data = sample, subset = ~long_distance, cluster = ~origin
)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

### Multiple estimation

``` r
# Multiple y variables
fwl_plot(
  c(dep_delay, arr_delay) ~ air_time | origin + dest,
  data = sample
)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

``` r
# `split` sample
fwl_plot(
  c(dep_delay, arr_delay) ~ air_time | origin + dest,
  data = sample, split = ~long_distance, n_sample = 1000
)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

``` r
# `fsplit` = `split` sample and Full sample
fwl_plot(
  c(dep_delay, arr_delay) ~ air_time | origin + dest,
  data = sample, fsplit = ~long_distance, n_sample = 1000
)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

### ggplot2

``` r
library(ggplot2)
theme_set(theme_grey(base_size = 16))
fwl_plot(
  c(dep_delay, arr_delay) ~ air_time | origin + dest,
  data = sample, fsplit = ~long_distance,
  n_sample = 1000, ggplot = TRUE
)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />
