#' FWL Plot
#'
#' This function creates a bivariate plot of `y` and `x` after residualizing over a set of covariates `w`.
#'
#' @param fml Of the form `y ~ x + covs | fes` following the fixest formula syntax. The x variable you want plotted should come first.
#' @param data A `dataframe` object that contains the variables in `fml`.
#' @param ggplot Boolean. Default is to use base R plot but if TRUE, use ggplot.
#' @param n_sample Numeric. Number of observations to sample for each facet.
#'  If NULL, will plot all rows.
#' @param ... Additional arguments passed to `fixest::feols`.
#' @examples
#' \donttest{
#' fwl_plot(mpg ~ hp + wt | cyl, mtcars)
#' }
#'
#' @return Either NULL if `ggplot = FALSE` or a ggplot object if `ggplot = TRUE`.
#'
#' @export
fwl_plot <- function(fml, data, ggplot = FALSE, n_sample = 1000, ...) {
  pt_est <- fixest::feols(
    fml, data,
    notes = FALSE, ...
  )

  # Reset
  fixest::setFixest_fml(..fwl_plot_FE = ~1)
  fixest::setFixest_fml(..fwl_plot_x = ~1)
  fixest::setFixest_fml(..fwl_plot_w = ~1)
  fixest::setFixest_fml(..fwl_plot_y = ~1)

  ## Process formula -----------------------------------------------------------
  fml <- fixest::xpd(fml)
  parts <- get_fml_parts(fml)

  has_fe <- !is.null(parts$W_FE)
  if (has_fe == TRUE) {
    fixest::setFixest_fml(..fwl_plot_FE = parts$W_FE)
  }

  covs <- fml_breaker(parts$W_lin, "+")
  has_w <- (length(covs) > 1)
  if (has_w == TRUE) {
    n_covs <- length(covs)
    fixest::setFixest_fml(..fwl_plot_x = covs[[n_covs]])
    fixest::setFixest_fml(
      ..fwl_plot_w =
        paste(rev(utils::head(covs, n_covs - 1)), collapse = " + ")
    )
  } else {
    fixest::setFixest_fml(..fwl_plot_x = covs[[1]])
  }

  x_var <- deparse(fixest::xpd(~..fwl_plot_x)[[2]])

  # Process multiple y vars
  y_vars <- rev(fml_breaker(parts$y_fml, "c"))
  has_multi_y <- (length(y_vars) > 1)
  y_vars <- unlist(lapply(y_vars, deparse))

  outcome_fml <- paste0(
    "c(",
    paste(c(y_vars, x_var), collapse = ", "),
    ")"
  )
  fixest::setFixest_fml(..fwl_plot_outcomes = outcome_fml)

  if (has_w & has_fe) {
    fml_new <- fixest::xpd(..fwl_plot_outcomes ~ ..fwl_plot_w | ..fwl_plot_FE)
  } else if (has_fe) {
    fml_new <- fixest::xpd(..fwl_plot_outcomes ~ 1 | ..fwl_plot_FE)
  } else if (has_w) {
    fml_new <- fixest::xpd(..fwl_plot_outcomes ~ ..fwl_plot_w)
  } else {
    fml_new <- fixest::xpd(..fwl_plot_outcomes ~ 0)
  }

  ## Residualize ---------------------------------------------------------------
  should_run_reg <- has_w | has_fe
  if (should_run_reg) {
    est <- fixest::feols(
      fml = fml_new,
      data = data,
      ...,
      notes = FALSE
    )
    resids <- get_resids(est, x_var, n_sample)
  } else {
    resids <- get_resids_no_cov(pt_est, x_var, n_sample)
  }

  ## Plot ----------------------------------------------------------------------

  if (ggplot == FALSE) {
    plot_resids_base_r(
      resids, x_var, y_vars,
      is_residualized = should_run_reg
    )
    invisible(NULL)
  } else {
    plot <- plot_resids_ggplot(
      resids, x_var, y_vars,
      is_residualized = should_run_reg
    )
    return(plot)
  }
}

#' @rdname fwl_plot
#' @export
fwlplot <- fwl_plot

# ggplot2 implementation
plot_resids_ggplot <- function(resids, x_var, y_vars, is_residualized) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The option `ggplot = TRUE` was used, but `ggplot2` is not installed")
  }

  # Fix: "no visibile binding for global variable"
  var <- x_resid <- y_resid <- fit <- lwr <- upr <- NULL

  # Check if a custom theme is set, otherwise use theme_light
  has_custom_theme <- !is_theme_default()
  if (has_custom_theme) {
    theme <- ggplot2::theme_get()
  } else {
    theme <- ggplot2::theme_light()
  }

  # Setup stuff
  has_multi_y <- (nrow(resids) > 1)
  has_split <- (resids[1, "sample"] != "")
  facet <- NULL
  if (has_multi_y & has_split) {
    facet <- ggplot2::facet_grid(
      rows = ggplot2::vars(var),
      cols = ggplot2::vars(sample),
      scales = "free_y"
    )
  }
  if (has_multi_y & !has_split) {
    facet <- ggplot2::facet_wrap(
      ggplot2::vars(var),
      scales = "free"
    )
  }
  if (!has_multi_y & has_split) {
    facet <- ggplot2::facet_wrap(
      ggplot2::vars(sample),
      scales = "fixed"
    )
  }

  y_label <- NULL
  if (is_residualized) {
    if (has_multi_y) {
      y_label <- NULL
      resids$var <- paste0("Residualized ", resids$var)
    } else {
      y_label <- paste0("Residualized ", y_vars[1])
    }
  } else {
    if (has_multi_y) {
      y_label <- NULL
    } else {
      y_label <- y_vars[1]
    }
  }

  if (is_residualized) {
    x_label <- paste0("Residualized ", x_var)
  } else {
    x_label <- x_var
  }

  # Plotting
  plot <- ggplot2::ggplot(
    resids,
  ) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        x = x_resid, ymin = lwr, ymax = upr
      ), 
      fill = "grey90"
    ) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = x_resid, y = fit
      ), 
      color = "darkgreen"
    ) + 
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = x_resid, y = y_resid
      )
    ) +
    facet +
    ggplot2::labs(
      x = x_label,
      y = y_label
    ) +
    theme

  return(plot)
}

# https://community.rstudio.com/t/how-to-check-if-the-user-has-set-a-global-ggplot-theme-with-theme-set-or-not/129162
is_theme_default <- function() {
  comparison <- all.equal(ggplot2::theme_get(), ggplot2::theme_gray())
  if (is.logical(comparison) & isTRUE(comparison)) {
    return(TRUE)
  }
  FALSE
}

# Base R implementation
plot_resids_base_r <- function(resids, x_var, y_vars, is_residualized) {
  y_lab = ""
  x_lab = ifelse(is_residualized, paste0("Residualized ", x_var), x_var)
  n_unique_var = length(unique(resids$var))
  n_unique_sample = length(unique(resids$sample))
   
  if (n_unique_var > 1 & n_unique_sample > 1) {
    facet_fml = var ~ sample
  } else if (n_unique_var > 1) {
    facet_fml = ~ var
  } else if (n_unique_sample > 1) {
    facet_fml = ~ sample
  } else {
    facet_fml = NULL
    y_lab = ifelse(is_residualized, paste0("Residualized ", y_vars), y_vars)
  }

  if (is_residualized) {
    resids$var <- paste0("Residualized ", resids$var)
  }

  tinyplot::tinyplot(
    x = resids$x_resid, y = resids$y_resid,
    facet = facet_fml,
    data = resids, 
    ylab = y_lab, 
    xlab = x_lab
  )
  tinyplot::tinyplot(
    x = resids$x_resid, ymin = resids$lwr, ymax = resids$upr,
    facet = facet_fml,
    data = resids, 
    type = "ribbon", add = TRUE
  )
}
