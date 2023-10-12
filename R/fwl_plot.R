#' FWL Plot
#'
#' This function creates a bivariate plot of `y` and `x` after residualizing over a set of covariates `w`.
#'
#' @param fml Of the form `y ~ x + covs | fes` following the fixest formula syntax. The x variable you want plotted should come first.
#' @param data A `dataframe` object that contains the variables in `fml`.
#' @param ggplot Boolean. Default is to use base R plot but if TRUE, use ggplot.
#' @param ... Additional arguments passed to `fixest::feols`.
#' @examples
#' if (interactive()) {
#'   fwl_plot(mpg ~ hp + wt | cyl, mtcars)
#' }
#' 
#' @return Either NULL if `ggplot = FALSE` or a ggplot object if `ggplot = TRUE`.
#'
#' @export
fwl_plot <- function(fml, data, ggplot = TRUE, ...) {

  pt_est = fixest::feols(
    fml, data, notes = FALSE, ...
  )

  # Reset
  fixest::setFixest_fml(..fwl_plot_FE = ~1)
  fixest::setFixest_fml(..fwl_plot_x = ~1)
  fixest::setFixest_fml(..fwl_plot_w = ~1)
  fixest::setFixest_fml(..fwl_plot_y = ~1)

  ## Process formula -----------------------------------------------------------
  fml = fixest::xpd(fml)
  parts = get_fml_parts(fml)
  
  has_fe = !is.null(parts$W_FE)
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

  x_var = deparse(fixest::xpd(~..fwl_plot_x)[[2]])

  # Process multiple y vars
  y_vars = rev(fml_breaker(parts$y_fml, "c"))
  has_multi_y = (length(y_vars) > 1)
  y_vars = unlist(lapply(y_vars, deparse))
  
  outcome_fml = paste0("c(",
    paste(c(y_vars, x_var), collapse = ", "), 
    ")"
  )
  fixest::setFixest_fml(..fwl_plot_outcomes = outcome_fml)


  ## Create formula ------------------------------------------------------------
  if (has_w & has_fe) {
    fml_new <- fixest::xpd(
      ..fwl_plot_outcomes ~ ..fwl_plot_w | ..fwl_plot_FE
    )
  } else if (has_w) {
    fml_new <- fixest::xpd(
      ..fwl_plot_outcomes ~ ..fwl_plot_w
    )
  } else {
    fml_new <- fixest::xpd(
      ..fwl_plot_outcomes ~ 0
    )
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
    resids = get_resids(est, x_var)
  } else {
    resids = get_resids_no_cov(pt_est, x_var)
  }

  ## Plot ----------------------------------------------------------------------

  if (ggplot == FALSE) {
    plot_resids_base_r(resids, x_var, y_vars, is_residualized = should_run_reg)
    invisible(NULL)
  } 
  else {
    plot <- plot_resids_ggplot(resids, x_var, y_vars, is_residualized = should_run_reg)
    return(plot)
  }

}

#' @rdname fwl_plot
#' @export
fwlplot <- fwl_plot

plot_resids_ggplot <- function(resids, x_var, y_vars, is_residualized) {
  # Fix: "no visibile binding for global variable"
  var = x_resid = y_resid = NULL

  # Setup stuff
  has_multi_y = (nrow(resids) > 1)
  has_split = (resids[1, "sample"] != "")
  facet = NULL
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

  y_label = NULL
  if (is_residualized) {
    if (has_multi_y) {
      y_label = NULL
      resids$var = paste0("Residualized ", resids$var)
    } else {
      y_label = paste0("Residualized ", y_vars[1])
    }
  } else {
    if (has_multi_y) {
      y_label = NULL
    } else {
      y_label = y_vars[1]
    }
  }

  if (is_residualized) {
    x_label = paste0("Residualized ", x_var)
  } else {
    x_label = x_var
  }
  
  # Plotting
  plot <- ggplot2::ggplot(
    resids,
    mapping = ggplot2::aes(x = x_resid, y = y_resid)
  ) + 
    ggplot2::geom_smooth(
      formula = y ~ x,
      method = "lm",
      color = "darkgreen", linewidth = 1.2
    ) + 
    ggplot2::geom_point() +
    facet +
    ggplot2::labs(
      x = x_label, 
      y = y_label
    )

  return(plot)
}

plot_resids_base_r <- function(resids, x_var, y_vars, is_residualized) {

  # Need to `split` by = c("sample", "var")
  # Then `lapply` over each element of the list
  resids_fitted = lapply(
    split(resids, interaction(resids$sample, resids$var)), 
    function(data) {
      data = data[order(data$x_resid)]
      data = data[stats::complete.cases(data), ]
      pred = stats::predict(
        stats::lm(y_resid ~ x_resid, data = data), 
        newdata = data, interval = "confidence"
      ) 
      cbind(data, pred)
    }
  )

  # df = data.table::rbindlist(resids_fitted)
  df = resids_fitted[[1]]
  
  # set up graphics device and window for new plot
  graphics::plot.new()
  graphics::plot.window(
    xlim = range(df$x_resid, na.rm = TRUE), 
    ylim = range(df$lwr, df$upr, df$y_resid, na.rm = TRUE)
  )

  # add background grid
  graphics::grid()
  
  # residualized data points
  graphics::points(
    x = df$x_resid, y = df$y_resid,
    pch = 19, col = grDevices::adjustcolor("black", 0.5),
  )
  
  # best of fit line and CI
  graphics::polygon(
    c(df$x_resid, rev(df$x_resid)), c(df$lwr, rev(df$upr)),
    col = grDevices::adjustcolor("gray", 0.3), border = NA
  )
  graphics::lines(df$x_resid, y = df$fit, lwd = 2, col = "darkgreen")
  
  # axes: Add 'col = NA' args if you just want the ticks labels with no lines
  graphics::axis(1)
  graphics::axis(2, las = 2) 
  # plot frame: You might want to comment this out if you drop the axis lines
  graphics::box()
  
  graphics::title(main = coef_note, adj = 0)
  graphics::title(xlab = var_names[2], ylab = var_names[1])
}

