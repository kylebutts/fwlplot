get_resids <- function(est, x_var, n_sample = NULL) {
  # Fix: "no visibile binding for global variable"
  vars = idx = NULL

  resid_list <- lapply(est, function(x) {
    var <- as.character(x$fml[[2]])

    sample_var <- x$model_info$sample$var 
    sample <- x$model_info$sample$value
    if (is.null(sample)) sample <- ""
    if (sample != "" & sample != "Full sample") { 
      sample = paste0(sample_var, " = ", sample)
    } 

    resid <- stats::resid(x, na.rm = FALSE)
    idx <- which(!is.na(resid))

    df <- data.frame(
      var = var,
      sample = sample,
      idx = idx,
      y_resid = resid[idx]
    )
  })
  
  df <- data.table::rbindlist(resid_list)
  rownames(df) <- NULL

  y_resid = df[(df$var != x_var), ]
  x_resid = df[(df$var == x_var), ]
  colnames(x_resid)[colnames(x_resid) == "y_resid"] = "x_resid"
  x_resid$var = NULL
  
  resid = merge(
    y_resid, 
    x_resid,
    by = c("sample", "idx")
  )

  data.table::setorder(resid, "var", "sample", "idx")
  data.table::setcolorder(resid, c("var", "sample", "idx"))

  # Add predictions
  resid = data.table::rbindlist(lapply(
    split(resid, interaction(resid$var, resid$sample)), 
    function(df) {
      df = add_prediction(df)
      # Sample points if needed
      if (!is.null(n_sample)) {
        df = df[sample(nrow(df), min(nrow(df), n_sample)), ]
      }
      return(df)
    }
  ))
  return(resid)
}

# When x is the only variable, use the original variables (scatterplot)
get_resids_no_cov <- function(pt_est, x_var, n_sample = NULL) {

  if (!inherits(pt_est, "fixest_multi")) {
    pt_est = list(pt_est)
  }

  resid_list <- lapply(pt_est, function(est) {
    var <- as.character(est$fml[[2]])
    
    sample_var <- est$model_info$sample$var 
    sample <- est$model_info$sample$value
    if (is.null(sample)) sample <- ""
    if (sample != "" & sample != "Full sample") { 
      sample = paste0(sample_var, " = ", sample)
    } 

    # Steps, figure out observation idx
    resid <- stats::resid(est, na.rm = FALSE)
    idx <- which(!is.na(resid))

    # Not actually resid, original values
    y_resid = resid[idx] + est$fitted.values
    x_resid = as.numeric(
      stats::model.matrix(est, type = "rhs", subset = x_var)
    )

    df <- data.frame(
      var = var,
      sample = sample,
      idx = idx,
      y_resid = y_resid,
      x_resid = x_resid
    )
    
    df = add_prediction(df)

    # Sample points if needed
    if (!is.null(n_sample)) {
      df = df[sample(nrow(df), min(nrow(df), n_sample)), ]
    }

    return(df)
  })
  
  resid <- data.table::rbindlist(resid_list)
  data.table::setorder(resid, "var", "sample", "idx")
  data.table::setcolorder(resid, c("var", "sample", "idx"))
  return(resid)
}

add_prediction = function(data) { 
  data <- data[order(data$x_resid), ]
  data <- data[stats::complete.cases(data), ]
  pred <- stats::predict(
    stats::lm(y_resid ~ x_resid, data = data),
    newdata = data, interval = "confidence"
  )
  return(cbind(data, pred))
}
