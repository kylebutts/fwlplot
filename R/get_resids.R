get_resids <- function(est, x_var) {
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
      sample = sample,
      idx = idx,
      var = var,
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
  return(resid)
}

# When x is the only variable, use the original variables (scatterplot)
get_resids_no_cov <- function(pt_est, x_var) {

  if (!inherits(pt_est, "fixest_multi")) {
    pt_est = list(pt_est)
  }

  resid_list <- lapply(pt_est, function(x) {
    var <- as.character(x$fml[[2]])
    
    sample_var <- x$model_info$sample$var 
    sample <- x$model_info$sample$value
    if (is.null(sample)) sample <- ""
    if (sample != "" & sample != "Full sample") { 
      sample = paste0(sample_var, " = ", sample)
    } 

    # Steps, figure out observation idx
    resid <- stats::resid(x, na.rm = FALSE)
    idx <- which(!is.na(resid))

    # Not actually resid, original values
    y_resid = resid[idx] + x$fitted.values
    x_resid = as.numeric(
      stats::model.matrix(x, type = "rhs", subset = x_var)
    )

    df <- data.frame(
      sample = sample,
      idx = idx,
      var = var,
      y_resid = y_resid,
      x_resid = x_resid
    )
  })
  
  resid <- data.table::rbindlist(resid_list)
  data.table::setorder(resid, "var", "sample", "idx")
  return(resid)
}


# Not needed, but it is nice code
get_resids_wide <- function(est, x_var) {
  
  resid_list <- lapply(est, function(x) {
    var <- as.character(x$fml[[2]])

    sample <- x$model_info$sample$value
    if (is.null(sample)) sample <- ""

    resid <- stats::resid(x, na.rm = FALSE)
    idx <- which(!is.na(resid))

    df <- data.frame(
      resid = resid[idx],
      idx = idx,
      var = var,
      sample = sample
    )
  })

  df <- do.call(
    "rbind",
    resid_list
  )
  df <- data.table::as.data.table(df)
  rownames(df) <- NULL

  # Pivot resid wider by var
  resid <- data.table::dcast(
    df,
    idx + sample ~ var,
    value.var = "resid"
  )

  return(resid)
}
