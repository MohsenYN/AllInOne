#' Listed Outliers
#'
#' @description return the list of outliers in a numeric vector
#'
#' @param v a numeric vector
#' @param input object
#' @param Coefficient factor which is 1.5 by defualt
#'
#' @return return a vector in which all the raws number of outliers are listed
#'
#' @noRd
#'
check_outlier <- function(v, input, coef = 1.5) {
  quantiles <- stats::quantile(
    v,
    probs = base::c(base::as.numeric(input$minp),
                    base::as.numeric(input$maxp)),
    na.rm = TRUE)
  IQR <- quantiles[2] - quantiles[1]
  res <- (v < (quantiles[1] - coef * IQR)) | (v > (quantiles[2] + coef * IQR))
  base::return(res)
}
