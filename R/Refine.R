#' Refine Outliers
#'
#' @description Automatically refine outliers by removing all the detected outliers.
#'
#' @param input object including user's input values
#' @param rv object including reactive variables
#'
#' @noRd

Refine <- function(input, rv) {

  COLN = base::colnames(rv$dependent_variables)

  MI = rv$data

  ._MSROUT <- MI


  j <- input$indep_outlier_2

  for (i in COLN) {
    if (base::is.character(MI[, i]) == TRUE) { base::print(base::paste0(i, " is not continuous trait"))
    } else {
      ._MSROUT <- ._MSROUT %>%
        dplyr::group_by(._MSROUT[, j])

      ._MSROUT <- base::as.data.frame(._MSROUT)
      MS <- base::as.factor(._MSROUT[, j])
 }
  }

}
