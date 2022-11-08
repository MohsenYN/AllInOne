#' Outlier Detector
#'
#' @description Detecting outliers in a given dataset.
#'
#' @param input object including user's input values
#' @param rv object including reactive variables
#'
#' @noRd
#'
find_outliers <- function(rv, input) {
  db = rv$data
  outliers_pos = NULL

  COLN = base::colnames(rv$dependent_variables)
  COL_INDEP <- input$indep_outlier_2
  cloned <- data.table::data.table(db)

  for (i in COLN) {
    col_i = cloned[[i]]
    if (base::is.numeric(col_i))
    {
      counter <- 0
      base::colnames(cloned)[base::which(base::names(cloned) == i)] <- ".___Buffer_dep"
      base::colnames(cloned)[base::which(base::names(cloned) == COL_INDEP)] <- ".___Buffer_indep"

      cloned[, outlier := check_outlier(.___Buffer_dep, input), by = .___Buffer_indep]


      for (j in cloned[["outlier"]]) {
        counter <- counter + 1

        if (!base::is.na(j) & j)
          outliers_pos = base::append(outliers_pos, base::c(i, counter, cloned[[".___Buffer_dep"]][counter]))
      }
      base::colnames(cloned)[base::which(base::names(cloned) == ".___Buffer_dep")] <- i
      base::colnames(cloned)[base::which(base::names(cloned) == ".___Buffer_indep")] <- COL_INDEP
    }
  }
  base::return(outliers_pos)
}