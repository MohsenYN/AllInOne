#' Outlier Detector
#'
#' @description Detecting outliers in a given dataset.
#'
#' @param input object including user's input values
#' @param rv object including reactive variables
#'
#' @importFrom data.table :=
#' @noRd
#'
find_outliers <- function(rv, input) {
  db = rv$data
  outliers_pos = NULL

  # include Independent variables TOO
  rv$independent_variables <-
    rv$data %>% dplyr::select(input$main_db_indep_val)

  # include Dependent variables TOO
  rv$dependent_variables <-
      rv$data %>% dplyr::select(input$main_db_dep_val)

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

find_outliers_beta <- function(db, mini=0.25,maxi=0.75) {
  input_ = list('minp' = mini, 'maxp' = maxi)
  cloned = data.table::data.table(db)
  base::colnames(cloned)[1] <- ".___Buffer_dep"
  base::colnames(cloned)[2] <- ".___Buffer_indep"

  cloned[, outlier := check_outlier(.___Buffer_dep, input_), by = .___Buffer_indep]

  Res = list()
  counter = 0
  for (j in which(cloned[["outlier"]])) {
    counter <- counter + 1
    Res[[counter]] = base::c('Trait' = base::colnames(db)[1], 'Independent' = base::colnames(db)[2],'Observation' = j, 'Value' = cloned[['.___Buffer_dep']][j])
  }
  return(Res)
}