#' Missing Data Imputation
#'
#' @description Impute missing data using one of methods provided in MICE package.
#'
#' @param input object including user's input values
#' @param rv object including reactive variables
#'
#' @noRd
ImputeMissing <- function(input, rv, session) {
  if (!require(mice))
  utils::install.packages("mice")

  set_wd('Missing Imputation')

  rv$buffer = T
  if (base::sum(base::is.na(rv$dependent_variables)) == 0) {
    rv$buffer = F
    session$sendCustomMessage(
      type = 'testmessage',
      message = " WoW! You do not have any missing data! Cool!")
  }

  else if (input$impute_method != 'rm') {
    SelectedTraitsss = rv$dependent_variables
    if (base::ncol(SelectedTraitsss) == 1) {
      SelectedTraitsss = base::cbind(SelectedTraitsss, SelectedTraitsss)
      base::names(SelectedTraitsss)[2] <- base::paste("DUPLICATED")
    }
    PMM_MD <-
      mice::mice(
        SelectedTraitsss,
        method = input$impute_method,
        m = base::as.numeric(input$mice_input_m),
        maxit = base::as.numeric(input$mice_input_maxit),
        seed = base::as.numeric(input$mice_input_seed),
        remove.collinear = base::as.logical(input$mice_input_collinear)
      )

    p <- function()
      base::print(mice::bwplot(PMM_MD))

    filesave('png', input$project_name, " -- Box-and-whisker plots ", p, rv)
    filesave('pdf', input$project_name, " -- Box-and-whisker plots ", p, rv)

    p <- function()
      base::print(base::plot(PMM_MD))

    filesave('png', input$project_name, " -- Mean of the imputed variables based on the number of multiple imputations  ", p, rv)
    filesave('pdf', input$project_name, " -- Mean of the imputed variables based on the number of multiple imputations  ", p, rv)

    p <- function()
      base::print(mice::densityplot(PMM_MD))

    filesave('png', input$project_name, " -- Density plot based on the number of multiple imputations  ", p, rv)
    filesave('pdf', input$project_name, " -- Density plot based on the number of multiple imputations  ", p, rv)

    p <- function()
      base::print(mice::stripplot(PMM_MD))

    filesave('png', input$project_name, " -- Strip plot based on the number of multiple imputations  ", p, rv)
    filesave('pdf', input$project_name, " -- Strip plot based on the number of multiple imputations  ", p, rv)


    CPMMData <- mice::complete(PMM_MD)
    OFILE <- base::as.data.frame(CPMMData)

    FinalDataset <- base::cbind(rv$independent_variables, OFILE)

    utils::write.csv(FinalDataset,
              file = base::paste0(input$project_name, " -- Missing imputation dataset.csv"), row.names = FALSE)
  }

  else if (input$impute_method == 'rm') {
    FinalDataset <- stats::na.omit(rv$data)
    utils::write.csv(FinalDataset,
              file = base::paste0(input$project_name, " -- Removing missing datapoints.csv"), row.names = FALSE)
  }
  # else {
  #   session$sendCustomMessage(
  #     type = 'testmessage',
  #     message = "Please select a valid option!")
  # }

  set_wd('Missing Imputation', rv, input$save_results)
}
