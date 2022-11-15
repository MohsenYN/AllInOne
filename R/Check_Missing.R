#' Check Missing Values
#'
#' @description Handeling missing data in the dataset by providing several heatmap plots of missing pattern and descriptive statistics based on the selected independent variables.
#'
#' @param input object
#' @param rv object
#'
#' @noRd
CheckMissing <- function(input, rv) {
  if (!require(finalfit))
    utils::install.packages("finalfit")
  if (!require(naniar))
    utils::install.packages("naniar")

  set_wd('Missing Values')

  func_mplot <- function()
    base::print(finalfit::missing_plot(rv$dependent_variables))

  filesave("png",
           input$project_name,
           " -- Missing values in each trait",
           func_mplot)
  filesave("pdf",
           input$project_name,
           " -- Missing values in each trait",
           func_mplot)


  P <- finalfit::ff_glimpse(rv$dependent_variables)
  utils::write.csv(P$Continuous,
            file = base::paste0(input$project_name, " -- Descriptive statistics.csv"), row.names = FALSE)


  func_pp <- function()
    VIM::aggr(
      rv$dependent_variables,
      col = base::c('navyblue', 'yellow'),
      numbers = TRUE,
      sortVars = TRUE,
      labels = base::names(rv$dependent_variables),
      cex.axis = 0.7,
      cex.numbers = 0.55,
      gap = 3,
      ylab = base::c("Missing data", "Pattern")
    )

  filesave('png',
           input$project_name,
           " -- Missing values percentage and pattern",
           func_pp)
  filesave('pdf',
           input$project_name,
           " -- Missing values percentage and pattern",
           func_pp)

  set_wd('Missing Values', rv, input$save_results)
}
