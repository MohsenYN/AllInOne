#' Scatterplot Generator
#'
#' @description Generate an scatterplot based on two dependent variables using marginplot function
#' @param input object including user's input values
#' @param rv object including reactive variables
#'
#' @noRd
CheckSatterplot <- function(input, rv) {

  set_wd('Data Visualization')

  Scatter <- rv$data %>% dplyr::select(input$scatter_vars)

  p <- function()
    VIM::marginplot(Scatter)

  filesave('png', input$project_name, " -- Scatter plot ", p, rv)
  filesave('pdf', input$project_name, " -- Scatter plot ", p, rv)

  set_wd('Data Visualization', rv, input$save_results)
}