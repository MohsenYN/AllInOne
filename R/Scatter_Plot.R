#' Scatterplot Generator
#'
#' @description Generate an scatterplot based on two dependent variables using marginplot function
#' @param input object including user's input values
#' @param rv object including reactive variables
#'
#' @noRd
CheckSatterplot <- function(input, rv) {

  set_wd('Box Plots')

  Scatter <- rv$data %>% dplyr::select(input$scatter_vars)

  p <- function()
    base::print(VIM::marginplot(Scatter))

  filesave('png', input$project_name, " -- Scatter plot ", p)
  filesave('pdf', input$project_name, " -- Scatter plot ", p)

  set_wd('Box Plots', 'out')
}