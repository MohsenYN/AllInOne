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

  colors_f <- grDevices::colorRampPalette(rv$setting_colors)
  colors_ = colors_f(rv$setting_general_cnum)

  p <- function()
    VIM::marginplot(Scatter,
                    col = colors_)

  filesave('png', input$project_name, " -- Scatter plot ", p, rv)
  filesave('pdf', input$project_name, " -- Scatter plot ", p, rv)

  set_wd('Data Visualization', rv, input$save_results)
}