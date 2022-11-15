#' Boxplot Generator
#'
#'  @description Generate box plots for the selected dependent variables based on the selected independent variables.
#'
#' @param input object
#' @param rv object
#'
#' @noRd
CheckBOXVIO <- function(input, rv) {

  set_wd('Box Plots')

  VarPYSL <- dplyr::select(rv$independent_variables, dplyr::all_of(input$boxplot_vars))

  for (i in base::colnames(rv$dependent_variables)) {

    for (j in base::colnames(VarPYSL)) {

      if (base::is.character(rv$dependent_variables[, i]) == TRUE) {
        base::print(base::paste0(i, " is not continuous trait"))
      } else {
        levels_j = base::length(base::unique(rv$data[[j]]))
        grDevices::png(file = base::paste0(
          input$project_name,
          " -- Boxplot and scatterplot -- ",
          j,
          " - ",
          i,
          ".png"
        ))
        A <- ggpubr::ggsummarystats(
          rv$data,
          j,
          i,
          ggfunc = ggpubr::ggboxplot,
          add = "jitter",
          color = j,
          labeller = "label_value",
          legend = "top",
          ggtheme = ggpubr::theme_pubr(x.text.angle = get_x_text_angle(levels_j))
        )
        base::invisible(base::print(A))
        base::invisible(grDevices::dev.off())

        grDevices::pdf(file = base::paste0(
          input$project_name,
          " -- Boxplot and scatterplot -- ",
          j,
          " - ",
          i,
          ".pdf"
        ))
        A <- ggpubr::ggsummarystats(
          rv$data,
          j,
          i,
          ggfunc = ggpubr::ggboxplot,
          add = "jitter",
          color = j,
          labeller = "label_value",
          legend = "top",
          ggtheme = ggpubr::theme_pubr(x.text.angle = get_x_text_angle(levels_j))
        )
        base::invisible(base::print(A))
        base::invisible(grDevices::dev.off())
      }
    }
  }
  set_wd('Box Plots', rv, input$save_results)

}