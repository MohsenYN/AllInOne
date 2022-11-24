#' Density plot Generator
#'
#' @description Generate density plots for the selected dependent variables based on the selected independent variables.
#'
#' @param input object including user's input values
#' @param rv object including reactive variables
#'
#' @noRd
DensityPlot <- function(input, rv) {

  set_wd('Data Visualization')

  dat = rv$data
  VarPYSLK <- dplyr::select(rv$independent_variables, dplyr::all_of(input$boxplot_vars))
  SelectedTraits <- rv$dependent_variables

  A <- base::list()
  for (j in base::colnames(VarPYSLK)) {
    for (i in base::colnames(SelectedTraits)) {
      if (base::is.character(SelectedTraits[, i]) == TRUE) {
        base::print(base::paste0(i, " is not continuous trait"))
      } else {
        ME <- base::as.factor(dat[, j])
        A[[i]] <- ggplot2::ggplot(data = SelectedTraits, ggplot2::aes_string(x = i,
                                                           fill = ME)) +
          ggplot2::geom_density(alpha = 0.1) +
          ggplot2::labs(
            x = i,
            title = base::paste0(input$project_name, " -- Density plot -- ", j),
            subtitle = i
          ) +
          ggplot2::guides(fill = ggplot2::guide_legend(j)) +
          ggplot2::theme_classic()
        ggplot2::ggsave(
          A[[i]],
          file = base::paste0(input$project_name, " -- Density plot ( ", j, " -- ", i, " ).png"),
          width = 32,
          height = 15,
          units = "cm"
        )
        ggplot2::ggsave(
          A[[i]],
          file = base::paste0(input$project_name, " -- Density plot ( ", j, " -- ", i, " ).pdf"),
          width = 32,
          height = 15,
          units = "cm"
        )
      }
    }
    P <- gridExtra::grid.arrange(grobs = A, ncol = 2)
    ggplot2::ggsave(
      P,
      file = base::paste0(input$project_name, " -- Density plot ( ", j, " -- ", 'All Traits', " ).png"),
      width = 32,
      height = 15,
      units = "cm"
    )
    grDevices::pdf(file = base::paste0(input$project_name, " -- Density plot ( ", j, " -- ", 'All Traits', " ).pdf"))
    P <- gridExtra::grid.arrange(grobs = A, ncol = 2)
    base::invisible(base::print(P))
    base::invisible(grDevices::dev.off())
  }

  A <- base::list()
  for (i in base::colnames(SelectedTraits)) {
    if (base::is.character(SelectedTraits[, i]) == TRUE) {
      A[[i]] <- ggplot2::ggplot(SelectedTraits, ggplot2::aes_string(x = i)) +
        ggplot2::geom_histogram(stat = "count") +
        ggplot2::geom_density(alpha = .2, fill = "Blue") +
        ggplot2::labs(x = i,
             title = "Density plot",
             subtitle = i) +
        ggplot2::theme_classic()
      ggplot2::ggsave(
        A[[i]],
        file = base::paste0(input$project_name, " -- Density Plot -- ", i, ".png"),
        width = 32,
        height = 15,
        units = "cm"
      )
      ggplot2::ggsave(
        A[[i]],
        file = base::paste0(input$project_name, " -- Density Plot -- ", i, ".pdf"),
        width = 32,
        height = 15,
        units = "cm"
      )
    }
    else {
      A[[i]] <- ggplot2::ggplot(SelectedTraits, ggplot2::aes_string(x = i)) +
        ggplot2::geom_histogram(
          ggplot2::aes(y = ..density..),
          colour = "black",
          fill = "Red",
          alpha = .2
        ) +
        ggplot2::geom_density(alpha = .2, fill = "Blue") +
        ggplot2::labs(x = i,
             title = "Density plot ",
             subtitle = i) +
        ggplot2::theme_classic()
      ggplot2::ggsave(
        A[[i]],
        file = base::paste0(input$project_name, " -- Density plot -- ", i, ".png"),
        width = 32,
        height = 15,
        units = "cm"
      )
      ggplot2::ggsave(
        A[[i]],
        file = base::paste0(input$project_name, " -- Density plot -- ", i, ".pdf"),
        width = 32,
        height = 15,
        units = "cm"
      )
    }
  }

  set_wd('Data Visualization', rv, input$save_results)
}