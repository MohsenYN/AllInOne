#' Refine Outliers
#'
#' @description Automatically refine outliers by removing all the detected outliers.
#'
#' @param input object including user's input values
#' @param rv object including reactive variables
#'
#' @noRd

Refine <- function(input, rv) {

  set_wd('Refine')

  COLN = base::colnames(rv$dependent_variables)

  MI = rv$data

  ._MSROUT <- MI

  B <- base::list()

  j <- input$indep_outlier_2

  for (i in COLN) {
    if (base::is.character(MI[, i]) == TRUE) { base::print(base::paste0(i, " is not continuous trait"))
    } else {
      ._MSROUT <- ._MSROUT %>%
        dplyr::group_by(._MSROUT[, j])

      ._MSROUT <- base::as.data.frame(._MSROUT)
      MS <- base::as.factor(._MSROUT[, j])
      B[[i]] <- ggplot2::ggplot(._MSROUT, ggplot2::aes_string(x = MS, y = i, fill = MS)) +
        ggplot2::geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4) +
        ggplot2::theme_classic() +
        ggplot2::labs(x = i, title = "Refining outlier in each ", j, subtitle = i) +
        ggplot2::guides(fill = ggplot2::guide_legend(j)) +
        ggplot2::scale_x_discrete(name = j) +
        ggplot2::scale_y_continuous(name = i)
      ggplot2::ggsave(B[[i]], file = base::paste0("Refining outlier in each ", j, " for ", i, ".png"), width = 32, height = 15, units = "cm")
      ggplot2::ggsave(B[[i]], file = base::paste0("Refining outlier in each ", j, " for ", i, ".pdf"), width = 32, height = 15, units = "cm")
    }
  }
  if (base::length(COLN) > 1) {
    P <- gridExtra::grid.arrange(grobs = B, ncol = 2)
    ggplot2::ggsave(P, file = base::paste0(input$project_name, "-Refining outlier in each ", j, " (combine).png"), width = 32, height = 15, units = "cm")
    grDevices::pdf(file = base::paste0(input$project_name, "-Refining outlier in each ", j, " (combine).pdf"))
    P <- gridExtra::grid.arrange(grobs = B, ncol = 2)
    base::invisible(base::print(P))
    base::invisible(grDevices::dev.off())
  }
  set_wd('Refine', rv)
}