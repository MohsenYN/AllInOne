#' Checking Outliers
#'
#' @description Checking outliers in a given dataset using quantile and cook's distance methods.
#'
#' @param input object including user's input values
#' @param rv object including reactive variables
#'
#' @importFrom data.table :=
#' @noRd

PoSiBlEoUtLieR <- function(input, rv) {

  set_wd('Outlier')

  rv$outliers_row = rv$outliers = NULL

  # include Independent variables TOO
  rv$independent_variables <-
    rv$data %>% dplyr::select(input$main_db_indep_val)

  # include Dependent variables TOO
  rv$dependent_variables <-
    rv$data %>% dplyr::select(input$main_db_dep_val)

  if (input$outlier_method == 'A') {

    COLN = base::colnames(rv$dependent_variables)
    VarPYSL = dplyr::select(rv$data, dplyr::all_of(input$indep_outlier_2))

    A <- base::list()

    COL_INDEP <- input$indep_outlier_2
    cloned <- data.table::data.table(rv$data)
    colors_f <- grDevices::colorRampPalette(rv$setting_colors)
    for (j in base::colnames(VarPYSL)) {
      for (i in COLN) {
        if (base::is.character(rv$data[, i]) == TRUE) {
          shiny_showNotification(rv, base::paste0(i, " is not continuous trait so we ignore it"))
        }
        else {
          col_i = cloned[[i]]
          if (base::is.numeric(col_i))
          {
            counter <- 0
            base::colnames(cloned)[base::which(base::names(cloned) == i)] <- ".___Buffer_dep"
            base::colnames(cloned)[base::which(base::names(cloned) == COL_INDEP)] <- ".___Buffer_indep"

            cloned[, outlier := check_outlier(.___Buffer_dep, input), by = .___Buffer_indep]

            for (k in cloned$outlier) {
              counter = counter + 1
              if (!base::is.na(k) & k)
              {
                cloned$outlier[counter] =
                  base::length(base::which(cloned$.___Buffer_dep == cloned$.___Buffer_dep[counter] &
                                             cloned$.___Buffer_indep == cloned$.___Buffer_indep[counter]))
                if (cloned$outlier[counter] == 1)cloned$outlier[counter] = NA
              }
              else
                cloned$outlier[counter] = NA
            }
            base::colnames(cloned)[base::which(base::names(cloned) == ".___Buffer_dep")] <- i
            base::colnames(cloned)[base::which(base::names(cloned) == ".___Buffer_indep")] <- COL_INDEP
          }

          MS <- base::as.factor(rv$data[, j])
          A[[i]] <-
            ggplot2::ggplot(cloned, ggplot2::aes_string(
              x = MS,
              y = i,
              fill = MS
            )) +
              ggplot2::geom_text(ggplot2::aes(label = outlier), na.rm = TRUE, hjust = -1) +
              ggplot2::geom_boxplot(
                outlier.colour = "red",
                outlier.shape = 8,
                outlier.size = 4
              ) +
              ggplot2::theme_classic() +
              ggplot2::labs(x = i,
                            title = base::paste0("Detecting outlier in each ", j),
                            subtitle = i) +
              # ggplot2::guides(fill = ggplot2::guide_legend(j)) +
              ggplot2::scale_y_continuous(name = i) +
              ggplot2::scale_x_discrete(name = j) +
              ggplot2::scale_fill_manual(values = colors_f(base::length(base::levels(MS))))
          levels_j = base::length(base::unique(rv$data[[j]]))
          if (levels_j <= rv$Maximum_Level_For_Group_By) {
            ggplot2::ggsave(
              A[[i]],
              file = base::paste0("Detecting outlier in each ", j, " for ", i, ".png"),
              width = 32,
              height = 15,
              units = "cm"
            )
            ggplot2::ggsave(
              A[[i]],
              file = base::paste0("Detecting outlier in each ", j, " for ", i, ".pdf"),
              width = 32,
              height = 15,
              units = "cm"
            )
          }else
            shiny_showNotification(rv, base::paste0('As the ', j, ' column has ',
                                                    levels_j, ' Levels we ignore it for grouped by plots'))
        }
      }
    }

    outliers <- find_outliers(rv, input)

    counter = 2
    outl_rows = NULL
    for (i in 1:base::length(outliers)) {
      if (counter < base::length(outliers)) {
        outl_rows <- base::append(outl_rows, base::as.numeric(outliers[counter]))
      }else break
      counter = counter + 3
    }
    rv$outliers <- outliers
    rv$outliers_row <- outl_rows
  }

  else if (input$outlier_method == 'B') {

    # vars = base::unique(base::c(input$outlier_rand_interact, input$outlier_rand))
    vars = input$outlier_rand
    rv$outliers <- NULL
    colors_f <- grDevices::colorRampPalette(rv$setting_colors)

    for (outlier_resp in input$outlier_resp) {
      ##################FOR##################

      fix.F <- stats::as.formula(base::paste(outlier_resp, base::paste(vars, collapse = " + "), sep = " ~ "))

      model <- stats::lm(fix.F, data = rv$data)
      k <- olsrr::ols_prep_cdplot_data(model)
      d <- olsrr::ols_prep_outlier_obs(k)
      f <- olsrr::ols_prep_cdplot_outliers(k)

      buf = k$ckd
      buf[['obs']] <- rownames(buf)
      utils::write.csv(buf, base::paste0(input$project_name, '-- ', outlier_resp, ' -- Cooks distance values.csv'), row.names = F)
      utils::write.csv(f, base::paste0(input$project_name, '-- ', outlier_resp, ' -- Cooks distance outliers.csv'), row.names = F)

      ##############################
      p <- ggplot2::ggplot(d, ggplot2::aes(x = obs, y = cd, label = txt)) +
        ggplot2::geom_bar(width = 0.5,
                          stat = "identity", ggplot2::aes(fill = fct_color)) +
        ggplot2::scale_fill_manual(values = base::c("blue",
                                                    "red")) +
        ggplot2::labs(fill = "Observation") +
        ggplot2::ylim(0,
                      k$maxx) +
        ggplot2::ylab("Cook's D") +
        ggplot2::xlab("Observation") +
        ggplot2::ggtitle("Cook's D Bar Plot") +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::geom_hline(yintercept = k$ts, colour = "red") +
        ggplot2::geom_text(hjust = -0.2, nudge_x = 0.05, size = 2, na.rm = TRUE) +
        ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1.2,
                          vjust = 2, family = "serif", fontface = "italic",
                          colour = "darkred", label = base::paste("Threshold:",
                                                                  base::round(k$ts, 3))) +
        ggplot2::theme_classic() +
        ggplot2::scale_fill_manual(values = colors_f(2))
      grDevices::png(base::paste0(input$project_name, '-- ', outlier_resp, ' -- cooks distance bar.png'),
                     width = get_width(rv, 2))
      base::invisible(base::print(p))
      grDevices::dev.off()

      grDevices::pdf(base::paste0(input$project_name, '-- ', outlier_resp, ' -- cooks distance bar.pdf'))
      base::invisible(base::print(p))
      grDevices::dev.off()
      ##############################
      p <- ggplot2::ggplot(d, ggplot2::aes(x = obs, y = cd, label = txt, ymin = base::min(cd), ymax = cd)) +
        ggplot2::geom_linerange(colour = "blue") +
        ggplot2::geom_point(shape = 1, colour = "blue") +
        ggplot2::geom_hline(yintercept = k$ts,
                            colour = "red") +
        ggplot2::xlab("Observation") +
        ggplot2::ylab("Cook's D") +
        ggplot2::ggtitle("Cook's D Chart") +
        ggplot2::annotate("text",
                          x = Inf, y = Inf, hjust = 1.2, vjust = 2, family = "serif",
                          fontface = "italic", colour = "darkred",
                          label = base::paste("Threshold:", base::round(k$ts, 3))) +
        ggplot2::theme_classic()
      grDevices::png(base::paste0(input$project_name, '-- ', outlier_resp, ' -- cooks distance chart.png'), width = get_width(rv, 2))
      base::invisible(base::print(p))
      grDevices::dev.off()

      grDevices::pdf(base::paste0(input$project_name, '-- ', outlier_resp, ' -- cooks distance chart.pdf'))
      base::invisible(base::print(p))
      grDevices::dev.off()
      ##############################
      grDevices::png(base::paste0(input$project_name, '-- ', outlier_resp, ' --  DFFIT plot.png'),
                     width = get_width(rv, 2))
      base::invisible(base::print(olsrr::ols_plot_dffits(model)))
      grDevices::dev.off()

      grDevices::pdf(base::paste0(input$project_name, '-- ', outlier_resp, ' --  DFFIT plot.pdf'))
      base::invisible(base::print(olsrr::ols_plot_dffits(model)))
      grDevices::dev.off()
      ##############################
      grDevices::png(base::paste0(input$project_name, '-- ', outlier_resp, ' -- cooks distance Hadi plot.png'), width = get_width(rv, 2))
      base::invisible(base::print(olsrr::ols_plot_hadi(model)))
      grDevices::dev.off()

      grDevices::pdf(base::paste0(input$project_name, '-- ', outlier_resp, ' -- cooks distance Hadi plot.pdf'))
      base::invisible(base::print(olsrr::ols_plot_hadi(model)))
      grDevices::dev.off()
      ##############################
      grDevices::png(base::paste0(input$project_name, '-- ', outlier_resp, ' -- cooks distance resid plot.png'))
      base::invisible(base::print(olsrr::ols_plot_resid_pot(model)))
      grDevices::dev.off()

      grDevices::pdf(base::paste0(input$project_name, '-- ', outlier_resp, ' -- cooks distance resid plot.pdf'))
      base::invisible(base::print(olsrr::ols_plot_resid_pot(model)))
      grDevices::dev.off()
      ##############################
      grDevices::png(base::paste0(input$project_name, '-- ', outlier_resp, ' -- cooks distance resid_lev plot.png'))
      base::invisible(base::print(olsrr::ols_plot_resid_lev(model)))
      grDevices::dev.off()

      grDevices::pdf(base::paste0(input$project_name, '-- ', outlier_resp, ' -- cooks distance resid_lev plot.pdf'))
      base::invisible(base::print(olsrr::ols_plot_resid_lev(model)))
      grDevices::dev.off()

      rv$outliers_row <- c(rv$outliers_row, buf[['obs']][k$ckd$obs[base::which(k$ckd$color == 'outlier')]])

      rv$outliers_row = as.numeric(rv$outliers_row)

      for (i in rv$outliers_row)
        rv$outliers = base::c(rv$outliers, outlier_resp, i, rv$data[[outlier_resp]][i])

      for (gp in vars) {
        asfct = base::as.factor(rv$data[[gp]][1:base::length(d[[1]])])
        if (base::length(base::levels(asfct)) <= rv$Maximum_Level_For_Group_By) {
          p <- ggplot2::ggplot(d, ggplot2::aes(x = obs, y = cd, label = txt)) +
            ggplot2::geom_bar(width = 0.5, stat = "identity", ggplot2::aes(fill = asfct)) +
            ggplot2::labs(fill = gp) +
            ggplot2::ylim(0, k$maxx) +
            ggplot2::ylab("Cook's D") +
            ggplot2::xlab("Observation") +
            ggplot2::ggtitle(base::paste0('-- ', outlier_resp, "Cook's D Bar Plot")) +
            ggplot2::geom_hline(yintercept = 0) +
            ggplot2::geom_hline(yintercept = k$ts, colour = "red") +
            ggplot2::annotate("text", x = Inf, y = Inf, hjust = 1.2,
                              vjust = 2, family = "serif", fontface = "italic",
                              colour = "darkred", label = base::paste("Threshold:", base::round(k$ts, 3))) +
            ggplot2::theme_classic() +
            ggplot2::scale_fill_manual(values = colors_f(base::length(base::levels(asfct))))

          grDevices::png(base::paste0(input$project_name, '-- ', outlier_resp, ' -- cooks distance plot grouped by ', gp, '.png'),
                         width = get_width(rv, 2))
          base::invisible(base::print(p))
          grDevices::dev.off()

          grDevices::pdf(base::paste0(input$project_name, '-- ', outlier_resp, ' -- cooks distance plot grouped by ', gp, '.pdf'))
          base::invisible(base::print(p))
          grDevices::dev.off()
        }else {
          shiny_showNotification(rv, base::paste0('As the ', gp, ' column has ',
                                                  base::length(base::levels(asfct)), ' Levels we ignore it for grouped by plots'))
        }
      }
    }
  }
  set_wd('Outlier', rv, input$save_results)
}
