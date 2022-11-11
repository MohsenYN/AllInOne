#' Normalization
#'
#' @description Normalizing a given dataset based on the methods provided by bestNormalize package.
#'
#' @param input object including user's input values
#' @param rv object including reactive variables
#'
#' @noRd
NormaLiZaTIoN <- function(input, rv) {

  if (!require(car))
    utils::install.packages("car")
  if (!require(bestNormalize))
    utils::install.packages("bestNormalize")

  set_wd('Normalization')

  dat <- stats::na.omit(rv$data)

  VarPYSL <- dat %>% dplyr::select(dplyr::all_of(input$main_db_indep_val))
  SelectedTraits <- dat %>% dplyr::select(dplyr::all_of(input$main_db_dep_val))

  COLN <- base::colnames(SelectedTraits)

  for (i in COLN) {
    if (base::is.character(SelectedTraits[, i]) == TRUE) {
      base::print(base::paste0(i, " is not continuous trait"))
    } else {
      grDevices::png(file = base::paste0(i, "-normality plot (before normalization).png"))
      MASS::truehist(SelectedTraits[, base::match(i, COLN)],
                     nbins = base::as.numeric(input$nbin),
                     xlab = i)
      base::invisible(grDevices::dev.off())
      grDevices::pdf(file = base::paste0(i, "-normality plot (before normalization).pdf"))
      MASS::truehist(SelectedTraits[, base::match(i, COLN)],
                     nbins = base::as.numeric(input$nbin),
                     xlab = i)
      base::invisible(grDevices::dev.off())

      (arcsinh_obj <-
        bestNormalize::arcsinh_x(SelectedTraits[, base::match(i, COLN)]))
      (boxcox_obj <- bestNormalize::boxcox(SelectedTraits[, base::match(i, COLN)]))
      (yeojohnson_obj <-
        bestNormalize::yeojohnson(SelectedTraits[, base::match(i, COLN)]))
      (orderNorm_obj <-
        bestNormalize::orderNorm(SelectedTraits[, base::match(i, COLN)]))
      (BNobject <-
        bestNormalize::bestNormalize(SelectedTraits[, base::match(i, COLN)]))
      (binarize_obj <-
        bestNormalize::binarize(SelectedTraits[, base::match(i, COLN)]))
      xx <-
        base::seq(base::min(SelectedTraits[, base::match(i, COLN)]), base::max(SelectedTraits[, base::match(i, COLN)]))

      grDevices::png(file = base::paste0(i, "-normality approaches.png"))
      base::plot(
        xx,
        stats::predict(arcsinh_obj, newdata = xx),
        type = "l",
        col = 1,
        ylim = base::c(-4, 4),
        xlab = 'x',
        ylab = "g(x)"
      )
      graphics::lines(xx, stats::predict(boxcox_obj, newdata = xx), col = 2)
      graphics::lines(xx, stats::predict(yeojohnson_obj, newdata = xx), col = 3)
      graphics::lines(xx, stats::predict(orderNorm_obj, newdata = xx), col = 4)

      graphics::legend(
        "bottomright",
        legend = base::c("arcsinh", "Box Cox", "Yeo-Johnson", "OrderNorm"),
        col = 1:4,
        lty = 1,
        bty = 'n'
      )
      base::invisible(grDevices::dev.off())
      grDevices::pdf(file = base::paste0(i, "-normality approaches.pdf"))
      base::plot(
        xx,
        stats::predict(arcsinh_obj, newdata = xx),
        type = "l",
        col = 1,
        ylim = base::c(-4, 4),
        xlab = 'x',
        ylab = "g(x)"
      )
      graphics::lines(xx, stats::predict(boxcox_obj, newdata = xx), col = 2)
      graphics::lines(xx, stats::predict(yeojohnson_obj, newdata = xx), col = 3)
      graphics::lines(xx, stats::predict(orderNorm_obj, newdata = xx), col = 4)
      graphics::legend(
        "bottomright",
        legend = base::c("arcsinh", "Box Cox", "Yeo-Johnson", "OrderNorm"),
        col = 1:4,
        lty = 1,
        bty = 'n'
      )
      base::invisible(grDevices::dev.off())
      grDevices::png(file = base::paste0(i, "-normality plot (after normalization).png"))
      graphics::par(mfrow = base::c(2, 2))
      MASS::truehist(arcsinh_obj$x.t, main = "Arcsinh transformation", nbins = base::as.numeric(input$nbin))
      MASS::truehist(boxcox_obj$x.t, main = "Box Cox transformation", nbins = base::as.numeric(input$nbin))
      MASS::truehist(yeojohnson_obj$x.t, main = "Yeo-Johnson transformation", nbins = base::as.numeric(input$nbin))
      MASS::truehist(orderNorm_obj$x.t, main = "orderNorm transformation", nbins = base::as.numeric(input$nbin))
      base::invisible(grDevices::dev.off())
      grDevices::pdf(file = base::paste0(i, "-normality plot (after normalization).pdf"))
      graphics::par(mfrow = base::c(2, 2))
      MASS::truehist(arcsinh_obj$x.t, main = "Arcsinh transformation", nbins = base::as.numeric(input$nbin))
      MASS::truehist(boxcox_obj$x.t, main = "Box Cox transformation", nbins = base::as.numeric(input$nbin))
      MASS::truehist(yeojohnson_obj$x.t, main = "Yeo-Johnson transformation", nbins = base::as.numeric(input$nbin))
      MASS::truehist(orderNorm_obj$x.t, main = "orderNorm transformation", nbins = base::as.numeric(input$nbin))
      base::invisible(grDevices::dev.off())


      grDevices::png(file = base::paste0(i, "-normality plot (best normalization approach).png"))
      graphics::par(mfrow = base::c(1, 2))
      MASS::truehist(
        BNobject$x.t,
        main = base::paste(
          "Best Transformation:",
          base::class(BNobject$chosen_transform)[1]
        ),
        nbins = base::as.numeric(input$nbin)
      )
      base::plot(
        xx,
        stats::predict(BNobject, newdata = xx),
        type = "l",
        col = 1,
        main = "Best Normalizing transformation",
        ylab = "g(x)",
        xlab = "x"
      )
      base::invisible(grDevices::dev.off())
      grDevices::pdf(file = base::paste0(i, "-normality plot (best normalization approach).pdf"))
      graphics::par(mfrow = base::c(1, 2))
      MASS::truehist(
        BNobject$x.t,
        main = base::paste(
          "Best Transformation:",
          base::class(BNobject$chosen_transform)[1]
        ),
        nbins = base::as.numeric(input$nbin)
      )
      base::plot(
        xx,
        stats::predict(BNobject, newdata = xx),
        type = "l",
        col = 1,
        main = "Best Normalizing transformation",
        ylab = "g(x)",
        xlab = "x"
      )
      base::invisible(grDevices::dev.off())
      grDevices::png(
        file = base::paste0(i, "-normality approaches (box plot).png"),
        width = 1000,
        height = 480
      )
      graphics::boxplot(base::log10(BNobject$oos_preds),
              yaxt = 'n',
              xlab = "Normalization test")
      graphics::axis(2,
           at = base::log10(base::c(.1, .5, 1, 2, 5, 10)),
           labels = base::c(.1, .5, 1, 2, 5, 10))
      base::invisible(grDevices::dev.off())
      grDevices::pdf(file = base::paste0(i, "-normality approaches (box plot).pdf"))
      graphics::boxplot(base::log10(BNobject$oos_preds),
              yaxt = 'n',
              xlab = "Normalization test")
      graphics::axis(2,
           at = base::log10(base::c(.1, .5, 1, 2, 5, 10)),
           labels = base::c(.1, .5, 1, 2, 5, 10))
      base::invisible(grDevices::dev.off())
      grDevices::png(file = base::paste0(i, "-normality quantiles (before).png"))
      car::qqPlot(SelectedTraits[, base::match(i, COLN)])
      base::invisible(grDevices::dev.off())
      grDevices::pdf(file = base::paste0(i, "-normality quantiles (before).pdf"))
      car::qqPlot(SelectedTraits[, base::match(i, COLN)])
      base::invisible(grDevices::dev.off())
      grDevices::png(file = base::paste0(i, "-normality quantiles (after).png"))
      car::qqPlot(BNobject$x.t)
      base::invisible(grDevices::dev.off())
      grDevices::pdf(file = base::paste0(i, "-normality quantiles (after).pdf"))
      car::qqPlot(BNobject$x.t)
      base::invisible(grDevices::dev.off())
      A <- ggpubr::ggqqplot(SelectedTraits[, base::match(i, COLN)])
      ggplot2::ggsave(
        A,
        file = base::paste0(i, "-B-normality quantiles (before).png"),
        width = 32,
        height = 15,
        units = "cm"
      )
      A <- ggpubr::ggqqplot(BNobject$x.t)
      ggplot2::ggsave(
        A,
        file = base::paste0(i, "-B-normality quantiles (after).png"),
        width = 32,
        height = 15,
        units = "cm"
      )
      A <- ggpubr::ggdensity(SelectedTraits[, base::match(i, COLN)],
                     main = "Density plot (Original data)",
                     xlab = i)
      ggplot2::ggsave(
        A,
        file = base::paste0(i, "-Density plot (Original data).png"),
        width = 32,
        height = 15,
        units = "cm"
      )
      A <- ggpubr::ggdensity(BNobject$x.t,
                     main = "Density plot (Normalized data)",
                     xlab = i)
      ggplot2::ggsave(
        A,
        file = base::paste0(i, "-Density plot (Normalized data).png"),
        width = 32,
        height = 15,
        units = "cm"
      )

      A <- ggpubr::ggqqplot(SelectedTraits[, base::match(i, COLN)])
      ggplot2::ggsave(
        A,
        file = base::paste0(i, "-B-normality quantiles (before).pdf"),
        width = 32,
        height = 15,
        units = "cm"
      )
      A <- ggpubr::ggqqplot(BNobject$x.t)
      ggplot2::ggsave(
        A,
        file = base::paste0(i, "-B-normality quantiles (after).pdf"),
        width = 32,
        height = 15,
        units = "cm"
      )
      A <- ggpubr::ggdensity(SelectedTraits[, base::match(i, COLN)],
                     main = "Density plot (Original data)",
                     xlab = i)
      ggplot2::ggsave(
        A,
        file = base::paste0(i, "-Density plot (Original data).pdf"),
        width = 32,
        height = 15,
        units = "cm"
      )
      A <- ggpubr::ggdensity(BNobject$x.t,
                     main = "Density plot (Normalized data)",
                     xlab = i)
      ggplot2::ggsave(
        A,
        file = base::paste0(i, "-Density plot (Normalized data).pdf"),
        width = 32,
        height = 15,
        units = "cm"
      )
      SelectedTraits[, base::match(i, COLN)] <- BNobject$x.t
      FinalDataset <- base::cbind(VarPYSL, SelectedTraits)
      utils::write.csv(
        FinalDataset,
        file = base::paste0(input$project_name, " - Normalized Dataset.csv"),
        row.names = FALSE
      )
    }
  }

  set_wd('Normalization', rv)
}