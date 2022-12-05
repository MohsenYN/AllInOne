#' General, Inter, and Intra Correlation
#'
#' @description Generates 1) general correlation based on the selected dependent variables, 2) inter correlation for the selected independent and dependent variable, and 3) Intra correlation between two dependent variables in selected independent variable.
#'
#' @param input object including user's input values
#' @param rv object including reactive variables
#'
#' @noRd
CoReLaTiOnSS <- function(input, rv) {

  if (!require(corrplot))
    utils::install.packages("corrplot")
  if (!require(RColorBrewer))
    utils::install.packages("RColorBrewer")
  set_wd('Correlation')


  # include Independent variables TOO
  rv$independent_variables <-
    rv$data %>% dplyr::select(input$main_db_indep_val)

  # include Dependent variables TOO
  rv$dependent_variables <-
      rv$data %>% dplyr::select(input$main_db_dep_val)

  dat <- stats::na.omit(rv$data)

  SelectedTraits <- stats::na.omit(rv$dependent_variables)
  SelectedTraits <-
    SelectedTraits %>% dplyr::select(where(is.numeric))

  COL_INDEP = input$indep_cor
  COL_DEP = input$dep_cor
  COLN = base::colnames(SelectedTraits)

  colors <- grDevices::colorRampPalette(rv$setting_colors)
  colors = colors(rv$setting_general_cnum)
  if (input$cor_opt == 'Non-independent-based correlation') {
    cor_ <- Hmisc::rcorr(as.matrix(SelectedTraits))
    M <- cor_$r
    PV <- cor_$P

    p <- function()
      corrplot::corrplot(PV, method = "pie", col = colors)

    filesave("png",
             input$project_name,
             " -- P-value Correlation (pie)",
             p, rv)
    filesave("pdf",
             input$project_name,
             " -- P-value Correlation (pie)",
             p, rv)
    utils::write.csv(M, base::paste0(input$project_name, ' -- Correlation Table.csv'), row.names = F)
    utils::write.csv(cor_$P, base::paste0(input$project_name, ' -- P-value.csv'), row.names = F)
  }
  else if (input$cor_opt == 'Inter correlation') {
    y = COL_INDEP
    Independent <- dat %>% dplyr::mutate_at(dplyr::vars(y), as.factor)

    A <- base::list()
    B <- base::list()

    for (i in base::levels(Independent[[y]])) {
      for (j in COL_DEP) {
        A[[i]] <- base::subset(Independent, base::get(y) == i)
        B[[i]] <- A[[i]] %>% dplyr::select(dplyr::all_of(j))
        base::colnames(B[[i]]) <- i
      }
    }

    C <- base::lapply(B, function(x)
      x[!base::is.na(x)])
    D <- base::do.call(cbind, base::lapply(C, 'length<-', base::max(base::lengths(C))))
    D <- base::as.data.frame(D)
    rv$cor_temp = D

    M <- stats::cor(D, use = "pairwise.complete.obs")

    utils::write.csv(D, base::paste0(input$project_name,' -- Observed Value Table.csv'), row.names = F)

    utils::write.csv(M, base::paste0(input$project_name,' -- Correlation Table.csv'), row.names = F)


  }
  else if (input$cor_opt == 'Intra correlation') {
    FTrait = input$dep_cor[1]
    STrait = input$dep_cor[2]

    temp = dat %>% tibble::add_column(interacted = 'a')

    for (i in base::seq(1:base::length(temp[[1]]))) {
      temp[i, 'interacted'] = ""
      counter = 0
      for (k in input$indep_cor) {
        counter = counter + 1
        j = base::which(base::names(dat) == dplyr::all_of(k))
        temp[i, 'interacted'] = base::paste0(temp[i, 'interacted'], base::ifelse(counter == 1, '', '-'), dat[i, j])
      }
    }
    dat = temp
    MainV = "interacted"
    MainVb <- base::as.data.frame(base::unique(dat[, MainV]))
    result = NULL
    for (j in 1:base::nrow(MainVb)) {
      A <- dat %>% filter(base::get(MainV) %in% MainVb[j,])
      ._CORR <- stats::cor(A[FTrait], A[STrait])
      B <- ._CORR
      C <- base::as.character(MainVb[j,])
      res = base::list(C, B)
      base::names(res) <- base::c(MainV, STrait)

      res = base::do.call(cbind.data.frame, res)
      result <- base::rbind(result, res)
    }
    base::row.names(result) = 1:base::length(base::row.names(result))
    base::colnames(result) = base::c(MainV, 'Correlation')

    buffer = result
    colnames(buffer)[1] = input$indep_cor
    utils::write.csv(buffer, base::paste0(input$project_name,' -- Correlation Table.csv'), row.names = F)

    Cordata <- base::as.data.frame(result)

    red.bold.italic.text <- ggplot2::element_text(face = "bold.italic", color = "Black")
    is.num <- base::sapply(Cordata, is.numeric)
    Cordata[is.num] <- base::lapply(Cordata[is.num], round, 2)

    UOGAP <- Cordata %>%
      dplyr::mutate(name = forcats::fct_reorder(interacted, dplyr::desc(Correlation))) %>%
      ggplot2::ggplot(ggplot2::aes(x = name, y = Correlation, fill = name)) +
      ggplot2::geom_bar(stat = "identity", alpha = .6, width = .4) +
      ggplot2::coord_flip() +
      ggplot2::xlab("") +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none", axis.title = red.bold.italic.text) +
      ggplot2::scale_y_continuous(limits = base::c(-1, 1)) +
      ggplot2::labs(y = "Pearson correlation coefficient")+
      ggplot2::scale_fill_manual( values = colors )

    ggplot2::ggsave(
      UOGAP,
      file = base::paste0(input$project_name, " -- Pearson correlation coefficient (Barplot).png"),
      width = 32,
      height = 15,
      units = "cm"
    )
    ggplot2::ggsave(
      UOGAP,
      file = base::paste0(input$project_name, " -- Pearson correlation coefficient (Barplot).pdf"),
      width = 32,
      height = 15,
      units = "cm"
    )

    UOGAP <- Cordata %>%
      dplyr::mutate(name = forcats::fct_reorder(interacted, dplyr::desc(Correlation))) %>%
      ggplot2::ggplot(ggplot2::aes(x = name, y = Correlation, fill = name)) +
      ggplot2::geom_bar(stat = "identity", alpha = .001, width = .4) +
      ggplot2::coord_flip() +
      ggplot2::xlab("") +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none", axis.title = red.bold.italic.text) +
      ggplot2::scale_y_continuous(limits = base::c(-1, 1)) +
      ggplot2::geom_text(ggplot2::aes(label = Correlation, y = Correlation), size = 5) +
      ggplot2::labs(y = "Pearson correlation coefficient")

    ggplot2::ggsave(
      UOGAP,
      file = base::paste0(input$project_name, " -- Pearson correlation coefficient (Value).png"),
      width = 32,
      height = 15,
      units = "cm"
    )
    ggplot2::ggsave(
      UOGAP,
      file = base::paste0(input$project_name, " -- Pearson correlation coefficient (Value).pdf"),
      width = 32,
      height = 15,
      units = "cm"
    )


    UOGAP <- Cordata %>%
      dplyr::arrange(Correlation) %>%
      dplyr::mutate(name = forcats::fct_reorder(interacted, dplyr::desc(Correlation))) %>%
      ggplot2::ggplot(ggplot2::aes(x = name, y = Correlation)) +
      ggplot2::geom_segment(ggplot2::aes(xend = name, yend = 0), alpha = .6) +
      ggplot2::geom_point(size = 4, color = "orange") +
      ggplot2::theme_classic() +
      ggplot2::coord_flip() +
      ggplot2::theme(legend.position = "none", axis.title = red.bold.italic.text) +
      ggplot2::scale_y_continuous(limits = base::c(-1, 1)) +
      ggplot2::labs(y = "Pearson correlation coefficient")

    ggplot2::ggsave(
      UOGAP,
      file = base::paste0(input$project_name, " -- Pearson correlation coefficient (Lollipop plot).png"),
      width = 32,
      height = 15,
      units = "cm"
    )
    ggplot2::ggsave(
      UOGAP,
      file = base::paste0(input$project_name, " -- Pearson correlation coefficient (Lollipop plot).pdf"),
      width = 32,
      height = 15,
      units = "cm"
    )


    UOGAP <- Cordata %>%
      dplyr::arrange(Correlation) %>%
      dplyr::mutate(name = forcats::fct_reorder(interacted, dplyr::desc(Correlation))) %>%
      ggplot2::ggplot(ggplot2::aes(x = name, y = Correlation)) +
      ggplot2::geom_segment(ggplot2::aes(xend = name, yend = 0), alpha = .01) +
      ggplot2::geom_point(size = 4, color = "orange") +
      ggplot2::theme_classic() +
      ggplot2::coord_flip() +
      ggplot2::theme(legend.position = "none", axis.title = red.bold.italic.text) +
      ggplot2::scale_y_continuous(limits = base::c(-1, 1)) +
      ggplot2::geom_text(ggplot2::aes(label = Correlation, y = Correlation), size = 5) +
      ggplot2::labs(y = "Pearson correlation coefficient")

    ggplot2::ggsave(
      UOGAP,
      file = base::paste0(input$project_name, " -- Pearson correlation coefficient (Lollipop - Value).png"),
      width = 32,
      height = 15,
      units = "cm"
    )
    ggplot2::ggsave(
      UOGAP,
      file = base::paste0(input$project_name, " -- Pearson correlation coefficient (Lollipop - Value).pdf"),
      width = 32,
      height = 15,
      units = "cm"
    )
  }

  if (input$cor_opt != 'Intra correlation') {
    if ('circle' %in% rv$setting_cor_plot) {
    grDevices::png(
      file = base::paste0(input$project_name, " -- Correlation (circle).png"),
      width = 1000,
      height = 480
    )
    corrplot::corrplot(M, method = "circle", col = colors)
    base::invisible(grDevices::dev.off())
    grDevices::pdf(file = base::paste0(input$project_name, " -- Correlation (circle).pdf"))
    P <- corrplot::corrplot(M, method = "circle", col = colors)
    base::invisible(base::print(P))
    base::invisible(grDevices::dev.off())

    }

    if ('pie' %in% rv$setting_cor_plot) {
    grDevices::png(
      file = base::paste0(input$project_name, " -- Correlation (pie).png"),
      width = 1000,
      height = 480
    )
    corrplot::corrplot(M, method = "pie", col = colors)
    base::invisible(grDevices::dev.off())
    grDevices::pdf(file = base::paste0(input$project_name, " -- Correlation (pie).pdf"))
    P <- corrplot::corrplot(M, method = "pie", col = colors)
    base::invisible(base::print(P))
    base::invisible(grDevices::dev.off())
    }
    if ('color' %in% rv$setting_cor_plot) {
    grDevices::png(
      file = base::paste0(input$project_name, " -- Correlation (color).png"),
      width = 1000,
      height = 480
    )
    corrplot::corrplot(M, method = "color", col = colors)
    base::invisible(grDevices::dev.off())
    grDevices::pdf(file = base::paste0(input$project_name, " -- Correlation (color).pdf"))
    P <- corrplot::corrplot(M, method = "color", col = colors)
    base::invisible(base::print(P))
    base::invisible(grDevices::dev.off())
    }

    if ('number' %in% rv$setting_cor_plot) {
    grDevices::png(
      file = base::paste0(input$project_name, " -- Correlation (number).png"),
      width = 1000,
      height = 480
    )
    corrplot::corrplot(M, method = "number", col = colors)
    base::invisible(grDevices::dev.off())
    grDevices::pdf(file = base::paste0(input$project_name, " -- Correlation (number).pdf"))
    P <- corrplot::corrplot(M, method = "number", col = colors)
    base::invisible(base::print(P))
    base::invisible(grDevices::dev.off())
    }
    if ('upper' %in% rv$setting_cor_plot) {
    grDevices::png(
      file = base::paste0(input$project_name, " -- Correlation (upper).png"),
      width = 1000,
      height = 480
    )
    corrplot::corrplot(M, type = "upper", col = colors)
    base::invisible(grDevices::dev.off())
    grDevices::pdf(file = base::paste0(input$project_name, " -- Correlation (upper).pdf"))
    P <- corrplot::corrplot(M, type = "upper", col = colors)
    base::invisible(base::print(P))
    base::invisible(grDevices::dev.off())
    }
    if ('lower' %in% rv$setting_cor_plot) {
    grDevices::png(
      file = base::paste0(input$project_name, " -- Correlation (lower).png"),
      width = 1000,
      height = 480
    )
    corrplot::corrplot(M, type = "lower", col = colors)
    base::invisible(grDevices::dev.off())
    grDevices::pdf(file = base::paste0(input$project_name, " -- Correlation (lower).pdf"))
    P <- corrplot::corrplot(M, type = "lower", col = colors)
    base::invisible(base::print(P))
    base::invisible(grDevices::dev.off())

    }

    if (base::length(COLN) >= 2) {
      if ('hclust' %in% rv$setting_cor_plot) {
      grDevices::png(
        file = base::paste0(input$project_name, " -- Correlation (hclust).png"),
        width = 1000,
        height = 480
      )

      corrplot::corrplot(M, type = "upper", order = "hclust", col = colors)

      base::invisible(grDevices::dev.off())
      grDevices::pdf(file = base::paste0(input$project_name, " -- Correlation (hclust).pdf"))
      P <- corrplot::corrplot(M, type = "upper", order = "hclust", col = colors)
      base::invisible(base::print(P))
      base::invisible(grDevices::dev.off())

    }
    if ('br' %in% rv$setting_cor_plot) {
      col <- colorRampPalette(base::c("red", "white", "blue"))(20)
      grDevices::png(
        file = base::paste0(
          input$project_name,
          " -- Correlation (upper and hclust (BR)).png"
        ),
        width = 1000,
        height = 480
      )
      corrplot::corrplot(M,
                         type = "upper",
                         order = "hclust", col = colors)
      base::invisible(grDevices::dev.off())

      grDevices::pdf(file = base::paste0(
        input$project_name,
        " -- Correlation (upper and hclust (BR)).pdf"
      ))
      P <- corrplot::corrplot(M,
                              type = "upper",
                              order = "hclust", col = colors)
      base::invisible(base::print(P))
      base::invisible(grDevices::dev.off())
    }
    if ('bw' %in% rv$setting_cor_plot) {
      grDevices::png(
        file = base::paste0(
          input$project_name,
          " -- Correlation (upper and hclust (BW)).png"
        ),
        width = 1000,
        height = 480
      )
      corrplot::corrplot(
        M,
        type = "upper",
        order = "hclust",
        col = colors,
        bg = "lightblue"
      )
      base::invisible(grDevices::dev.off())
      grDevices::pdf(file = base::paste0(
        input$project_name,
        " -- Correlation (upper and hclust (BW)).pdf"
      ))
      P <-
        corrplot::corrplot(
          M,
          type = "upper",
          order = "hclust", col = colors,
          bg = "lightblue"
        )
      base::invisible(base::print(P))
      base::invisible(grDevices::dev.off())
    }
      if ('cola' %in% rv$setting_cor_plot) {
      grDevices::png(
        file = base::paste0(
          input$project_name,
          " -- Correlation (upper and hclust (COLA)).png"
        ),
        width = 1000,
        height = 480
      )
      corrplot::corrplot(
        M,
        type = "upper",
        order = "hclust",
        col = colors
      )
      base::invisible(grDevices::dev.off())
      grDevices::pdf(file = base::paste0(
        input$project_name,
        " -- Correlation (upper and hclust (COLA)).pdf"
      ))
      P <- corrplot::corrplot(
        M,
        type = "upper",
        order = "hclust",
        col = colors
      )
      base::invisible(base::print(P))
      base::invisible(grDevices::dev.off())
    }
      if ('colb' %in% rv$setting_cor_plot) {
      grDevices::png(
        file = base::paste0(
          input$project_name,
          " -- Correlation (upper and hclust (COLB)).png"
        ),
        width = 1000,
        height = 480
      )
      corrplot::corrplot(
        M,
        type = "upper",
        order = "hclust",
        col = colors
      )
      base::invisible(grDevices::dev.off())
      grDevices::pdf(file = base::paste0(
        input$project_name,
        " -- Correlation (upper and hclust (COLB)).pdf"
      ))
      P <- corrplot::corrplot(
        M,
        type = "upper",
        order = "hclust",
        col = colors
      )
      base::invisible(base::print(P))
      base::invisible(grDevices::dev.off())
    }
      if ('axis' %in% rv$setting_cor_plot) {
      grDevices::png(
        file = base::paste0(
          input$project_name,
          " -- Correlation (upper and hclust (AXIS)).png"
        ),
        width = 1000,
        height = 480
      )
      corrplot::corrplot(
        M,
        type = "upper",
        order = "hclust",
        tl.col = "black",
        tl.srt = 45, col = colors
      )
      base::invisible(grDevices::dev.off())
      grDevices::pdf(file = base::paste0(
        input$project_name,
        " -- Correlation (upper and hclust (AXIS)).pdf"
      ))
      P <-
        corrplot::corrplot(
          M,
          type = "upper",
          order = "hclust",
          tl.col = "black",
          tl.srt = 45, col = colors
        )
      base::invisible(base::print(P))
      base::invisible(grDevices::dev.off())
    }

      Cor.mtest <- function(mat, ...) {
        mat <- base::as.matrix(mat)
        n <- base::ncol(mat)
        p.mat <- base::matrix(NA, n, n)
        base::diag(p.mat) <- 0
        for (i in 1:(n - 1)) {
          for (j in (i + 1):n) {
            tmp <- stats::cor.test(mat[, i], mat[, j], ...)
            p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
          }
        }
        base::colnames(p.mat) <- base::rownames(p.mat) <- base::colnames(mat)
        p.mat
      }
      if ('sig' %in% rv$setting_cor_plot) {
      base::tryCatch({
        # matrix of the p-value of the correlation
        p.mat <- Cor.mtest(SelectedTraits)
        grDevices::png(
          file = base::paste0(
            input$project_name,
            " -- Correlation (upper and hclust (SIG)).png"
          ),
          width = 1000,
          height = 480
        )
        corrplot::corrplot(
          M,
          type = "upper",
          order = "hclust",
          p.mat = p.mat,
          sig.level = 0.01, col = colors
        )
        base::invisible(grDevices::dev.off())
      }, error = function(e) {
        corrplot::corrplot(
          M,
          type = "upper",
          order = "hclust",
          sig.level = 0.01, col = colors
        )
        base::invisible(grDevices::dev.off())
      })

      base::tryCatch({
        grDevices::pdf(file = base::paste0(
          input$project_name,
          " -- Correlation (upper and hclust (SIG)).pdf"
        ))
        P <- corrplot::corrplot(
          M,
          type = "upper",
          order = "hclust",
          p.mat = p.mat,
          sig.level = 0.01, col = colors
        )
        base::invisible(base::print(P))
        base::invisible(grDevices::dev.off())

      }, error = function(e) {

        P <- corrplot::corrplot(
          M,
          type = "upper",
          order = "hclust",
          sig.level = 0.01, col = colors
        )
        base::invisible(base::print(P))
        base::invisible(grDevices::dev.off())
      })
    }
      if ('sigblank' %in% rv$setting_cor_plot) {
      base::tryCatch({
        grDevices::png(
          file = base::paste0(
            input$project_name,
            " -- Correlation (upper and hclust (SIGBLANK)).png"
          ),
          width = 1000,
          height = 480
        )
        corrplot::corrplot(
          M,
          type = "upper",
          order = "hclust",
          p.mat = p.mat,
          sig.level = 0.01,
          insig = "blank", col = colors
        )
        base::invisible(grDevices::dev.off())

      }, error = function(e) {

        corrplot::corrplot(
          M,
          type = "upper",
          order = "hclust",
          sig.level = 0.01,
          insig = "blank", col = colors
        )
        base::invisible(grDevices::dev.off())

      })

      base::tryCatch({
        grDevices::pdf(file = base::paste0(
          input$project_name,
          " -- Correlation (upper and hclust (SIGBLANK)).pdf"
        ))
        P <- corrplot::corrplot(
          M,
          type = "upper",
          order = "hclust",
          p.mat = p.mat,
          sig.level = 0.01,
          insig = "blank", col = colors
        )
        base::invisible(base::print(P))
        base::invisible(grDevices::dev.off())

      }, error = function(e) {
        P <- corrplot::corrplot(
          M,
          type = "upper",
          order = "hclust",
          sig.level = 0.01,
          insig = "blank", col = colors
        )
        base::invisible(base::print(P))
        base::invisible(grDevices::dev.off())
      })

    }
      if ('full' %in% rv$setting_cor_plot) {
      base::tryCatch({
        grDevices::png(
          file = base::paste0(input$project_name, " -- Correlation (Full).png"),
          width = 1000,
          height = 480
        )
        corrplot::corrplot(
          M,
          method = "color",
          col = colors,
          type = "upper",
          order = "hclust",
          addCoef.col = "black",
          tl.col = "black",
          tl.srt = 45,
          p.mat = p.mat,
          sig.level = 0.01,
          insig = "blank",
          diag = FALSE
        )
        base::invisible(grDevices::dev.off())

      }, error = function(e) {

        corrplot::corrplot(
          M,
          method = "color",
          col = colors,
          type = "upper",
          order = "hclust",
          addCoef.col = "black",
          tl.col = "black",
          tl.srt = 45,
          sig.level = 0.01,
          insig = "blank",
          diag = FALSE
        )
        base::invisible(grDevices::dev.off())

      })

      base::tryCatch({
        grDevices::pdf(file = base::paste0(input$project_name, " -- Correlation (Full).pdf"))
        P <- corrplot::corrplot(
          M,
          method = "color",
          col = colors,
          type = "upper",
          order = "hclust",
          addCoef.col = "black",
          tl.col = "black",
          tl.srt = 45,
          p.mat = p.mat,
          sig.level = 0.01,
          insig = "blank",
          diag = FALSE
        )
        base::invisible(base::print(P))
        base::invisible(grDevices::dev.off())
      }, error = function(e) {

        P <- corrplot::corrplot(
          M,
          method = "color",
          col = colors,
          type = "upper",
          order = "hclust",
          addCoef.col = "black",
          tl.col = "black",
          tl.srt = 45,
          sig.level = 0.01,
          insig = "blank",
          diag = FALSE
        )
        base::invisible(base::print(P))
        base::invisible(grDevices::dev.off())
      })
    }
       #trycatch
    }
  }

  set_wd('Correlation', rv, input$save_results)
}
