#' Mixed analysis
#'
#' @description
#'
#' @param input object
#' @param rv object
#'
#' @noRd

Mixed_Analysis <- function(input, rv) {

  set_wd('Mixed Analysis')

  random_part = fix_part = T
  if(is.null(input$blue_rand)){
    random_part = F
  }
  if(is.null(input$blue_fix)){
    fix_part = F
  }
  if(random_part & fix_part)
    type = 'Mix'
  else if (!random_part &  fix_part)
    type = 'Fix'
  else if (random_part & !fix_part)
    type = 'Rand'

  if (type == 'Mix') {
    response = input$blue_resp
    Randv = input$blue_rand
    Cof = input$blue_cof

    data <- rv$data %>% dplyr::mutate_at(dplyr::vars(input$main_db_indep_val), as.factor)

    if (!base::is.null(rv$spat_buffer))
      if (input$use_spat)
        data <- rv$spat_buffer %>% dplyr::mutate_at(dplyr::vars(input$main_db_indep_val), as.factor)

   fix.F <- stats::as.formula(base::paste(response, base::paste(input$mix_intercept, "+", base::paste(input$rv_blue_rand, "+", input$rv_blue_fix), collapse = "+"), sep = "~"))


    if (base::is.null(Cof)) {
      Model <- base::eval(base::bquote(lme4::lmer(.(fix.F), data = data)))
    } else {
      Model <- base::eval(base::bquote(lme4::lmer(.(fix.F), weights = base::get(Cof), data = data)))
    }

    fixed_variables = base::unique(base::c(input$blue_fix, input$blue_fix_interact))

    for (each_NameG in fixed_variables) {
      ._BLUE <- Model %>%
        lme4::fixef() %>%
        base::data.frame() %>%
        tibble::rownames_to_column({ { each_NameG } }) %>%
        dplyr::rename(!!response := .) %>%
        dplyr::mutate(across({ { response } }, as.numeric)) %>%
        dplyr::filter(grepl({ { each_NameG } }, .data[[each_NameG]])) %>%
        dplyr::mutate(across({ { each_NameG } }, ~stringr::str_replace(., each_NameG, "")))

      utils::write.csv(._BLUE,
                       file = base::paste0(input$project_name, " -- ", each_NameG, " based BLUE value for ", response, ".csv"), row.names = FALSE)

    }

    A <- base::as.data.frame(Model@resp$y)

    p <- ggplot2::ggplot(A, ggplot2::aes(x = Model@resp$y)) +
      ggplot2::geom_histogram(color = "black", fill = "white") +
      ggplot2::theme_classic() +
      ggplot2::labs(title = "Raw Value") +
      ggplot2::xlab("Response Varibale")

    ggplot2::ggsave(
      p,
      file = base::paste0(input$project_name, " -- Raw Value ( ", response, " ).png"),
      width = 32,
      height = 15,
      units = "cm"
    )
    ggplot2::ggsave(
      p,
      file = base::paste0(input$project_name, " -- Raw Value ( ", response, " ).pdf"),
      width = 32,
      height = 15,
      units = "cm"
    )

    if (base::is.null(Cof)) {
      MM.S <- base::eval(base::bquote(lmerTest::lmer(.(fix.F), data = data)))
    } else {
      MM.S <- base::eval(base::bquote(lmerTest::lmer(.(fix.F), weights = base::get(Cof), data = data)))
    }

    b <- lmerTest::ranova(MM.S)
    bb <- as.data.frame(b)
    bb = bb[-1,]
    new_rownames = NULL
    for (i in rownames(bb)) {
      str = base::strsplit(i, '')[[1]][-c(2, 3, 4, 5)]
      buf = ''
      for (j in str)
        buf = paste0(buf, j)
      new_rownames = c(new_rownames, buf)
    }
    rownames(bb) = new_rownames
    utils::write.csv(bb, paste0(input$project_name, '-- Anova (random effects) Table.csv'))

    a <- stats::anova(MM.S)
    aa <- as.data.frame(a)

    utils::write.csv(aa, paste0(input$project_name, '-- Anova table (fixed effects).csv'))

    base::sink("Summary of Model.txt")
    base::print(base::summary(MM.S))
    base::sink()

    MM.Sresid <- stats::residuals(MM.S, type = "pearson")
    MM.Sactual <- stats::predict(MM.S)
    utils::write.csv(MM.Sactual, paste0(input$project_name, '-- Predicted Values.csv'))
    # create plot

    p <- function() {
      base::plot(
        MM.Sactual,
        MM.Sresid,
        xlab = "Predicted data",
        ylab = "Residual data",
        main = "Predicted vs Residual"
      )
    }

    filesave('png', input$project_name, ' -- Predicted vs Residual', callback = p, rv)
    filesave('pdf', input$project_name, ' -- Predicted vs Residual', callback = p, rv)

    p <- function() {
      car::qqPlot(MM.Sresid)
    }

    filesave('png', input$project_name, ' -- Quantile Normality', callback = p, rv)
    filesave('pdf', input$project_name, ' -- Quantile Normality', callback = p, rv)

    shapiro.test <- stats::shapiro.test(MM.Sresid)
    buffer <- base::as.data.frame(base::cbind(shapiro.test$statistic, shapiro.test$p.value))
    base::colnames(buffer) <- base::c("statistic (W)", "p.value")
    base::rownames(buffer) <- "Shapiro-Wilk normality test"
    utils::write.csv(buffer, paste0(input$project_name, ' -- Shapiro-Wilk normality test.csv'))

  }

  else if (type == 'Rand') {
    formula_str = input$rv_blue_rand
    random_var = input$blue_rand
    response = input$blue_resp
    Cof = input$blue_cof
    project_name = input$project_name
    intercep = input$mix_intercept

    data_buf = dplyr::select(rv$data, dplyr::all_of(c(input$main_db_indep_val, response, Cof)))

    if (base::is.null(Cof))
      B2 <- lme4::lmer(formula = base::paste0(response, ' ~ ', intercep, ' + ', formula_str), data = stats::na.omit(data_buf))
    else {
      base::colnames(data_buf)[[base::which(base::colnames(data_buf) == Cof)]] <- 'Cof'
      B2 <- lme4::lmer(formula = base::paste0(response, ' ~ ', intercep, ' + ', formula_str), data = stats::na.omit(data_buf), weights = Cof)
    }
    for (indep in random_var) {
      A2 <- base::list()
      A2[[response]] = stats::coef(B2)[[indep]]
      base::colnames(A2[[response]]) <- response
      A2 <- base::as.data.frame(A2)
      A2[[indep]] <- base::row.names.data.frame(A2)
      buf = A2[[1]]
      A2[[1]] = A2[[2]]
      A2[[2]] = buf
      buf = base::colnames(A2)[[1]]
      base::colnames(A2)[[1]] = base::colnames(A2)[[2]]
      base::colnames(A2)[[2]] = buf

      utils::write.csv(
        A2,
        base::paste0(project_name, ' -- ', response,'BLUP value (', indep, ').csv'),
        row.names = FALSE)
    }

    dat_long <- rv$data %>% tidyr::gather(key = 'DTriats', value = 'Valuee', response)

    form <- stats::formula(base::paste0('Valuee ~ ', formula_str))

    vc_lmer <- function(x, reml_arg = TRUE) {
      df <- base::data.frame(x) %>%
        dplyr::do(base::as.data.frame(lme4::VarCorr(lme4::lmer(data = ., form, REML = reml_arg)))) %>%
        dplyr::select(-c(var1, var2)) %>%
        dplyr::rename(Variance = vcov, Std.Dev = sdcor) %>%
        dplyr::mutate(VarSum = base::sum(Variance), Prop.Var.Comp = base::round((Variance / VarSum * 100), 5))
      base::return(df)
    }

    res <- dat_long %>%
      dplyr::group_by(DTriats) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        vc.ge = purrr::map(data, ~vc_lmer(.x))
      )

    p1 <- function() {
      var <- res %>%
        tidyr::unnest(vc.ge) %>%
        ggplot2::ggplot(ggplot2::aes(x = DTriats, y = Prop.Var.Comp, fill = grp)) +
        ggplot2::geom_col(colour = "black", alpha = 0.8) +
        ggplot2::scale_fill_brewer(palette = "Dark2") +
        ggplot2::labs(x = "Dependant Variables", y = "Proportion of Variation") +
        ggplot2::theme_classic() +
        ggplot2::guides(fill = ggplot2::guide_legend(title = "Source of Variation"))
      base::invisible(base::print(var))
    }

    filesave('png', project_name, ' -- Variance Porportion', p1, rv)
    filesave('pdf', project_name, ' -- Variance Porportion', p1, rv)

    utils::write.csv(
      base::as.data.frame(res$vc.ge),
      base::paste0(project_name, " -- Variance poportaion for ", response, ".csv"),
      row.names = FALSE)
  }

  else if (type == 'Fix') {
    response = input$blue_resp
    Cof = input$blue_cof

    data <- rv$data %>% dplyr::mutate_at(dplyr::vars(input$main_db_indep_val), as.factor)
    if (!base::is.null(rv$spat_buffer))
      if (input$use_spat)
        data <- rv$spat_buffer %>% dplyr::mutate_at(dplyr::vars(input$main_db_indep_val), as.factor)


    fix.F <-
      stats::as.formula(
        base::paste(
          response,
          base::paste(
            input$mix_intercept,
            "+",
            input$rv_blue_fix,
            collapse = "+"),
          sep = "~"))


    if (base::is.null(Cof)) {
      Model <- base::eval(base::bquote(stats::lm(.(fix.F), data = data)))
    } else {
      Model <- base::eval(base::bquote(stats::lm(.(fix.F), weights = base::get(Cof), data = data)))
    }
    MM.S = Model

    a <- stats::anova(MM.S)
    aa <- as.data.frame(a)

    utils::write.csv(aa, paste0(input$project_name, '-- Anova table (fixed effects).csv'))

    base::sink("Summary of Model.txt")
    base::print(base::summary(MM.S))
    base::sink()

    MM.Sresid <- stats::residuals(MM.S, type = "pearson")
    MM.Sactual <- stats::predict(MM.S)
    utils::write.csv(MM.Sactual, paste0(input$project_name, '-- Predicted Values.csv'), row.names = F)
    # create plot

    p <- function() {
      base::plot(
        MM.Sactual,
        MM.Sresid,
        xlab = "Predicted data",
        ylab = "Residual data",
        main = "Predicted vs Residual"
      )
    }

    filesave('png', input$project_name, ' -- Predicted vs Residual', callback = p, rv)
    filesave('pdf', input$project_name, ' -- Predicted vs Residual', callback = p, rv)

    p <- function() {
      car::qqPlot(MM.Sresid)
    }

    filesave('png', input$project_name, ' -- Quantile Normality', callback = p, rv)
    filesave('pdf', input$project_name, ' -- Quantile Normality', callback = p, rv)

    shapiro.test <- stats::shapiro.test(MM.Sresid)
    buffer <- base::as.data.frame(base::cbind(shapiro.test$statistic, shapiro.test$p.value))
    base::colnames(buffer) <- base::c("statistic (W)", "p.value")
    base::rownames(buffer) <- "Shapiro-Wilk normality test"
    utils::write.csv(buffer, paste0(input$project_name, ' -- Shapiro-Wilk normality test.csv'))
  }

  set_wd('Mixed Analysis', rv, input$save_results)
}
