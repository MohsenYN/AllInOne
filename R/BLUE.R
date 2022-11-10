#' Best Linear Unbiased Estimator (BLUE)
#'
#' @description Calculation of best linear unbiased estimators based on random and fixed variables in a given dataset.
#'
#' @param input object
#' @param rv object
#'
#' @noRd

ExBLUE <- function(input, rv) {

  set_wd('Blue')

  response = input$blue_resp
  Randv = input$blue_rand
  Cof = input$blue_cof

  data <- rv$data %>% dplyr::mutate_at(dplyr::vars(input$main_db_indep_val), as.factor)
  if (!base::is.null(rv$spat_buffer))
    if (input$use_spat)
      data <- rv$spat_buffer %>% dplyr::mutate_at(dplyr::vars(input$main_db_indep_val), as.factor)

  if (!base::is.null(Randv)) {
    fix.F <- stats::as.formula(base::paste(response, base::paste("0+", base::paste(input$rv_blue_rand, "+", input$rv_blue_fix), collapse = "+"), sep = "~"))
  } else {
    fix.F <- stats::as.formula(base::paste(response, base::paste("0 + ", base::paste(input$rv_blue_fix), collapse = "+"), sep = " ~ "))
  }

  if (base::is.null(Cof)) {
    Model <- base::eval(base::bquote(lme4::lmer(.(fix.F), data = data)))
  } else {
    Model <- base::eval(base::bquote(lme4::lmer(.(fix.F), weights = base::get(Cof), data = data)))
  }

  fixed_variables = base::unique(base::c(input$blue_fix, input$blue_fix_interact))
  #in this case fixed value is extracted from checkboxes and are not editable by hand
  #if fixed value should be extracted from input$help_(rand / fix)_blue you should change this part

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

  rv$cor_temp = ._BLUE

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

  #####################################################################

  df = stats::anova(Model)
  df = base::as.data.frame(df)

  df[['VarName']] <- base::row.names.data.frame(df)
  buf = df[['VarName']]
  df[['VarName']] = df[[1]]
  df[[1]] = buf

  buf = base::colnames(df)[[1]]
  base::colnames(df)[[1]] = 'Variable Name'
  base::colnames(df)[[base::which(base::colnames(df) == 'VarName')]] = buf

  buf = df[['npar']]
  df[['npar']] = df[[2]]
  df[[2]] = buf

  buf = base::colnames(df)[[2]]
  base::colnames(df)[[2]] = 'Parameter Number'
  base::colnames(df)[[base::which(base::colnames(df) == 'npar')]] = buf

  utils::write.csv(df, 'Anova Table.csv', row.names = F)

  #####################################################################

  if (base::is.null(Cof)) {
    MM.S <- base::eval(base::bquote(lmerTest::lmer(.(fix.F), data = data)))
  } else {
    MM.S <- base::eval(base::bquote(lmerTest::lmer(.(fix.F), weights = base::get(Cof), data = data)))
  }

  # Sig. Level (Random)
  # Anova table (random effect)

  b <- lmerTest::ranova(MM.S)
  bb <- as.data.frame(b)

  utils::write.csv(bb, 'Anova (random) Table.csv')


  # Sig. Level (Fix)
  # Anova table (fixed effects)

  a <- stats::anova(MM.S)
  aa <- as.data.frame(a)

  utils::write.csv(aa, 'Anova table (fixed effects).csv')




  MM.Sresid <- stats::residuals(MM.S, type = "pearson")
  MM.Sactual <- stats::predict(MM.S)

  # create plot

  p <- function(){
    base::plot(
      MM.Sactual,
      MM.Sresid,
      xlab = "Predicted data",
      ylab = "Residual data",
      main = "Predicted vs Residual"
    )
  }
  filesave('png', input$project_name, 'Predicted vs Residual', callback = p)
  filesave('pdf', input$project_name, 'Predicted vs Residual', callback = p)

  p <- function(){
    car::qqPlot(MM.Sresid)
  }
  filesave('png', input$project_name, 'MM_Sresid', callback = p)
  filesave('pdf', input$project_name, 'MM_Sresid', callback = p)

  shapiro.test <- stats::shapiro.test(MM.Sresid)
  buffer <- base::as.data.frame(base::cbind(shapiro.test$statistic, shapiro.test$p.value))
  base::colnames(buffer) <- base::c("statistic (W)", "p.value")
  base::rownames(buffer) <- "Shapiro-Wilk normality test"
  utils::write.csv(buffer, 'Shapiro-Wilk normality test.csv')

  set_wd('Blue', 'out')
}
