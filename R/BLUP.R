#' Best Linear Unbiased Prediction (BLUP)
#'
#' @description Calculation of best linear unbiased prediction under the random mixed model using REML.
#'
#' @param input object
#' @param rv object
#'
#' @noRd
BLUP <- function(input, rv) {

  set_wd('Blup')

  formula_str = input$rv_her
  A2 <- base::list()
  # varComp <- base::list()
  i = input$blup_resp

  if (base::is.null(input$blup_cof))
    B2 <- lme4::lmer(formula = base::paste0(i, ' ~ ', formula_str), data = stats::na.omit(rv$data))
  else{
    data_buf = rv$data
    base::colnames(data_buf)[[base::which(base::colnames(data_buf) == input$blup_cof)]] <- 'Cof'
    B2 <- lme4::lmer(formula = base::paste0(i, ' ~ ', formula_str), data = stats::na.omit(data_buf), weights = Cof)
  }

  # varComp[[i]] <- base::as.data.frame(lme4::VarCorr(B2, comp = "vcov"))
  # varComp <- base::as.data.frame(varComp)
  # rv$blup_buffer = varComp

  A2[[i]] = stats::coef(B2)[[input$blup_indep]]
  base::colnames(A2[[i]]) <- i
  A2 <- base::as.data.frame(A2)
  A2[[input$blup_indep]] <- base::row.names.data.frame(A2)
  buf = A2[[1]]
  A2[[1]] = A2[[2]]
  A2[[2]] = buf
  buf = base::colnames(A2)[[1]]
  base::colnames(A2)[[1]] = base::colnames(A2)[[2]]
  base::colnames(A2)[[2]] = buf
  rv$blup_temp = A2

  dat_long <- rv$data %>% tidyr::gather(key = "DTriats", value = "Valuee", input$blup_resp)

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

  filesave('png', input$project_name, ' - Variance Porportion', p1)
  filesave('pdf', input$project_name, ' - Variance Porportion', p1)


  utils::write.csv(rv$blup_temp,
            base::paste0(input$project_name, " -- BLUP value for ", input$blup_resp, ".csv"),
            row.names = FALSE)
  rv$blup_buffer = base::as.data.frame(res$vc.ge)
  utils::write.csv(rv$blup_buffer,
            base::paste0(input$project_name, " -- Variance poportaion for ", input$blup_resp, ".csv"),
            row.names = FALSE)

  set_wd('Blup', 'out')
}