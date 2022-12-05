#' Heritability Calculator
#'
#' @description Calculating heritability considering all the selected independent variables as random and identifying genotype variable.
#'
#' @param input object including user's input values
#' @param rv object including reactive variables
#'
#' @noRd
Heritability <- function(input, rv) {
  set_wd('Heritability')
  formula_str = input$rv_her
  genom_dep_col = input$indep_her
  indep_cols = input$main_db_indep_val

  for (i in indep_cols) {
    if (base::length(base::unique(rv$data[[i]])) < 2) {
      indep_cols = base::subset(indep_cols, indep_cols != i)
    }
  }
  if (!base::is.null(rv$spat_buffer)) {
    if (input$use_spat) {
      dat <- rv$spat_buffer %>% dplyr::mutate_at(dplyr::vars(input$main_db_indep_val), as.factor)
    }else {
      dat <- rv$data %>% dplyr::mutate_at(dplyr::vars(input$main_db_indep_val), as.factor)
    }
  }
  else {
    dat <- rv$data %>% dplyr::mutate_at(dplyr::vars(input$main_db_indep_val), as.factor)
  }

  dat_long <- dat %>% tidyr::gather(key = "DTriats", value = "Valuee", input$main_db_dep_val)


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


  bs_her <- res %>%
    tidyr::unnest(vc.ge) %>%
    dplyr::group_by(DTriats) %>%
    dplyr::summarize(h = (Variance[grp == genom_dep_col]) / (base::sum(Variance)))

  utils::write.csv(bs_her, base::paste0(input$project_name,' -- Heritability Values.csv'), row.names = F)

  p <- function() {
    var <- ggplot2::ggplot(data = bs_her, ggplot2::aes(x = DTriats, y = h, group = 5, fill = DTriats)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge", width = 0.5) +
      ggplot2::theme_classic() +
      ggplot2::labs(y = "Heritability", x = "Dependant/Response Variable") +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Trait"))
    base::invisible(base::print(var))
  }

  filesave('png', input$project_name, ' --  Heritability', p, rv)
  filesave('pdf', input$project_name, ' --  Heritability', p, rv)

  set_wd('Heritability', rv, input$save_results)
}
