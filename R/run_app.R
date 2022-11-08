#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom dplyr %>%
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {

  check_AllnOne_dependencies <- function() {

    is_equal_greater <- function(a, b) {
      res = a < b
      return(!res)
    }

    AllInOne_Dependencies <- list(
      'tidyverse' = '1.3.2',
      'ggplot2' = '3.4.0',
      'ggpubr' = '0.4.0',
      'forcats' = '0.5.1',
      'tidyr' = '1.2.0',
      'stringr' = '1.4.0',
      'purrr' = '0.3.4',
      'tibble' = '3.1.8',
      'stats' = '4.2.0',
      'finalfit' = '1.0.5',
      'naniar' = '0.6.1',
      'corrplot' = '0.92',
      'RColorBrewer' = '1.1-3',
      'gridExtra' = '2.3',
      'mice' = '3.14.0',
      'bestNormalize' = '1.8.3',
      'MASS' = '7.3-56',
      'car' = '3.1-0',
      'usethis' = '2.1.6',
      'testthat' = '3.1.4',
      'config' = '0.3.1',
      'DT' = '0.26',
      'lme4' = '1.1-30',
      'readxl' = '1.4.1',
      'shinydashboard' = '0.7.2',
      'shinydisconnect' = '0.1.0',
      'shinyjs' = '2.1.0',
      'VIM' = '6.2.2'
    )
    flag = F
    err = NULL
    for (str in base::names(AllInOne_Dependencies)) {
      pkgs = installed.packages()
      if (str %in% pkgs) {
        if (!is_equal_greater(pkgs[str, 'Version'], AllInOne_Dependencies[str])) {
          base::print(base::paste0('The <<', str, '>> package is out of date!'))
          utils::install.packages(str, quiet = T)
          pkgs = utils::installed.packages()
          if(!is_equal_greater(pkgs[str, 'Version'], AllInOne_Dependencies[str])) {
            base::print(base::paste0('Ops! Error in installing package ', str))
            flag = T
            err = c(err, str)
          }
        }
      }else {
        base::print(base::paste0('The <<', str, '>> package is not installed!'))
        utils::install.packages(str, quiet = T)
        pkgs = utils::installed.packages()
        if (!(str %in% pkgs)) {
          base::print(base::paste0('Ops! Error in installing package ', str))
          flag = T
          err = c(err, str)
        }
      }
    }

    if (flag){
      print('Sorry, Due to failing to install the following packages it is not possible to run AllInOne')
      print(err)
      print('Please restart R session (ctrl+shift+F10) and run again.')
      return(F)
    }else
      return(T)

  }

  if (check_AllnOne_dependencies()) {
    golem::with_golem_options(
      app = shinyApp(
        ui = app_ui,
        server = app_server,
        onStart = onStart,
        options = options,
        enableBookmarking = enableBookmarking,
        uiPattern = uiPattern
      ),
      golem_opts = list(...)
    )
  }
}
