#' Set working directory to the Results folder
#'
#' @param str the name of subfolder
#'
#' @noRd

set_wd <- function (str, rv = NULL){
  if (is.null(rv)) {
    base::setwd(app_sys("app/Results"))
    if (!base::dir.exists(str))
      base::dir.create(str)
    base::setwd(str)
  }
  else{
    if(!base::dir.exists(rv$Path_For_Saving_Results))
      shiny::showNotification(paste0('There is no directory for Results'))
    else{
      base::setwd('../')
      base::file.copy(
        str,
        rv$Path_For_Saving_Results,
        recursive = T,
        overwrite = T
      )
    }
  }
}
