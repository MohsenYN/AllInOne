
#' Save callback function's output whether in PDF or PNG file
#' @description This function immitates grDevices::png() or grDevices::pdf() functions. By this function you can generate a pdf/png file easier and more reliable
#'
#' @param fileformat Whether it is 'png' or 'pdf'
#' @param outputPrefix It is the prefix of file name
#' @param name The main name of the file
#' @param callback This function is supposed to generate the value of generating file
#'
#' @noRd
#'
filesave <- function(fileformat,
                     outputPrefix,
                     name,
                     callback) {
  base::tryCatch({
    base::get(fileformat)(file = base::paste0(outputPrefix, name, ".", fileformat))
    base::invisible(callback())
    base::invisible(grDevices::dev.off())
  }, error = function(e) {
    shiny::showNotification(base::paste0(e$message,' Error in saving file: ', name))
    base::invisible(grDevices::dev.off())
  })
}