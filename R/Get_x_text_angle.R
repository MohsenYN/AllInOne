#' Check the level of column and return the proper angle of X_text_axis
#'
#' @param levels the level number of a column
#'
#' @return the proper angle of X_text_axis
#'
get_x_text_angle <- function(levels) {
  if (levels > 12)
    base::return(90)
  else if(levels > 6 & levels <= 12)
    base::return(45)
  else
    base::return(0)
}


get_width <- function(rv, factor = 1) {
  base::return(nrow(rv$data) * factor)
}


shiny_showNotification <- function(rv, text){
  i = rv$User_Config_notif_size
  msg = shiny::HTML(paste0('<h',i,'>',text,'</h',i,'>'))
  shiny::showNotification(
    ui = msg,
    duration = rv$User_Config_notif_delay,

  )
}
