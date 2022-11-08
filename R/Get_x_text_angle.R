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
