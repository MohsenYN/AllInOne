#' Check if str is name of a CSV file or not
#'
#' @param a character vector including names of a file
#'
#' @return return true or false whether the name ended with '.csv' or not
#'
is.csv <- function(str) {
  str = base::strsplit(str, '.c')[[1]]
  len = base::length(str)
  if (str[len] == 'sv')
    base::return(T)
  else
    base::return(F)
}
