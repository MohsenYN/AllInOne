#' Put CSV file last in the list
#'
#' @param   filelist a list of file names
#'
#' @return a list of file names which the csv files located at the end of it
#'
put_csv_last <- function(filelist) {
  str = filelist
  if(base::length(str)<2){
    base::return(str)
  }
  for (i in base::seq(from = 1, to = base::length(str) - 1))
    for (j in base::seq(from = i + 1, to = base::length(str)))
      if (is.csv(str[i]) & !is.csv(str[j])) {
        buf = str[i]
        str[i] = str[j]
        str[j] = buf
      }

  base::return(str)
}
