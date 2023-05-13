load_pkg <- function(pkg,load = F) {
  for (str in pkg){
    pkgs_name = rownames(installed.packages())
    if (!str %in% pkgs_name) {
      shiny_showNotification(rv ,base::paste0('Package "{str}" is not installed, we try to install it now!!'))
      utils::install.packages(str, quiet = T)
    }else{
      if(load)
        library(str, character.only = T)
    }
  }
  ret = T
  for (str in pkg){
    pkgs_name = rownames(installed.packages())
    if (!str %in% pkgs_name) {
      shiny_showNotification(rv ,paste0('Sorry, installation of package {str} failed'))
      ret = F
    }
  }
  return(ret)
}