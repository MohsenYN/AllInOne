check_AllnOne_dependencies <- function(){
  AllInOne_Dependencies <-
    list('leaflet' = '4.2.3')
  pkgs = installed.packages()
  if('leaflet' %in% pkgs){
    if(pkgs['leaflet','Built'] < AllInOne_Dependencies['leaflet']){
      showNotification('Error in dependencies')
      load_pkg('leaflet')
    }
  }
}