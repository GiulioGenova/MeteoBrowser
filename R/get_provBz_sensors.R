#' Get data from province of Bozen sensors. Internal function
#'
#' @export
#' @importFrom httr GET content
#' @importFrom dplyr bind_rows
#' @importFrom magrittr %>%
#'
get_provBz_sensors <- function(){

url <- "http://daten.buergernetz.bz.it/services/meteo/v1/sensors"
u <- GET(url) %>% content
se<-bind_rows(u)
se <-se[!duplicated(se[ , 1:2 ]), ]# %>% as.data.frame
colnames(se)[colnames(se)=="TYPE"] <- "Sensor"
return(se)

}
