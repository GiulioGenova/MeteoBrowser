#' translates text into current language
#'
#' @export
#'

tr <- function(key,l,t=translation){
  x<-as.character(t[grep(key,t$key),l])
  return(x)
}
