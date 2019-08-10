#' gets most frequrnt value form a vector
#'
#' @param Value a vector
#' @export
#'

Dir <- function(Value){

  Value=as.numeric(names(which.max(table(Value,useNA = "no"))))

  return(Value)

}
