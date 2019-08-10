#' reclassify wind from degrees to 8 directions
#'
#' @param wind a vector with values between 0 and 360
#' @export
#'


reclass_wind <- function(wind){

  wind=ifelse(wind>0 & wind<=22.5,1,
             ifelse(wind>22.5 & wind<=67.5,2,
                    ifelse(wind>67.5 & wind<=112.5,3,
                           ifelse(wind>112.5 & wind<=157.5,4,
                                  ifelse(wind>157.5 & wind<=202.5,5,
                                         ifelse(wind>202.5 & wind<=247.5,6,
                                                ifelse(wind>247.5 & wind<=292.5,7,
                                                       ifelse(wind>292.5 & wind<=337.5,8,
                                                              ifelse(wind>292.5 & wind<=360,1,NA)))))))))

  return(wind)
}
