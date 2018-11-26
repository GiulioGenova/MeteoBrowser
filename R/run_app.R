#' Get data from province of Bozen monitoring stations.
#' @param app character string with the name of the app.
#' @param launchBrowser whether to launch the application with the browser or not.
#' @export
#' @examples
#' run_app('MeteoBrowser')
#'

run_app <- function(app,launchBrowser=T) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shinyApps", package = "MeteoBrowser"))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(app) || !nzchar(app) ||
      !app %in% validExamples) {
    stop(
      'Please run `run_app()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shinyApps", app, package = "MeteoBrowser")
  if(launchBrowser){
    shiny::runApp(appDir, display.mode = "normal",launch.browser = T)
  } else {
    shiny::runApp(appDir, display.mode = "normal")
  }
}
