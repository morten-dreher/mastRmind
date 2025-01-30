#' Play Shiny game
#'
#' @description Launches a game that you can play as a Shiny app.
#'
#' @param replaceColours Logical value describing if the same colour is allowed more than once. So far, only FALSE is implemented.
#' @return Nothing, launches the shiny app
#' @export
playShinyGame <- function(replaceColours = FALSE) {

  appDir <- system.file("shiny-examples", "mastRmind", package = "mastRmind")
  if(appDir == "") {
    stop("Could not find directory. Try re-installing package")
  }


  shiny::runApp(appDir, display.mode = "normal")
}
