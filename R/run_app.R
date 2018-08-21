#' @export
runApp <- function() {
  appDir <- system.file("app", package = "TexTrove")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `TexTrove`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
