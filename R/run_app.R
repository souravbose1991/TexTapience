#' @export
runApp <- function() {
  appDir <- system.file("app", package = "TexTapience")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `TexTapience`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
