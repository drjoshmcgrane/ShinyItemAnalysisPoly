.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste("This is ShinyItemAnalysisPoly version "), packageVersion("ShinyItemAnalysisPoly"),
    "\n- to run the interactive {shiny} app, call `run_app(background = FALSE)`",
    "\n- fork of ShinyItemAnalysis with extended polytomous IRT support"
  )
}
