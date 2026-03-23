#' Start de prep1cho Shiny app
#'
#' Interactieve data verkenner voor prep1cho pipeline output.
#' Upload een CSV of FST bestand, of gebruik automatisch de synthetische data.
#'
#' @param ... Extra argumenten doorgegeven aan [shiny::runApp()].
#'
#' @return Onzichtbaar. Wordt aangeroepen voor het starten van de app.
#' @export
run_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg shiny} is nodig. Installeer met: {.code install.packages('shiny')}")
  }
  app_dir <- system.file("app", package = "prep1cho")
  if (app_dir == "") {
    cli::cli_abort("Kan de Shiny app niet vinden. Is {.pkg prep1cho} correct geinstalleerd?")
  }
  shiny::runApp(app_dir, ...)
}
