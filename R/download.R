#' Get RIO referentiedata
#'
#' Haal actuele RIO (Register Instellingen en Opleidingen) data op.
#' Probeert eerst gebundelde data te lezen uit package installatie.
#' Als niet beschikbaar, download dan van DUO (zonder op te slaan).
#'
#' @param force_download Logisch. Forceer download van DUO? Standaard: FALSE
#'
#' @return Data frame met RIO referentiedata
#' @export
#'
#' @examples
#' \dontrun{
#'   # Get RIO data (from package or download)
#'   rio <- get_rio()
#'
#'   # Force download from DUO
#'   rio <- get_rio(force_download = TRUE)
#' }
get_rio <- function(force_download = FALSE) {

  # RIO download URL (DUO)
  rio_url <- "https://onderwijsdata.duo.nl/datastore/dump/28a4d89b-c223-4dbc-8deb-9d02a533f215?format=csv"

  # Try to read from package installation first (if not forcing download)
  if (!force_download) {
    rio_file <- system.file("data/rio.csv", package = "prep1cho")

    if (rio_file != "") {
      message("Reading RIO data from package...")
      rio_data <- utils::read.csv(rio_file, stringsAsFactors = FALSE)
      # Audit and translate column names
      rio_data <- audit_rio(rio_data)
      return(rio_data)
    }
  }

  # Download directly into memory
  message("Downloading RIO data from DUO...")

  tryCatch(
    {
      # Read directly from URL
      rio_data <- utils::read.csv(url(rio_url), stringsAsFactors = FALSE)
      message("Download complete.")

      # Audit and translate column names
      rio_data <- audit_rio(rio_data)

      return(rio_data)
    },
    error = function(e) {
      rlang::abort(paste0("Failed to download RIO data: ", e$message))
    }
  )
}
