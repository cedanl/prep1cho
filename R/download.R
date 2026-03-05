#' Download RIO referentiedata
#'
#' Download actuele RIO (Register Instellingen en Opleidingen) data van DUO.
#' Gebruikt cached versie indien beschikbaar, tenzij force_download = TRUE.
#'
#' @param output_dir Directory voor opslag. Standaard: "data/00_raw"
#' @param force_download Logisch. Forceer nieuwe download? Standaard: FALSE
#'
#' @return Data frame met RIO referentiedata
#' @export
#'
#' @examples
#' \dontrun{
#'   # Download RIO data (gebruikt cache indien beschikbaar)
#'   rio <- download_rio()
#'
#'   # Forceer nieuwe download
#'   rio <- download_rio(force_download = TRUE)
#' }
download_rio <- function(output_dir = "data/00_raw", force_download = FALSE) {

  # RIO download URL (DUO)
  rio_url <- "https://duo.nl/open_onderwijsdata/images/02-actueel-rio.csv"

  # Output path
  output_file <- file.path(output_dir, "rio.csv")

  # Skip if already exists and not forcing download
  if (file.exists(output_file) && !force_download) {
    message("RIO file already exists. Use force_download = TRUE to re-download.")
    rio_data <- utils::read.csv(output_file, stringsAsFactors = FALSE)
    # Translate column names from new RIO format to old format
    rio_data <- translate_rio_colnames(rio_data)
    return(rio_data)
  }

  # Download file
  message("Downloading RIO data from DUO...")

  tryCatch(
    {
      utils::download.file(rio_url, output_file, mode = "wb", quiet = FALSE)
      message("Download complete: ", output_file)
    },
    error = function(e) {
      stop(paste0("Failed to download RIO data: ", e$message))
    }
  )

  return(rio_data)
}
