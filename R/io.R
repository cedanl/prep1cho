# I/O Functions
# Simplified file reading/writing with consistent format

#' Lees bestand op basis van extensie
#'
#' Leest bestanden in verschillende formaten (CSV, FST, RDS) met correcte
#' encoding en Europese CSV instellingen (puntkomma delimiter, komma decimaal).
#'
#' @param file_path Pad naar het bestand
#'
#' @return Data frame met bestandsinhoud
#' @keywords internal
read_file <- function(file_path) {

  extension <- tools::file_ext(file_path)

  if (extension == "csv") {
    # European CSV format (semicolon delimiter, comma decimal)
    data <- readr::read_delim(
      file_path,
      delim = ";",
      locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
      show_col_types = FALSE
    )

  } else if (extension == "fst") {
    data <- fst::read_fst(file_path)

    # Ensure UTF-8 encoding for character columns
    data <- dplyr::mutate_if(
      data,
      is.character,
      function(x) `Encoding<-`(x, "UTF-8")
    )

  } else if (extension == "rds") {
    data <- readRDS(file_path)

  } else {
    stop(paste0("Unsupported file extension: ", extension))
  }

  return(data)
}


#' Schrijf bestand in meerdere formaten
#'
#' Schrijft data naar een of meerdere bestandsformaten (CSV, FST, RDS).
#'
#' @param data Data frame om weg te schrijven
#' @param file_path Basispad (zonder extensie)
#' @param formats Character vector met gewenste formaten. Standaard: "fst"
#'
#' @return Invisible NULL
#' @keywords internal
write_file <- function(data,
                       file_path,
                       formats = "fst") {

  # Remove extension from file_path to add our own
  file_base <- tools::file_path_sans_ext(file_path)

  # Write in requested formats
  for (format in formats) {
    if (format == "csv") {
      # European CSV format
      data.table::fwrite(
        data,
        paste0(file_base, ".csv"),
        sep = ";",
        dec = ","
      )

    } else if (format == "fst") {
      fst::write_fst(data, paste0(file_base, ".fst"), compress = 100)

    } else if (format == "rds") {
      saveRDS(data, paste0(file_base, ".rds"), version = 3)

    } else {
      warning(paste0("Unknown format: ", format, " - skipped"))
    }
  }

  return(invisible(file_base))
}


# Helper: Get path from config
get_data_path <- function(folder, filename, extension = "fst") {
  file_path <- file.path("data", folder, paste0(filename, ".", extension))
  return(file_path)
}
