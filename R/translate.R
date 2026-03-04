# Translation Functions
# Translate column names using documentation files

#' Vertaal kolom namen met documentatie bestand
#'
#' Algemene functie om kolom namen te vertalen van export formaat naar
#' intern formaat, gebruik makend van een documentatie CSV bestand.
#'
#' @param data Data frame met kolommen in export formaat
#' @param doc_file Bestandsnaam van documentatie CSV (bijv. "Documentatie_RIO.csv")
#' @param doc_dir Directory waar documentatie bestand staat. Standaard: "metadata/assertions"
#'
#' @return Data frame met vertaalde kolom namen
#' @keywords internal
translate_colnames_documentation <- function(data,
                                           doc_file,
                                           doc_dir = "metadata/assertions") {

  # Read documentation file with column name mapping
  doc_path <- system.file(file.path(doc_dir, doc_file), package = "choprep")
  if (doc_path == "") {
    # Development mode - use relative path
    doc_path <- file.path(doc_dir, doc_file)
  }

  doc_naming <- utils::read.csv2(doc_path, stringsAsFactors = FALSE)

  # Create mapping: export name -> internal name
  # Only use rows where In_gebruik == TRUE and Veldnaam_export is not NA
  doc_naming_active <- doc_naming[
    doc_naming$In_gebruik == "TRUE" & !is.na(doc_naming$Veldnaam_export),
  ]

  # Create named vector for renaming
  name_mapping <- stats::setNames(
    doc_naming_active$Veldnaam,
    doc_naming_active$Veldnaam_export
  )

  # Get current column names and map them
  current_cols <- names(data)
  new_cols <- ifelse(
    current_cols %in% names(name_mapping),
    name_mapping[current_cols],
    current_cols
  )

  # Apply new names
  names(data) <- new_cols

  return(data)
}
