#' Controleer datakwaliteit van inschrijvingsgegevens
#'
#' Voert basale kwaliteitscontroles uit op inschrijvingsgegevens, inclusief
#' controle op lege kolommen en hoog percentage missende waarden.
#' Audit informatie wordt als messages/warnings getoond.
#'
#' @param enrollments Data frame met ruwe inschrijvingsgegevens
#'
#' @return Data frame met vertaalde kolom namen
#' @export
audit_enrollments <- function(enrollments) {

  doc_path <- system.file(file.path("metadata/assertions/Documentatie_ev.csv"), package = "prep1cho")
  doc_naming <- utils::read.csv2(doc_path, stringsAsFactors = FALSE)
  # Translate column names from 1CHO format to internal format
  enrollments <- vusa::wrapper_translate_colnames_documentation(enrollments, doc_naming)

  # Basic validation
  n_rows <- nrow(enrollments)
  n_cols <- ncol(enrollments)

  message("Auditing enrollments data...")
  message("  Rows: ", format(n_rows, big.mark = ","))
  message("  Columns: ", n_cols)

  # Check for completely empty columns
  empty_cols <- sapply(enrollments, function(x) all(is.na(x)))
  n_empty <- sum(empty_cols)

  if (n_empty > 0) {
    message("  Empty columns: ", n_empty)
    warning(paste0(n_empty, " columns are completely empty"))
  } else {
    message("  Empty columns: 0")
  }

  # Check NA percentages
  na_pct <- sapply(enrollments, function(x) round(mean(is.na(x)) * 100, 1))
  high_na <- na_pct[na_pct > 75]

  if (length(high_na) > 0) {
    message("  High NA columns: ", length(high_na))
    warning(paste0(length(high_na), " columns have >75% missing values"))
  } else {
    message("  High NA columns: 0")
  }

  message("Audit complete.")

  return(enrollments)
}

#' Controleer en vertaal RIO kolommen
#'
#' Vertaalt kolom namen van RIO data van DUO export formaat naar intern formaat.
#'
#' @param rio_data Data frame met ruwe RIO data
#'
#' @return Data frame met vertaalde kolom namen
#'
#' @keywords internal
audit_rio <- function(rio_data) {

  doc_path <- system.file(file.path("metadata/assertions/Documentatie_RIO.csv"), package = "prep1cho")
  doc_naming <- utils::read.csv2(doc_path, stringsAsFactors = FALSE)


  # Translate column names from new RIO format to old format
  rio_data <- vusa::wrapper_translate_colnames_documentation(rio_data, doc_naming)

  return(rio_data)
}
