# Audit Functions
# Data quality checks and validation

#' Translate enrollment column names from 1CHO to internal format
#'
#' @param enrollments Data frame met 1CHO enrollment data (export kolom namen)
#' @return Data frame met vertaalde kolom namen (internal format)
#' @keywords internal
translate_enrollments_colnames <- function(enrollments) {
  translate_colnames_documentation(
    enrollments,
    doc_file = "Documentatie_enrollments_avans_feature_requests.csv"
  )
}


#' Controleer datakwaliteit van inschrijvingsgegevens
#'
#' Voert basale kwaliteitscontroles uit op inschrijvingsgegevens, inclusief
#' controle op lege kolommen en hoog percentage missende waarden.
#'
#' @param enrollments Data frame met ruwe inschrijvingsgegevens
#'
#' @return Lijst met:
#'   \item{data}{Originele data (ongewijzigd)}
#'   \item{report}{Audit rapport met kwaliteitsindicatoren}
#'
#' @keywords internal
audit_enrollments <- function(enrollments) {

  # Translate column names from 1CHO format to internal format
  enrollments <- translate_enrollments_colnames(enrollments)

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
    warning(paste0(n_empty, " columns are completely empty"))
  }

  # Check NA percentages
  na_pct <- sapply(enrollments, function(x) round(mean(is.na(x)) * 100, 1))
  high_na <- na_pct[na_pct > 75]

  if (length(high_na) > 0) {
    warning(paste0(length(high_na), " columns have >75% missing values"))
  }

  # Create audit report
  audit_report <- list(
    n_rows = n_rows,
    n_cols = n_cols,
    empty_cols = names(empty_cols)[empty_cols],
    high_na_cols = names(high_na),
    na_summary = na_pct
  )

  message("Audit complete.")

  return(list(
    data = enrollments,
    report = audit_report
  ))
}
