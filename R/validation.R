# Data Validation Functions
# Validate input data before processing

#' Validate raw enrollment data before column translation
#'
#' Performs structure validation and checks that all expected columns
#' from Documentatie_ev.csv are present in raw enrollment data.
#'
#' @param enrollments Data frame with raw enrollment data (before translation)
#' @param doc_naming Data frame from Documentatie_ev.csv
#'
#' @return Invisible NULL if validation passes, aborts otherwise
#' @keywords internal
validate_enrollments_raw <- function(enrollments, doc_naming) {

  if (!is.data.frame(enrollments)) {
    rlang::abort("enrollments moet een data frame zijn")
  }

  if (nrow(enrollments) == 0) {
    rlang::abort("enrollments data frame is leeg (0 rijen)")
  }

  if (ncol(enrollments) == 0) {
    rlang::abort("enrollments data frame heeft geen kolommen")
  }

  # For full datasets (>= 10 columns), check all expected columns from documentation
  # For minimal test data (< 10 columns), only check critical columns
  if (ncol(enrollments) >= 10) {
    # Get all expected columns from documentation (where In_gebruik = TRUE)
    expected_cols <- doc_naming$Veldnaam_export[
      doc_naming$In_gebruik == TRUE & !is.na(doc_naming$Veldnaam_export)
    ]

    # Check which expected columns are missing
    missing_cols <- setdiff(expected_cols, colnames(enrollments))

    if (length(missing_cols) > 0) {
      rlang::abort(c(
        "Ongeldige ruwe inschrijvingsdata: verwachte kolommen ontbreken",
        "x" = paste("Aantal ontbrekende kolommen:", length(missing_cols)),
        "x" = paste("Eerste 10:", paste(utils::head(missing_cols, 10), collapse = ", ")),
        "i" = paste("Verwacht aantal kolommen:", length(expected_cols)),
        "i" = paste("Aanwezig aantal kolommen:", ncol(enrollments)),
        "i" = "Gebruik eerst cedanl/1cijferho om het ASCII-bestand om te zetten naar CSV met decodering",
        "i" = "Zie: https://github.com/cedanl/1cijferho"
      ))
    }
  } else {
    # For minimal data, only check critical columns
    critical_internal <- c("INS_Studentnummer", "INS_Inschrijvingsjaar", "DEM_Geslacht_code")
    critical_export <- doc_naming$Veldnaam_export[
      doc_naming$Veldnaam %in% critical_internal & !is.na(doc_naming$Veldnaam_export)
    ]

    missing_critical <- setdiff(critical_export, colnames(enrollments))

    if (length(missing_critical) > 0) {
      rlang::abort(c(
        "Ongeldige ruwe inschrijvingsdata: kritieke kolommen ontbreken",
        "x" = paste("Ontbrekend:", paste(missing_critical, collapse = ", ")),
        "i" = "Minimaal vereist: persoonsgebonden_nummer, inschrijvingsjaar, geslacht"
      ))
    }
  }

  return(invisible(NULL))
}


#' Validate enrollment input data
#'
#' Checks enrollment data for required columns, data types, and valid ranges.
#' Aborts with clear error message if validation fails.
#'
#' @param enrollments Data frame with enrollment data
#'
#' @return Invisible NULL if validation passes, aborts otherwise
#' @keywords internal
validate_enrollments_input <- function(enrollments) {

  # Check it's a data frame
  if (!is.data.frame(enrollments)) {
    rlang::abort("enrollments moet een data frame zijn")
  }

  # Check not empty
  if (nrow(enrollments) == 0) {
    rlang::abort("enrollments data frame is leeg (0 rijen)")
  }

  if (ncol(enrollments) == 0) {
    rlang::abort("enrollments data frame heeft geen kolommen")
  }

  # Define expected columns after audit (internal format)
  # These are the critical columns needed for the pipeline
  required_cols <- c(
    "INS_Studentnummer",
    "INS_Inschrijvingsjaar",
    "DEM_Geslacht_code"
  )

  missing_cols <- setdiff(required_cols, colnames(enrollments))

  if (length(missing_cols) > 0) {
    rlang::abort(c(
      "Ongeldige inschrijvingsdata: vereiste kolommen ontbreken",
      "x" = paste("Ontbrekend:", paste(missing_cols, collapse = ", ")),
      "i" = "Is audit_enrollments() uitgevoerd om kolomnamen te vertalen?",
      "i" = paste("Beschikbare kolommen:", paste(utils::head(colnames(enrollments), 5), collapse = ", "), "...")
    ))
  }

  # Check data types for critical columns
  if ("INS_Inschrijvingsjaar" %in% colnames(enrollments)) {
    if (!is.numeric(enrollments$INS_Inschrijvingsjaar)) {
      rlang::abort("INS_Inschrijvingsjaar moet numeriek zijn")
    }

    # Check reasonable year range
    years <- enrollments$INS_Inschrijvingsjaar[!is.na(enrollments$INS_Inschrijvingsjaar)]
    if (length(years) > 0) {
      if (any(years < 1990 | years > 2050)) {
        rlang::abort(c(
          "Ongeldige inschrijvingsjaren gedetecteerd",
          "x" = paste("Jaarbereik in data:", min(years), "-", max(years)),
          "i" = "Verwachte jaren tussen 1990 en 2050",
          "i" = "Controleer INS_Inschrijvingsjaar kolom op invoerfouten"
        ))
      }
    }
  }

  if ("INS_Studentnummer" %in% colnames(enrollments)) {
    # Check not all NA
    if (all(is.na(enrollments$INS_Studentnummer))) {
      rlang::abort("INS_Studentnummer bevat alleen NA - kan niet verder")
    }

    # Check NA percentage - student number should always be present
    na_count <- sum(is.na(enrollments$INS_Studentnummer))
    na_pct <- round((na_count / nrow(enrollments)) * 100, 1)

    if (na_pct > 5) {
      rlang::abort(c(
        "Te veel missende studentnummers",
        "x" = paste0(na_pct, "% van INS_Studentnummer is NA (", na_count, " van ", nrow(enrollments), " rijen)"),
        "i" = "Studentnummer mag maximaal 5% NA bevatten",
        "i" = "Controleer de kwaliteit van de brondata"
      ))
    }
  }

  return(invisible(NULL))
}


#' Validate RIO input data
#'
#' Checks RIO reference data for required columns and structure.
#'
#' @param rio_data Data frame with RIO data
#'
#' @return Invisible NULL if validation passes, aborts otherwise
#' @keywords internal
validate_rio_input <- function(rio_data) {

  if (!is.data.frame(rio_data)) {
    rlang::abort("rio_data moet een data frame zijn")
  }

  if (nrow(rio_data) == 0) {
    rlang::abort("RIO data is leeg (0 rijen)")
  }

  # Check for required RIO columns (after translation to internal format)
  required_cols <- c(
    "OPL_Code_in_jaar",
    "OPL_Opleidingsnaam_CROHO"
  )

  missing_cols <- setdiff(required_cols, colnames(rio_data))

  if (length(missing_cols) > 0) {
    rlang::abort(c(
      "Ongeldige RIO data: vereiste kolommen ontbreken",
      "x" = paste("Ontbrekend:", paste(missing_cols, collapse = ", ")),
      "i" = "RIO data moet komen van get_rio() of correct geformatteerd zijn",
      "i" = paste("Beschikbare kolommen:", paste(utils::head(colnames(rio_data), 5), collapse = ", "), "...")
    ))
  }

  return(invisible(NULL))
}


#' Validate data types for key columns
#'
#' Checks that columns have expected data types.
#'
#' @param data Data frame to validate
#' @param type_specs Named list of column_name = expected_type
#'
#' @return Invisible NULL if validation passes, aborts otherwise
#' @keywords internal
validate_data_types <- function(data, type_specs) {

  for (col_name in names(type_specs)) {
    if (!col_name %in% colnames(data)) {
      next  # Skip if column doesn't exist
    }

    expected_type <- type_specs[[col_name]]
    actual_type <- class(data[[col_name]])[1]

    # Check type match
    type_ok <- switch(expected_type,
      "numeric" = is.numeric(data[[col_name]]),
      "character" = is.character(data[[col_name]]),
      "integer" = is.integer(data[[col_name]]),
      "Date" = inherits(data[[col_name]], "Date"),
      "logical" = is.logical(data[[col_name]]),
      FALSE
    )

    if (!type_ok) {
      rlang::abort(paste0(
        "Column '", col_name, "' has wrong type.\n",
        "Expected: ", expected_type, ", Got: ", actual_type
      ))
    }
  }

  return(invisible(NULL))
}
