# Utility Functions
# Simple helper functions used across the pipeline

#' Bereken academisch jaar uit datum
#'
#' Bepaalt het academisch jaar op basis van een datum. Een academisch jaar
#' begint op 1 september. Datums vanaf september worden toegewezen aan het
#' huidige jaar, datums vóór september aan het vorige jaar.
#'
#' @param date Date vector of te converteren waarde
#'
#' @return Integer vector met academisch jaren
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'   academic_year(as.Date("2024-09-01"))  # Returns 2024
#'   academic_year(as.Date("2024-08-31"))  # Returns 2023
#' }
academic_year <- function(date) {
  if (all(is.na(date))) {
    return(NA_integer_)
  }

  # Convert to Date if needed
  if (!lubridate::is.Date(date)) {
    date <- lubridate::as_date(date)
  }

  year <- lubridate::year(date)
  month <- lubridate::month(date)

  # If month >= 9 (Sept or later), academic year = current year
  # If month < 9, academic year = previous year
  academic_yr <- ifelse(month >= 9, year, year - 1)

  return(as.integer(academic_yr))
}


#' Detecteer metadata uit inschrijvingsdata
#'
#' Detecteert de BRIN code van de instelling en het academisch jaar uit
#' een inschrijvings data frame. Werkt met zowel ruwe 1CHO kolomnamen
#' (lowercase) als vertaalde kolomnamen (INS_ prefix).
#'
#' @param enrollments Data frame met inschrijvingsgegevens
#'
#' @return Lijst met:
#'   \item{year}{Maximaal inschrijvingsjaar, of NULL als niet gevonden}
#'   \item{institution_brin}{Meest voorkomende BRIN code, of NULL als niet gevonden}
#'
#' @export
detect_metadata <- function(enrollments) {
  year_col <- intersect(
    c("inschrijvingsjaar", "INS_Inschrijvingsjaar"),
    names(enrollments)
  )
  brin_col <- intersect(
    c("instellingscode", "INS_Instelling"),
    names(enrollments)
  )

  year <- if (length(year_col) > 0) {
    max(enrollments[[year_col[1]]], na.rm = TRUE)
  } else {
    NULL
  }

  brin <- if (length(brin_col) > 0) {
    freq <- sort(table(enrollments[[brin_col[1]]]), decreasing = TRUE)
    names(freq)[1]
  } else {
    NULL
  }

  return(list(year = year, institution_brin = brin))
}


#' Ruim objecten op uit environment
#' @keywords internal
clear_script_objects <- function(keep = NULL) {
  # Get all objects in calling environment
  all_objects <- ls(envir = parent.frame())

  # Remove objects except those in 'keep'
  if (!is.null(keep)) {
    to_remove <- setdiff(all_objects, keep)
  } else {
    to_remove <- all_objects
  }

  # Remove objects
  if (length(to_remove) > 0) {
    rm(list = to_remove, envir = parent.frame())
  }

  return(invisible(NULL))
}


#' Haal eerste class van object
#' @keywords internal
first_class <- function(x) {
  return(class(x)[1])
}


#' Rond waarden af (negatief naar beneden, positief naar boven)
#' @keywords internal
round_values <- function(x) {
  ifelse(x >= 0, ceiling(x), floor(x))
}


#' Minimum voor numerieke vectors
#' @keywords internal
minnum <- function(x) {
  if (!is.numeric(x)) {
    return(NA_real_)
  }

  non_missing <- x[!is.na(x)]

  if (length(non_missing) == 0) {
    return(NA_real_)
  }

  return(round_values(min(non_missing)))
}


#' Maximum voor numerieke vectors
#' @keywords internal
maxnum <- function(x) {
  if (!is.numeric(x)) {
    return(NA_real_)
  }

  non_missing <- x[!is.na(x)]

  if (length(non_missing) == 0) {
    return(NA_real_)
  }

  return(round_values(max(non_missing)))
}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Mapping functions (replacement for vusa package) ----
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' Translate values using a mapping table
#'
#' Maps values from a source column to a new column using a mapping table with
#' 'from' and 'to' columns. This is a simple wrapper around dplyr::left_join.
#'
#' @param Data Data frame to transform
#' @param current Name of the source column to translate
#' @param new Name of the new column to create
#' @param mapping_table_input Data frame with 'from' and 'to' columns
#' @param KeepOriginal Logical. Keep the original column? Default: TRUE
#'
#' @return Data frame with new translated column
#' @keywords internal
mapping_translate <- function(Data, current, new, mapping_table_input = NULL, KeepOriginal = TRUE) {
  # Validate mapping table
  if (is.null(mapping_table_input)) {
    rlang::abort("mapping_table_input must be provided")
  }

  if (!all(c("from", "to") %in% names(mapping_table_input))) {
    rlang::abort("mapping_table_input must contain columns 'from' and 'to'")
  }

  # Check if new column already exists
  if (new %in% colnames(Data)) {
    rlang::abort(paste0("Column '", new, "' already exists in Data"))
  }

  # Perform translation using match() to replicate vusa behavior exactly
  Data[[new]] <- mapping_table_input$to[match(Data[[current]], mapping_table_input$from)]

  # Remove original column if requested
  if (!KeepOriginal) {
    Data[[current]] <- NULL
  }

  return(Data)
}


#' Categorize numeric values using a mapping table
#'
#' Creates categories from numeric values using a mapping table with 'lower',
#' 'upper', and 'category' columns. Uses base R cut() function.
#'
#' @param Data Data frame to transform
#' @param current Name of the numeric source column
#' @param new Name of the new category column to create
#' @param mapping_table_input Data frame with 'lower', 'upper', and 'category' columns
#'
#' @return Data frame with new category column
#' @keywords internal
mapping_category <- function(Data, current, new, mapping_table_input = NULL) {
  # Validate mapping table
  if (is.null(mapping_table_input)) {
    rlang::abort("mapping_table_input must be provided")
  }

  if (!all(c("lower", "upper", "category") %in% names(mapping_table_input))) {
    rlang::abort("mapping_table_input must contain columns 'lower', 'upper', and 'category'")
  }

  # Create boundaries from lower and last upper value
  boundaries <- c(mapping_table_input$lower, utils::tail(mapping_table_input$upper, n = 1))

  # Create categories using cut (right = FALSE means [lower, upper))
  Data[[new]] <- as.character(cut(
    Data[[current]],
    breaks = boundaries,
    labels = mapping_table_input$category,
    right = FALSE
  ))

  return(Data)
}


#' Translate column names using documentation table
#'
#' Renames columns from export format to internal format using a documentation
#' table with 'Veldnaam' and 'Veldnaam_export' columns.
#'
#' @param data Data frame to transform
#' @param documentatie_df Documentation data frame with 'Veldnaam' and 'Veldnaam_export'
#' @param drop_na Logical. Drop columns that don't have a mapping? Default: TRUE
#'
#' @return Data frame with renamed columns
#' @keywords internal
translate_colnames_documentation <- function(data, documentatie_df, drop_na = TRUE) {
  # Create named vector for renaming: new_name = old_name
  current_cols <- colnames(data)

  # Match current column names to Veldnaam_export and get Veldnaam
  new_names <- documentatie_df$Veldnaam[match(current_cols, documentatie_df$Veldnaam_export)]

  # Apply new names
  colnames(data) <- new_names

  # Drop columns with NA names if requested
  if (drop_na) {
    data <- data[, !is.na(colnames(data)), drop = FALSE]
  }

  return(data)
}
