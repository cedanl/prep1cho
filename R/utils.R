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
