## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Npuls CEDA (Centre for Educational Data Analytics)
## Web Page: https://edu.nl/twt84
## Contact: corneel.denhartogh@surf.nl
##
##' *INFO*:
## Renders data dictionary for source data or end-of-pipeline data.
## Uses parametrized Quarto template with interactive reactable output.
## Supports comparison with previous dictionary to avoid redundant renders.
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#' Detect CSV Delimiter
#'
#' Detects whether a CSV file uses comma or semicolon as delimiter.
#'
#' @param file_path Path to the CSV file.
#'
#' @return Character: "," or ";"
#'
#' @keywords internal
detect_csv_delimiter <- function(file_path) {
  first_line <- readLines(file_path, n = 1, warn = FALSE)
  comma_count <- stringr::str_count(first_line, ",")
  semicolon_count <- stringr::str_count(first_line, ";")
  if (comma_count > semicolon_count) "," else ";"
}


#' Compute Dictionary Statistics
#'
#' Computes summary statistics for a dataframe without rendering the full dictionary.
#' Used for comparing source data between runs.
#'
#' @param df A dataframe to compute statistics for.
#'
#' @return A dataframe with column statistics (name, type, NA%, unique values).
#'
#' @keywords internal
compute_dictionary_stats <- function(df) {
  purrr::map_dfr(names(df), function(col) {
    x <- df[[col]]
    n_total <- length(x)
    n_na <- sum(is.na(x))
    na_pct <- round(n_na / n_total * 100, 1)
    n_uniek <- dplyr::n_distinct(x, na.rm = TRUE)

    col_type <- if (is.numeric(x)) {
      "numeric"
    } else if (is.logical(x)) {
      "logical"
    } else if (is.factor(x)) {
      "factor"
    } else {
      "character"
    }

    tibble::tibble(
      Veldnaam = col,
      Type = col_type,
      NA_pct = na_pct,
      N_uniek = n_uniek
    )
  })
}


#' Get Most Recent Stats File
#'
#' Finds the most recent stats CSV file in the dictionary directory.
#'
#' @param output_dir Directory to search in.
#' @param type Dictionary type ("source" or "end").
#'
#' @return Path to the most recent stats file, or NULL if none exists.
#'
#' @keywords internal
get_most_recent_stats <- function(output_dir, type) {
  stats_files <- list.files(
    output_dir,
    pattern = sprintf("stats_%s_.*\\.csv$", type),
    full.names = TRUE
  )

  if (length(stats_files) == 0) {
    return(NULL)
  }

  # Get most recent by file modification time
  file_info <- file.info(stats_files)
  most_recent <- stats_files[which.max(file_info$mtime)]
  return(most_recent)
}


#' Render Data Dictionary
#'
#' Generates an interactive HTML data dictionary for either source data
#' or end-of-pipeline prepared data.
#'
#' @param type Character. Either "source" (raw 1CHO data) or "end" (prepared data).
#' @param config_name Character. Config profile to use. Defaults to R_CONFIG_ACTIVE env var.
#' @param check_changed Logical. If TRUE, only renders if data has changed since last run.
#'   Compares column statistics with previous dictionary. Default FALSE.
#'
#' @return Invisibly returns the path to the generated HTML file, or NULL if skipped.
#'
#' @examples
#' \dontrun{
#' # Source dictionary (for source file analysis)
#' render_data_dictionary(type = "source")
#'
#' # End dictionary (after pipeline)
#' render_data_dictionary(type = "end")
#'
#' # Only render if source data changed
#' render_data_dictionary(type = "source", check_changed = TRUE)
#'
#' # With specific config
#' render_data_dictionary(type = "end", config_name = "vu")
#' }
render_data_dictionary <- function(
    type = c("source", "end"),
    config_name = Sys.getenv("R_CONFIG_ACTIVE", "default"),
    check_changed = FALSE
) {
  type <- match.arg(type)

  cfg <- config::get(config = config_name)
  institution <- cfg$metadata_institution_name
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M")

  output_dir_rel <- switch(type,
    source = "metadata/data_dictionary_source",
    end = "metadata/data_dictionary_end"
  )
  output_dir <- file.path(getwd(), output_dir_rel)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # If check_changed is TRUE, compare with previous stats

if (check_changed) {
    cli::cli_alert_info("Checking if {type} data has changed...")

    # Load current data
    if (type == "source") {
      source_file <- cfg$data_1cho_enrollments_file_path
      delim <- detect_csv_delimiter(source_file)
      current_df <- readr::read_delim(source_file, delim = delim, show_col_types = FALSE)
    } else {
      data_path_fst <- "data/03_combined/prepared_enrollments.fst"
      data_path_rds <- "data/03_combined/enrollments.rds"
      if (file.exists(data_path_fst)) {
        current_df <- fst::read_fst(data_path_fst)
      } else if (file.exists(data_path_rds)) {
        current_df <- readRDS(data_path_rds)
      } else {
        cli::cli_alert_warning("No data file found, skipping comparison")
        current_df <- NULL
      }
    }

    if (!is.null(current_df)) {
      current_stats <- compute_dictionary_stats(current_df)

      # Get previous stats
      previous_stats_file <- get_most_recent_stats(output_dir, type)

      if (!is.null(previous_stats_file)) {
        previous_stats <- readr::read_csv(previous_stats_file, show_col_types = FALSE)

        # Compare: same columns, same types, same NA%, same unique counts
        stats_identical <- identical(
          current_stats |> dplyr::arrange(Veldnaam),
          previous_stats |> dplyr::arrange(Veldnaam)
        )

        if (stats_identical) {
          cli::cli_alert_success(
            "Data unchanged since {basename(previous_stats_file)} - skipping render"
          )
          return(invisible(NULL))
        } else {
          cli::cli_alert_info("Data has changed - rendering new dictionary")
        }
      } else {
        cli::cli_alert_info("No previous dictionary found - rendering new dictionary")
      }

      # Save current stats for future comparison
      stats_file <- file.path(
        output_dir,
        sprintf("stats_%s_%s_%s.csv", type, institution, timestamp)
      )
      readr::write_csv(current_stats, stats_file)
    }
  }

  output_file <- sprintf("dictionary_%s_%s_%s.html", type, institution, timestamp)

  cli::cli_alert_info("Rendering {type} dictionary for {institution}...")

  # WINDOWS FIX: Add --no-clean flag to prevent Quarto from trying to delete
  # .quarto directory at the end (causes "os error 32" on Windows due to locked files)
  # We clean it up manually at the start of next render instead
  quarto::quarto_render(
    input = "04_export/data_dictionary.qmd",
    output_file = output_file,
    quarto_args = c("--output-dir", output_dir, "--no-clean"),
    execute_dir = getwd(),
    execute_params = list(type = type)
  )

  output_path <- file.path(output_dir_rel, output_file)
  cli::cli_alert_success("Dictionary created: {output_path}")
  invisible(output_path)
}
