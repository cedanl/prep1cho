# Main Pipeline Function
# Run complete 1CHO enrollment preparation pipeline

#' Voer volledige 1CHO inschrijvingsdata voorbereidingspipeline uit
#'
#' Deze functie voert de complete pipeline uit voor het voorbereiden van 1CijferHO
#' inschrijvingsgegevens. Het combineert data transformatie, kwaliteitscontrole,
#' mapping tables toepassen, en verrijking met RIO referentiedata.
#'
#' @param enrollments Data frame met 1CHO inschrijvingsgegevens (EV-bestand)
#' @param rio_data Optioneel: Data frame met RIO referentiedata. Als NULL, wordt
#'   RIO data automatisch gedownload.
#' @param year Academisch jaar voor maximale datum. Standaard: 2024
#' @param institution_brin BRIN code van de instelling. Standaard: "21XX"
#' @param create_synthetic Logisch. Maak synthetische test rows? Standaard: TRUE
#' @param download_rio Logisch. Download fresh RIO data? Standaard: FALSE
#'
#' @return Een lijst met:
#'   \item{data}{Data frame met verrijkte inschrijvingsgegevens}
#'   \item{audit_report}{Data kwaliteitsrapport}
#'   \item{n_enrollments}{Aantal verwerkte inschrijvingen}
#'   \item{n_columns}{Aantal kolommen in output}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Laad 1CHO inschrijvingsgegevens
#'   enrollments <- read.csv2("EV299XX24_DEMO_decoded.csv")
#'
#'   # Voer pipeline uit met standaard instellingen
#'   result <- run_pipeline(enrollments)
#'
#'   # Voor specifieke instelling en jaar
#'   result <- run_pipeline(
#'     enrollments,
#'     year = 2024,
#'     institution_brin = "21PL",
#'     create_synthetic = FALSE
#'   )
#'
#'   # Bekijk resultaat
#'   head(result$data)
#'
#'   # Exporteer
#'   write.csv2(result$data, "output.csv", row.names = FALSE)
#' }
run_pipeline <- function(enrollments,
                        rio_data = NULL,
                        year = 2024,
                        institution_brin = "21XX",
                        create_synthetic = TRUE,
                        download_rio = FALSE) {

  message("\n========================================")
  message("PREP1CHO Pipeline")
  message("========================================")
  message("Year: ", year)
  message("Institution: ", institution_brin)
  message("Input rows: ", format(nrow(enrollments), big.mark = ","))
  message("========================================\n")

  # Step 1: Get RIO data if needed
  if (is.null(rio_data) || download_rio) {
    message("[1/6] Getting RIO data...")
    rio_data <- get_rio(force_download = download_rio)
  } else {
    message("[1/6] Using provided RIO data...")
    message("  Auditing RIO data...")
    rio_data <- audit_rio(rio_data)
  }

  # Step 2: Audit enrollments
  message("\n[2/6] Auditing enrollments...")
  audit_result <- audit_enrollments(enrollments)
  enrollments_clean <- audit_result$data

  # Step 3: Prepare RIO data
  message("\n[3/6] Preparing RIO data...")
  rio_prepared <- prepare_rio(
    rio_data,
    year = year,
    institution_brin = institution_brin,
    create_synthetic = create_synthetic
  )

  # Step 4: Prepare enrollments
  message("\n[4/6] Preparing enrollments...")
  enrollments_prep <- enrollments_clean |>
    prepare_enrollments_mapping() |>
    prepare_enrollments_supplemental(year = year, institution_brin = institution_brin)

  # Step 5: Combine with RIO
  message("\n[5/6] Combining with RIO data...")
  enrollments_combined <- enrollments_prep |>
    combine_enrollments_rio(rio_prepared$rio_per_jaar) |>
    combine_enrollments_calculations() |>
    combine_enrollments_final()

  # Step 6: Summary
  message("\n[6/6] Pipeline complete!")
  message("========================================")
  message("Output rows: ", format(nrow(enrollments_combined), big.mark = ","))
  message("Output columns: ", ncol(enrollments_combined))
  message("========================================\n")

  # Return results
  return(list(
    data = enrollments_combined,
    audit_report = audit_result$report,
    n_enrollments = nrow(enrollments_combined),
    n_columns = ncol(enrollments_combined)
  ))
}
