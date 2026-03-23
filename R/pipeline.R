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
#' @param year Academisch jaar. Als NULL, wordt het maximale inschrijvingsjaar
#'   uit de data gedetecteerd.
#' @param institution_brin BRIN code van de instelling. Als NULL, wordt de
#'   meest voorkomende instellingscode uit de data gedetecteerd.
#' @param download_rio Logisch. Download fresh RIO data? Standaard: FALSE
#'
#' @return Data frame met verrijkte inschrijvingsgegevens
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Laad 1CHO inschrijvingsgegevens
#'   enrollments <- read.csv2("EV299XX24_DEMO_decoded.csv")
#'
#'   # Voer pipeline uit - year en BRIN worden automatisch gedetecteerd
#'   data <- run_pipeline(enrollments)
#'
#'   # Of specificeer handmatig
#'   data <- run_pipeline(
#'     enrollments,
#'     year = 2024,
#'     institution_brin = "21PL"
#'   )
#'
#'   # Bekijk resultaat
#'   head(data)
#'
#'   # Exporteer
#'   write.csv2(data, "output.csv", row.names = FALSE)
#' }
run_pipeline <- function(enrollments,
                        rio_data = NULL,
                        year = NULL,
                        institution_brin = NULL,
                        download_rio = FALSE) {

  ## Auto-detect year from data
  year_col <- intersect(
    c("inschrijvingsjaar", "INS_Inschrijvingsjaar"),
    names(enrollments)
  )
  if (is.null(year) && length(year_col) > 0) {
    year <- max(enrollments[[year_col[1]]], na.rm = TRUE)
    message("Detected year from data: ", year)
  } else if (is.null(year)) {
    year <- as.integer(format(Sys.Date(), "%Y"))
    message("No year detected, using current year: ", year)
  }

  ## Auto-detect BRIN from data
  brin_col <- intersect(
    c("instellingscode", "INS_Instelling"),
    names(enrollments)
  )
  if (is.null(institution_brin) && length(brin_col) > 0) {
    brin_freq <- sort(table(enrollments[[brin_col[1]]]),
                      decreasing = TRUE)
    institution_brin <- names(brin_freq)[1]
    message("Detected BRIN from data: ", institution_brin)
  } else if (is.null(institution_brin)) {
    institution_brin <- "21XX"
    message("No BRIN detected, using default: ", institution_brin)
  }

  ## Synthetic rows only needed for demo BRIN
  create_synthetic <- institution_brin == "21XX"

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
  enrollments_clean <- audit_enrollments(enrollments)

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
  data <- enrollments_prep |>
    combine_enrollments_rio(rio_prepared$rio_per_jaar) |>
    combine_enrollments_calculations() |>
    combine_enrollments_final()

  # Step 6: Summary
  message("\n[6/6] Pipeline complete!")
  message("========================================")
  message("Output rows: ", format(nrow(data), big.mark = ","))
  message("Output columns: ", ncol(data))
  message("========================================\n")

  # Return results
  return(data)
}
