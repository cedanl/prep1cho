# Package-level documentatie

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' prep1cho: 1CHO Inschrijvingsgegevens Voorbereiden
#'
#' Bereid 1CijferHO inschrijvingsgegevens voor voor analyse en visualisatie.
#' Het package biedt functies voor het downloaden van RIO referentiedata,
#' het controleren van datakwaliteit, het toepassen van mappingtabellen, en het
#' verrijken van inschrijvingsgegevens met opleidingsinformatie.
#'
#' @section Belangrijkste functies:
#' \describe{
#'   \item{\code{\link{run_pipeline}}}{Voer volledige pipeline uit (AANBEVOLEN)}
#'   \item{\code{\link{get_rio}}}{Haal RIO referentiedata op}
#'   \item{\code{\link{audit_enrollments}}}{Controleer datakwaliteit}
#'   \item{\code{\link{prepare_enrollments_mapping}}}{Pas mappingtabellen toe}
#'   \item{\code{\link{combine_enrollments_rio}}}{Verrijk met RIO gegevens}
#' }
#'
#' @section Snelstart workflow:
#' De eenvoudigste manier om een analyse uit te voeren:
#' \preformatted{
#' library(prep1cho)
#'
#' # Laad 1CHO gegevens
#' enrollments <- read.csv2("EV299XX24_DEMO_decoded.csv")
#'
#' # Voer pipeline uit
#' data <- run_pipeline(
#'   enrollments,
#'   year = 2024,
#'   institution_brin = "21XX"
#' )
#'
#' # Bekijk resultaat
#' head(data)
#'
#' # Sla op (optioneel)
#' write.csv2(data, "output.csv", row.names = FALSE)
#' }
#'
#' @section Stap-voor-stap workflow:
#' Voor meer controle over elke stap:
#' \enumerate{
#'   \item Haal RIO data op met \code{get_rio()}
#'   \item Controleer datakwaliteit met \code{audit_enrollments()}
#'   \item Pas mappings toe met \code{prepare_enrollments_mapping()}
#'   \item Verrijk met RIO met \code{combine_enrollments_rio()}
#'   \item Sla resultaat op met \code{write.csv2()}, \code{fst::write_fst()}, etc.
#' }
#'
#' @section Data voorbereiding:
#' Invoerdata moet voldoen aan het 1CHO formaat (EV-bestand). Het package
#' verwacht dat gebruikers hun data laden met \code{read.csv2()} of vergelijkbare
#' functie en de resulterende data frame doorgeven aan de pipeline functies.
#'
#' @references
#' Npuls CEDA (Centre for Educational Data Analytics). Web: https://edu.nl/twt84
#'
#' @name prep1cho-package
#' @aliases prep1cho
NULL
