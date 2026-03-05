# Prepare Functions
# Transform and enrich enrollment data

#' Bereid RIO referentiedata voor
#'
#' Transformeert ruwe RIO (Register Instellingen en Opleidingen) data naar
#' bruikbaar formaat met datumvelden, status codes, en opleidingsnamen met
#' niveau prefix (B/M/AD).
#'
#' @param rio Data frame met ruwe RIO data
#' @param year Academisch jaar voor maximale datum. Standaard: 2024
#' @param institution_brin BRIN code van de instelling. Standaard: "21XX"
#' @param create_synthetic Logisch. Maak synthetische test rows? Standaard: TRUE
#'
#' @return Lijst met:
#'   \item{rio}{Voorbereide RIO data}
#'   \item{rio_per_jaar}{RIO data per academisch jaar}
#'
#' @keywords internal
prepare_rio <- function(rio,
                       year = 2024,
                       institution_brin = "21XX",
                       create_synthetic = TRUE) {

  # Use provided parameters
  config_year <- year

  # Convert date fields
  rio_prepared <- rio |>
    dplyr::mutate(
      Datum_begin_opleiding = suppressWarnings(lubridate::as_date(lubridate::ymd_hms(Datum_begin_opleiding))),
      Datum_einde_opleiding = suppressWarnings(lubridate::as_date(lubridate::ymd_hms(Datum_einde_opleiding))),
      Datum_einde_instroom = suppressWarnings(lubridate::as_date(lubridate::ymd_hms(Datum_einde_instroom)))
    )

  # Derive Code_stand_record from dates
  rio_prepared <- rio_prepared |>
    dplyr::mutate(
      Code_stand_record = dplyr::case_when(
        # Future: start date is in the future
        Datum_begin_opleiding > lubridate::today() ~ "TOEKOMST",
        # Historical: end date is in the past
        !is.na(Datum_einde_opleiding) & Datum_einde_opleiding < lubridate::today() ~ "HISTORISCH",
        !is.na(Datum_einde_instroom) & Datum_einde_instroom < lubridate::today() ~ "HISTORISCH",
        # Current: everything else
        TRUE ~ "ACTUEEL"
      )
    )

  # Create synthetic rows if needed
  if (create_synthetic) {
    synthetic_rows <- rio_prepared |>
      dplyr::filter(OPL_Instellingscode == "21PL") |>
      dplyr::mutate(OPL_Instellingscode = "21XX")

    rio_prepared <- rio_prepared |>
      dplyr::bind_rows(synthetic_rows)
  }

  # Filter and transform
  rio_prepared <- rio_prepared |>
    # Get programmes from institution
    dplyr::filter(OPL_Instellingscode == institution_brin) |>
    dplyr::mutate(
      # Change field types
      OPL_Code_in_jaar = as.integer(OPL_Code_in_jaar),
      OPL_Nominale_studielast_EC_aantal = as.integer(OPL_Nominale_studielast_EC_aantal),
      # Study load per year
      OPL_Nominale_studieduur = as.integer(OPL_Nominale_studielast_EC_aantal / 60),
      # Add level prefix (B/M/AD)
      OPL_Opleidingsnaam_CROHO = dplyr::case_when(
        Graad %in% c("BACHELOR", "Bachelor") ~ paste0("B ", OPL_Opleidingsnaam_CROHO),
        Graad %in% c("MASTER", "Master") ~ paste0("M ", OPL_Opleidingsnaam_CROHO),
        Graad %in% c("ASSOCIATE_DEGREE", "Associate Degree") ~ paste0("AD ", OPL_Opleidingsnaam_CROHO),
        TRUE ~ OPL_Opleidingsnaam_CROHO
      )
    )

  # Read mapping table for CROHO code translation
  map_croho <- utils::read.csv2(
    system.file("metadata/mapping_tables/Mapping_OPL_Code_in_jaar_OPL_Code_historisch.csv",
                package = "prep1cho"),
    stringsAsFactors = FALSE
  )
  # Development mode fallback
  if (system.file(package = "prep1cho") == "") {
    map_croho <- utils::read.csv2("metadata/mapping_tables/Mapping_OPL_Code_in_jaar_OPL_Code_historisch.csv",
                                  stringsAsFactors = FALSE)
  }

  rio_prepared <- rio_prepared |>
    vusa::mapping_translate("OPL_Code_in_jaar", "OPL_Code_historisch",
                           mapping_table_input = map_croho)

  # Create per-year version
  rio_per_jaar <- rio_prepared |>
    dplyr::mutate(
      OPL_Academisch_jaar = academic_year(Datum_begin_opleiding),
      OPL_Academisch_jaar_einde_opleiding = academic_year(Datum_einde_opleiding),
      OPL_Academisch_jaar_einde_opleiding = pmin(OPL_Academisch_jaar_einde_opleiding, config_year)
    ) |>
    dplyr::group_by(OPL_Code_in_jaar, OPL_Instellingscode, INS_Opleidingsvorm, OPL_Academisch_jaar) |>
    dplyr::arrange(dplyr::desc(Datum_begin_opleiding)) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    # Get current codes and names
    dplyr::group_by(OPL_Code_historisch, OPL_Instellingscode) |>
    dplyr::arrange(Datum_begin_opleiding) |>
    dplyr::mutate(
      OPL_Code_actueel = dplyr::last(OPL_Code_in_jaar),
      OPL_Opleidingsnaam_CROHO_actueel = dplyr::last(OPL_Opleidingsnaam_CROHO)
    ) |>
    dplyr::ungroup() |>
    # Fill in missing years
    dplyr::group_by(OPL_Code_in_jaar, OPL_Instellingscode, INS_Opleidingsvorm) |>
    dplyr::mutate(
      temp_max_jaar = ifelse(
        all(Code_stand_record == "HISTORISCH"),
        max(OPL_Academisch_jaar_einde_opleiding),
        config_year
      ),
      volgend_jaar = dplyr::lead(OPL_Academisch_jaar - 1, default = max(temp_max_jaar))
    ) |>
    dplyr::mutate(
      Opvolgende_jaren = purrr::map2(OPL_Academisch_jaar, volgend_jaar, ~ seq(.x, .y))
    ) |>
    tidyr::unnest(Opvolgende_jaren) |>
    dplyr::select(-c(volgend_jaar, OPL_Academisch_jaar, temp_max_jaar)) |>
    dplyr::rename(OPL_Academisch_jaar = Opvolgende_jaren) |>
    dplyr::ungroup() |>
    dplyr::rename(OPL_Opleidingsnaam_CROHO_in_jaar = OPL_Opleidingsnaam_CROHO)

  return(list(
    rio = rio_prepared,
    rio_per_jaar = rio_per_jaar
  ))
}


#' Pas mapping tables toe op inschrijvingsgegevens
#'
#' Vertaalt ruwe veldwaarden naar leesbare labels met behulp van mapping tables.
#'
#' @param enrollments Data frame met ruwe inschrijvingsgegevens
#'
#' @return Data frame met gemapte waarden
#' @export
#'
#' @examples
#' \dontrun{
#'   enrollments <- read.csv2("data.csv")
#'   enrollments_mapped <- prepare_enrollments_mapping(enrollments)
#' }
prepare_enrollments_mapping <- function(enrollments) {

  # Helper function to read mapping table from package
  read_mapping <- function(filename) {
    path <- system.file("metadata/mapping_tables", filename, package = "prep1cho")
    if (path == "") {
      # Development mode - use relative path
      path <- file.path("metadata/mapping_tables", filename)
    }
    utils::read.csv2(path, stringsAsFactors = FALSE)
  }

  # Read all mapping tables
  map_geslacht <- read_mapping("Mapping_DEM_Geslacht_code_DEM_Geslacht_naam.csv")
  map_opleidingsvorm <- read_mapping("Mapping_INS_Opleidingsvorm_code_INS_Opleidingsvorm_naam.csv")
  map_opleidingsfase <- read_mapping("Mapping_INS_Opleidingsfase_actueel_code_INS_Opleidingsfase_actueel_naam.csv")
  map_actief <- read_mapping("Mapping_INS_Indicatie_actief_op_peildatum_code_INS_Indicatie_actief_op_peildatum_omschrijving.csv")
  map_examen <- read_mapping("Mapping_INS_Examenresultaat_code_INS_Examenresultaat_omschrijving.csv")
  map_eerstejaars_cat <- read_mapping("Mapping_INS_Indicatie_eerste_jaars_instelling_INS_Indicatie_eerste_jaars_instelling_Cat.csv")
  map_eerstejaars_naam <- read_mapping("Mapping_INS_Indicatie_eerste_jaars_instelling_INS_Indicatie_eerste_jaars_instelling_naam.csv")
  map_eerstejaars_opl <- read_mapping("Mapping_INS_Indicatie_eerste_jaars_opleiding_en_instelling_INS_Indicatie_eerste_jaars_opleiding_en_instelling_naam.csv")
  map_nationaliteit <- read_mapping("Mapping_DEM_Nationaliteit_code_DEM_Nationaliteit_naam.csv")
  map_leeftijd <- read_mapping("Mapping_DEM_Leeftijd_cat.csv")
  map_inschrijving <- read_mapping("Mapping_INS_Soort_inschrijving_1CHO_code_INS_Soort_inschrijving_1CHO_cat.csv")
  map_verblijfsjaren <- read_mapping("Mapping_INS_Verblijfsjaren_wetenschappelijk_onderwijs_cat.csv")
  map_vooropleiding_profiel <- read_mapping("Mapping_INS_Vooropleiding_code_INS_Vooropleiding_naam.csv")
  map_profiel_afk <- read_mapping("Mapping_INS_Profiel_omschrijving_Profiel_afkorting.csv")

  # Apply mappings
  enrollments_mapped <- enrollments |>
    vusa::mapping_translate("DEM_Geslacht_code", "DEM_Geslacht_naam",
                           mapping_table_input = map_geslacht) |>
    vusa::mapping_translate("INS_Opleidingsvorm_code", "INS_Opleidingsvorm_naam",
                           mapping_table_input = map_opleidingsvorm) |>
    vusa::mapping_translate("INS_Opleidingsfase_actueel_code", "INS_Opleidingsfase_actueel_naam",
                           mapping_table_input = map_opleidingsfase) |>
    vusa::mapping_translate("INS_Indicatie_actief_op_peildatum_code", "INS_Indicatie_actief_op_peildatum_omschrijving",
                           mapping_table_input = map_actief) |>
    vusa::mapping_translate("INS_Examenresultaat_code", "INS_Examenresultaat_omschrijving",
                           mapping_table_input = map_examen) |>
    vusa::mapping_translate("INS_Indicatie_eerste_jaars_instelling", "INS_Indicatie_eerste_jaars_instelling_cat",
                           mapping_table_input = map_eerstejaars_cat) |>
    vusa::mapping_translate("INS_Indicatie_eerste_jaars_instelling", "INS_Indicatie_eerste_jaars_instelling_naam",
                           mapping_table_input = map_eerstejaars_naam) |>
    vusa::mapping_translate("INS_Indicatie_eerste_jaars_opleiding_en_instelling", "INS_Indicatie_eerste_jaars_opleiding_en_instelling_naam",
                           mapping_table_input = map_eerstejaars_opl) |>
    vusa::mapping_translate("DEM_Nationaliteit_1", "DEM_Nationaliteit_1_naam",
                           mapping_table_input = map_nationaliteit) |>
    vusa::mapping_translate("DEM_Nationaliteit_2", "DEM_Nationaliteit_2_naam",
                           mapping_table_input = map_nationaliteit) |>
    vusa::mapping_translate("DEM_Nationaliteit_3", "DEM_Nationaliteit_3_naam",
                           mapping_table_input = map_nationaliteit) |>
    vusa::mapping_category("DEM_Leeftijd_peildatum_1_jan", "DEM_Leeftijd_peildatum_1_jan_cat",
                          mapping_table_input = map_leeftijd) |>
    vusa::mapping_translate("INS_Soort_inschrijving_1CHO_code", "INS_Soort_inschrijving_1CHO_cat",
                           mapping_table_input = map_inschrijving) |>
    vusa::mapping_category("DEM_Leeftijd_peildatum_1_oktober", "DEM_Leeftijd_peildatum_1_oktober_cat",
                          mapping_table_input = map_leeftijd) |>
    vusa::mapping_category("INS_Verblijfsjaren_wetenschappelijk_onderwijs", "INS_Verblijfsjaren_wetenschappelijk_onderwijs_vanaf_0_cat",
                          mapping_table_input = map_verblijfsjaren) |>
    vusa::mapping_category("INS_Verblijfsjaren_hoger_onderwijs", "INS_Verblijfsjaren_hoger_onderwijs_vanaf_0_cat",
                          mapping_table_input = map_verblijfsjaren) |>
    vusa::mapping_translate("INS_Vooropleiding_voor_HO_code", "INS_Vooropleiding_voor_HO_profiel",
                           mapping_table_input = map_vooropleiding_profiel)

  return(enrollments_mapped)
}


#' Bereid inschrijvingen voor - supplemental fields
#'
#' Voeg aanvullende berekende velden toe aan inschrijvingsgegevens.
#'
#' @param enrollments Data frame met gemapte inschrijvingsgegevens
#' @param year Academisch jaar voor configuratie
#' @param institution_brin BRIN code van de instelling
#' @return Data frame met supplemental fields
#' @keywords internal
prepare_enrollments_supplemental <- function(enrollments, year, institution_brin) {

  enrollments <- enrollments |>
    dplyr::mutate(
      # Living away from home indicator
      INS_Uitwonend = dplyr::if_else(
        INS_Postcode_student_1okt_peildatum == INS_Postcode_student_voor_HO,
        FALSE,
        TRUE
      ),
      # Full-time indicator
      INS_Indicatie_voltijd = dplyr::if_else(
        INS_Opleidingsvorm_code == 1,
        TRUE,
        FALSE
      )
    )

  # Double study detection
  enrollments <- enrollments |>
    dplyr::group_by(INS_Studentnummer, INS_Inschrijvingsjaar,
                    INS_Datum_inschrijving, INS_Datum_uitschrijving) |>
    dplyr::mutate(
      INS_Aantal_inschrijvingen_jaar_instelling = length(unique(OPL_code_historisch)),
      INS_Aantal_EOI_inschrijvingen_jaar_instelling = sum(
        INS_Indicatie_eerste_jaars_opleiding_en_instelling == 1
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      INS_Dubbele_studie_instelling = INS_Aantal_inschrijvingen_jaar_instelling > 1
    )

  # Study year calculations
  enrollments <- enrollments |>
    dplyr::group_by(INS_Studentnummer, OPL_code_historisch) |>
    dplyr::arrange(INS_Studentnummer, OPL_code_historisch, INS_Inschrijvingsjaar) |>
    dplyr::mutate(
      INS_Studiejaar = dplyr::dense_rank(INS_Inschrijvingsjaar),
      INS_Tussenjaren_binnen_opleiding = length(min(INS_Inschrijvingsjaar):max(INS_Inschrijvingsjaar)) - dplyr::n(),
      INS_Inschrijvingsjaar_is_EOI = INS_Inschrijvingsjaar == INS_Inschrijvingsjaar_EOI
    ) |>
    dplyr::ungroup()

  # Deregistration calculations
  enrollments <- enrollments |>
    dplyr::mutate(
      INS_Uitschrijving_voor_1_feb = INS_Datum_uitschrijving <=
        lubridate::as_date(paste0(INS_Inschrijvingsjaar + 1, "-01-31")),
      INS_Uitschrijving_voor_1_feb_EOI =
        INS_Inschrijvingsjaar == INS_Inschrijvingsjaar_EOI &
        INS_Uitschrijving_voor_1_feb
    ) |>
    dplyr::group_by(INS_Studentnummer, OPL_code_historisch) |>
    dplyr::mutate(
      INS_Herinschrijving_jaar_2_na_uitschrijving_voor_1_feb =
        sum(INS_Uitschrijving_voor_1_feb_EOI, na.rm = TRUE) > 0 &
        max(INS_Studiejaar) > 1
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      INS_Studiejaar_gecorrigeerd_uitschrijving_1_feb_EOI = dplyr::if_else(
        INS_Herinschrijving_jaar_2_na_uitschrijving_voor_1_feb & INS_Studiejaar > 1,
        INS_Studiejaar - 1,
        INS_Studiejaar
      )
    )

  # Gap year indicators
  enrollments <- enrollments |>
    dplyr::mutate(
      INS_Direct = (INS_Hoogste_vooropleiding_jaar_1CHO + 1) == INS_Inschrijvingsjaar_EOI,
      INS_Indicatie_Tussenjaar_voor_B = (
        INS_Hoogste_vooropleiding_jaar_1CHO + 1 < INS_Inschrijvingsjaar_EOI &
          OPL_Fase == "B" &
          INS_Verblijfsjaren_wetenschappelijk_onderwijs == 1 &
          INS_Verblijfsjaren_hoger_onderwijs == 1
      ),
      INS_Indicatie_Tussenjaar_voor_P = (
        INS_Hoogste_vooropleiding_jaar_1CHO + 1 < INS_Inschrijvingsjaar_EOI &
          OPL_Fase == "S" &
          INS_Verblijfsjaar_type_onderwijs_binnen_HO <= 1
      ),
      INS_Indicatie_Tussenjaar_voor_M = (
        INS_Hoogste_vooropleiding_jaar_1CHO + 1 < INS_Inschrijvingsjaar_EOI &
          OPL_Fase == "M" &
          INS_Verblijfsjaar_type_onderwijs_binnen_HO <= 1
      )
    )

  # Generalize gap year indicators
  enrollments <- enrollments |>
    dplyr::group_by(INS_Studentnummer, OPL_code_historisch) |>
    dplyr::mutate(
      INS_Indicatie_Tussenjaar_voor_B = INS_Indicatie_Tussenjaar_voor_B[dplyr::first(which(INS_Studiejaar == 1))],
      INS_Indicatie_Tussenjaar_voor_P = INS_Indicatie_Tussenjaar_voor_P[dplyr::first(which(INS_Studiejaar == 1))],
      INS_Indicatie_Tussenjaar_voor_M = INS_Indicatie_Tussenjaar_voor_M[dplyr::first(which(INS_Studiejaar == 1))],
      INS_Direct = INS_Direct[dplyr::first(which(INS_Studiejaar == 1))]
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      INS_Indicatie_Tussenjaar = dplyr::coalesce(
        INS_Indicatie_Tussenjaar_voor_B,
        INS_Indicatie_Tussenjaar_voor_P,
        INS_Indicatie_Tussenjaar_voor_M
      )
    )

  # Connection type
  enrollments <- enrollments |>
    dplyr::mutate(
      INS_Aansluiting = dplyr::case_when(
        INS_Direct & INS_Hoogste_vooropleiding_BRIN_1CHO == institution_brin ~
          "Direct after diploma institution",
        INS_Direct ~ "Direct after external diploma",
        INS_Indicatie_Tussenjaar == TRUE ~ "Gap year",
        INS_Verblijfsjaar_type_onderwijs_binnen_HO > INS_Studiejaar ~ "Switch / Second study",
        .default = "Unknown"
      )
    )

  # Study success indicators
  enrollments <- enrollments |>
    dplyr::group_by(INS_Studentnummer, OPL_code_historisch) |>
    dplyr::mutate(
      INS_Aantal_inschrijvingen = dplyr::n(),
      INS_Diploma_temp = suppressWarnings(max(INS_Diplomajaar, na.rm = TRUE)),
      INS_Diploma = dplyr::if_else(is.infinite(INS_Diploma_temp), NA_integer_, as.integer(INS_Diploma_temp)),
      INS_Datum_tekening_diploma_temp = suppressWarnings(max(INS_Datum_tekening_diploma, na.rm = TRUE)),
      INS_Datum_tekening_diploma = dplyr::case_when(
        is.infinite(INS_Datum_tekening_diploma_temp) ~ lubridate::as_date(NA),
        TRUE ~ lubridate::as_date(INS_Datum_tekening_diploma_temp)
      ),
      INS_Aantal_inschrijvingen_tot_diploma = dplyr::if_else(
        !is.na(INS_Datum_tekening_diploma),
        dplyr::n(),
        NA_integer_
      ),
      INS_Eerste_datum_inschrijving = min(INS_Datum_inschrijving, na.rm = TRUE),
      INS_Laatste_datum_uitschrijving = max(INS_Datum_uitschrijving, na.rm = TRUE),
      INS_Tijd_tot_diploma_in_maanden = dplyr::case_when(
        INS_Datum_tekening_diploma <= INS_Eerste_datum_inschrijving ~ 0,
        !is.na(INS_Datum_tekening_diploma) ~
          round(lubridate::time_length(
            lubridate::interval(INS_Eerste_datum_inschrijving, INS_Datum_tekening_diploma),
            "months"
          ), 0),
        .default = NA_real_
      ),
      INS_Inschrijvingsjaar_max = max(INS_Inschrijvingsjaar),
      INS_Actief_in_max_jaar = INS_Inschrijvingsjaar_max == year,
      INS_Uitval = dplyr::if_else(
        is.na(INS_Datum_tekening_diploma) & !INS_Actief_in_max_jaar,
        TRUE,
        FALSE
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-INS_Diploma_temp, -INS_Datum_tekening_diploma_temp)

  # VWO/HAVO profile calculations
  enrollments <- enrollments |>
    dplyr::mutate(
      # Clean and standardize profile names
      INS_Vooropleiding_voor_HO_profiel_standaard =
        stringr::str_replace_all(
          INS_Vooropleiding_voor_HO_profiel,
          c("VO " = "", "vwo profiel " = "", "havo profiel " = "",
            "havo " = "", "algemeen" = "", "profiel " = "",
            " en " = " & ", " \\+ " = " & ")
        ),
      # Keep only valid profiles (containing cultuur/economie/natuur)
      INS_Vooropleiding_voor_HO_profiel_standaard = dplyr::if_else(
        stringr::str_detect(
          INS_Vooropleiding_voor_HO_profiel_standaard,
          "cultuur|economie|natuur"
        ),
        INS_Vooropleiding_voor_HO_profiel_standaard,
        NA_character_
      ),
      # Remove invalid profiles (mbo/vmbo/vbo)
      INS_Vooropleiding_voor_HO_profiel_standaard = dplyr::if_else(
        stringr::str_detect(INS_Vooropleiding_voor_HO_profiel_standaard, "mbo|vmbo|vbo"),
        NA_character_,
        INS_Vooropleiding_voor_HO_profiel_standaard
      ),
      # VWO profiles only
      INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO =
        dplyr::if_else(
          stringr::str_detect(INS_Vooropleiding_voor_HO_profiel, "vwo"),
          INS_Vooropleiding_voor_HO_profiel_standaard,
          NA_character_
        ),
      # Individual profile indicators
      INS_Vooropleiding_voor_HO_profiel_standaard_NT =
        stringr::str_detect(INS_Vooropleiding_voor_HO_profiel, "techniek"),
      INS_Vooropleiding_voor_HO_profiel_standaard_NG =
        stringr::str_detect(INS_Vooropleiding_voor_HO_profiel, "gezondheid"),
      INS_Vooropleiding_voor_HO_profiel_standaard_EM =
        stringr::str_detect(INS_Vooropleiding_voor_HO_profiel, "maatschappij"),
      INS_Vooropleiding_voor_HO_profiel_standaard_CM =
        stringr::str_detect(INS_Vooropleiding_voor_HO_profiel, "cultuur"),
      # Combination profile indicator
      INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_combinatieprofiel =
        stringr::str_detect(INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO, "/")
    )

  # Helper function to read mapping table
  read_mapping <- function(filename) {
    path <- system.file("metadata", "mapping_tables", filename, package = "prep1cho")
    if (!file.exists(path)) {
      rlang::abort(paste0("Mapping table not found: ", filename))
    }
    data.table::fread(path, encoding = "Latin-1")
  }

  # Read profile abbreviation mapping
  map_profiel_afk <- read_mapping("Mapping_INS_Profiel_omschrijving_Profiel_afkorting.csv")

  # Map profiles to abbreviations
  enrollments <- enrollments |>
    vusa::mapping_translate("INS_Vooropleiding_voor_HO_profiel_standaard",
                           "INS_Vooropleiding_voor_HO_profiel_standaard_afk",
                           mapping_table_input = map_profiel_afk) |>
    vusa::mapping_translate("INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO",
                           "INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_afk",
                           mapping_table_input = map_profiel_afk)

  # Define profile levels and ignored combinations
  profile_levels <- c("NT", "NG", "NT & NG", "EM", "CM", "EM & CM")
  profile_ignored <- c("NG & CM", "NG & EM", "NT & EM", "NT & CM")

  # Create without-combination variants
  enrollments <- enrollments |>
    dplyr::mutate(
      # Variable for VWO and HAVO
      INS_Vooropleiding_voor_HO_profiel_standaard_zonder_combinatie = dplyr::if_else(
        INS_Vooropleiding_voor_HO_profiel_standaard_afk %in% profile_ignored,
        NA_character_,
        INS_Vooropleiding_voor_HO_profiel_standaard_afk
      ),
      INS_Vooropleiding_voor_HO_profiel_standaard_zonder_combinatie = factor(
        INS_Vooropleiding_voor_HO_profiel_standaard_zonder_combinatie,
        levels = profile_levels
      ),
      # Variable only VWO
      INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_zonder_combinatie = dplyr::if_else(
        INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_afk %in% profile_ignored,
        NA_character_,
        INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_afk
      ),
      INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_zonder_combinatie = factor(
        INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_zonder_combinatie,
        levels = profile_levels
      )
    )

  return(enrollments)
}
