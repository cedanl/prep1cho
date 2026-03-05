# Combine Functions
# Combine and enrich datasets

#' Combineer inschrijvingsgegevens met RIO data
#'
#' Voegt RIO referentiedata toe aan inschrijvingsgegevens via left join.
#'
#' @param enrollments Data frame met voorbereide inschrijvingsgegevens
#' @param rio_per_jaar Data frame met RIO data per academisch jaar
#'
#' @return Data frame met verrijkte inschrijvingsgegevens
#' @export
combine_enrollments_rio <- function(enrollments, rio_per_jaar) {

  # Prepare RIO data for joining (following old project logic)
  # Select specific columns, aggregate by OPL_Code_in_jaar
  rio_for_join <- rio_per_jaar |>
    dplyr::select(
      OPL_Opleidingsnaam_CROHO_actueel,
      OPL_Code_in_jaar,
      OPL_Nominale_studieduur,
      OPL_Bekostiging,
      OPL_Bekostigingsduur
    ) |>
    dplyr::group_by(OPL_Code_in_jaar) |>
    dplyr::mutate(
      OPL_Nominale_studieduur = if (all(is.na(OPL_Nominale_studieduur))) {
        NA_real_
      } else {
        min(OPL_Nominale_studieduur, na.rm = TRUE)
      },
      OPL_Bekostigingsduur = if (all(is.na(OPL_Bekostigingsduur))) {
        NA_real_
      } else {
        min(OPL_Bekostigingsduur, na.rm = TRUE)
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::distinct()

  # Join with RIO data (only on OPL_Code_in_jaar, not on year or form)
  enrollments_combined <- enrollments |>
    dplyr::left_join(
      rio_for_join,
      by = "OPL_Code_in_jaar"
    )

  message("Combined enrollments with RIO data")
  message("  Rows: ", format(nrow(enrollments_combined), big.mark = ","))

  return(enrollments_combined)
}


#' Voeg berekende velden toe
#'
#' Berekent afgeleide indicatoren voor study success en switches.
#'
#' @param enrollments Data frame met gecombineerde inschrijvingsgegevens
#'
#' @return Data frame met toegevoegde berekende velden
#' @export
combine_enrollments_calculations <- function(enrollments) {

  # Calculate type of outflow based on dropout/diploma and study duration
  enrollments <- enrollments |>
    dplyr::group_by(INS_Studentnummer, OPL_code_historisch) |>
    dplyr::mutate(
      SUC_Type_uitstroom = dplyr::case_when(
        INS_Uitval == TRUE ~ paste0("Dropout year ", INS_Aantal_inschrijvingen),
        !is.na(INS_Datum_tekening_diploma) &
          INS_Aantal_inschrijvingen_tot_diploma == OPL_Nominale_studieduur ~
          "Nominal",
        !is.na(INS_Datum_tekening_diploma) &
          INS_Aantal_inschrijvingen_tot_diploma < OPL_Nominale_studieduur ~
          paste0(
            "Nominal - ",
            OPL_Nominale_studieduur - INS_Aantal_inschrijvingen_tot_diploma
          ),
        !is.na(INS_Datum_tekening_diploma) &
          INS_Aantal_inschrijvingen_tot_diploma > OPL_Nominale_studieduur ~
          paste0(
            "Nominal + ",
            INS_Aantal_inschrijvingen_tot_diploma - OPL_Nominale_studieduur
          ),
        .default = NA_character_
      ),
      SUC_Type_uitstroom_studiejaar = dplyr::case_when(
        is.na(SUC_Type_uitstroom) ~ "Still studying",
        INS_Aantal_inschrijvingen != INS_Studiejaar ~ "Still studying",
        is.na(INS_Datum_tekening_diploma) ~ "Dropout",
        .default = "Diploma"
      )
    ) |>
    dplyr::ungroup()

  # Detect switches within institution
  drop_out_student_numbers <- enrollments |>
    dplyr::filter(stringr::str_detect(SUC_Type_uitstroom, "Dropout")) |>
    dplyr::pull(INS_Studentnummer)

  switch_within_institution <- enrollments |>
    dplyr::select(
      INS_Studentnummer,
      OPL_Code_in_jaar,
      INS_Inschrijvingsjaar,
      INS_Inschrijvingsjaar_EOI,
      INS_Opleidingsfase_actueel_naam,
      INS_Studiejaar,
      SUC_Type_uitstroom,
      SUC_Type_uitstroom_studiejaar,
      INS_Inschrijvingsjaar_max
    ) |>
    dplyr::filter(INS_Studentnummer %in% drop_out_student_numbers) |>
    dplyr::mutate(
      INS_Jaar_na_uitval = dplyr::if_else(
        stringr::str_detect(SUC_Type_uitstroom, "Dropout"),
        INS_Inschrijvingsjaar_max + 1,
        NA_integer_
      )
    ) |>
    dplyr::group_by(INS_Studentnummer, INS_Opleidingsfase_actueel_naam) |>
    dplyr::filter(dplyr::n_distinct(INS_Inschrijvingsjaar_EOI) > 1) |>
    dplyr::mutate(
      SUC_Uitval_switch_studie = INS_Jaar_na_uitval %in% INS_Inschrijvingsjaar_EOI,
      SUC_Uitval_switch_studiejaar = SUC_Uitval_switch_studie &
        stringr::str_detect(SUC_Type_uitstroom_studiejaar, "Dropout"),
      SUC_Instroom_switch_instelling = INS_Inschrijvingsjaar_EOI %in%
        (INS_Inschrijvingsjaar_max + 1),
      SUC_Uitval_switch_binnen_instelling_aantal_jaar = dplyr::if_else(
        SUC_Uitval_switch_studie,
        INS_Jaar_na_uitval - INS_Inschrijvingsjaar_EOI,
        NA_integer_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      INS_Studentnummer,
      OPL_Code_in_jaar,
      INS_Inschrijvingsjaar,
      SUC_Uitval_switch_studie,
      SUC_Uitval_switch_studiejaar,
      SUC_Uitval_switch_binnen_instelling_aantal_jaar,
      SUC_Instroom_switch_instelling
    ) |>
    dplyr::distinct()

  enrollments <- enrollments |>
    dplyr::left_join(
      switch_within_institution,
      by = c("INS_Studentnummer", "OPL_Code_in_jaar", "INS_Inschrijvingsjaar")
    )

  # Create outflow variables including switch
  enrollments <- enrollments |>
    dplyr::mutate(
      SUC_Type_uitstroom_incl_switch = dplyr::case_when(
        stringr::str_detect(SUC_Type_uitstroom, "Dropout") &
          SUC_Uitval_switch_studie == TRUE ~ stringr::str_replace(
            SUC_Type_uitstroom,
            "Dropout",
            "Switch within institution after"
          ),
        stringr::str_detect(SUC_Type_uitstroom, "Dropout") &
          SUC_Uitval_switch_studie == FALSE ~ stringr::str_replace(
            SUC_Type_uitstroom,
            "Dropout",
            "Dropout from institution"
          ),
        .default = SUC_Type_uitstroom
      ),
      SUC_Type_uitstroom_studiejaar_incl_switch = dplyr::case_when(
        stringr::str_detect(SUC_Type_uitstroom_studiejaar, "Dropout") &
          SUC_Uitval_switch_studiejaar == TRUE ~ stringr::str_replace(
            SUC_Type_uitstroom_studiejaar,
            "Dropout",
            "Switch within institution"
          ),
        stringr::str_detect(SUC_Type_uitstroom_studiejaar, "Dropout") &
          SUC_Uitval_switch_studiejaar == FALSE ~ stringr::str_replace(
            SUC_Type_uitstroom_studiejaar,
            "Dropout",
            "Dropout from institution"
          ),
        .default = SUC_Type_uitstroom_studiejaar
      )
    )

  return(enrollments)
}


#' Finale transformaties
#'
#' Voert laatste datatype conversies en kolomselectie uit.
#'
#' @param enrollments Data frame met berekende inschrijvingsgegevens
#'
#' @return Data frame gereed voor export
#' @export
combine_enrollments_final <- function(enrollments) {

  # Add any final transformations
  enrollments_final <- enrollments |>
    dplyr::mutate(
      # Ensure proper data types
      INS_Inschrijvingsjaar = as.integer(INS_Inschrijvingsjaar)
    ) |>
    # Select and order columns
    dplyr::select(
      dplyr::everything()
    )

  return(enrollments_final)
}
