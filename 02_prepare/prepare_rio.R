## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Npuls CEDA (Centre for Educational Data Analytics)
## Web Page: https://edu.nl/twt84
## Contact: corneel.denhartogh@surf.nl
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 1. READ ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Import all required files:

rio <- read_file_proj("rio")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. MODIFY ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rio <- rio %>%
  mutate(Datum_begin_opleiding = suppressWarnings(as_date(ymd_hms(Datum_begin_opleiding))),
         Datum_einde_opleiding = suppressWarnings(as_date(ymd_hms(Datum_einde_opleiding))),
         Datum_einde_instroom = suppressWarnings(as_date(ymd_hms(Datum_einde_instroom)))) %>%

  # Derive Code_stand_record from dates (not in RIO source, but needed for pipeline)
  mutate(Code_stand_record = case_when(
    # Future: start date is in the future
    Datum_begin_opleiding > today() ~ "TOEKOMST",
    # Historical: end date is in the past (either instroom or opleiding)
    !is.na(Datum_einde_opleiding) & Datum_einde_opleiding < today() ~ "HISTORISCH",
    !is.na(Datum_einde_instroom) & Datum_einde_instroom < today() ~ "HISTORISCH",
    # Current: everything else
    TRUE ~ "ACTUEEL"
  ))

# Create synthetic rows based on rows VU
if (Sys.getenv("R_CONFIG_ACTIVE") %in% c("synthetic", "default", "")) {

  synthetic_rows <- rio %>%
    filter(OPL_Instellingscode == "21PL") %>%
    mutate(OPL_Instellingscode = "21XX")

  rio <- rio %>%
    bind_rows(synthetic_rows)

}

rio <- rio %>%
  # Get programmes from institution
  filter(OPL_Instellingscode == config::get("metadata_institution_BRIN")) %>%
  mutate(
    # Change field types (including for linking programme table)
    OPL_Code_in_jaar = as.integer(OPL_Code_in_jaar),
    OPL_Nominale_studielast_EC_aantal = as.integer(OPL_Nominale_studielast_EC_aantal),
    # Create variable for study load per year
    OPL_Nominale_studieduur = as.integer(OPL_Nominale_studielast_EC_aantal / 60),
    # Add level prefix to program name (B/M/AD) to match original CROHO format
    OPL_Opleidingsnaam_CROHO = case_when(
      Graad %in% c("BACHELOR", "Bachelor") ~ paste0("B ", OPL_Opleidingsnaam_CROHO),
      Graad %in% c("MASTER", "Master") ~ paste0("M ", OPL_Opleidingsnaam_CROHO),
      Graad %in% c("ASSOCIATE_DEGREE", "Associate Degree") ~ paste0("AD ", OPL_Opleidingsnaam_CROHO),
      TRUE ~ OPL_Opleidingsnaam_CROHO
    )
  ) %>%
  mapping_translate("OPL_Code_in_jaar", "OPL_Code_historisch")


nMax_jaar <- config::get("year")

# The rio file only contains rows per change. To create it per year, we select the last
# change per year and then fill in missing years with the data from the last
# completed year
rio_per_jaar <- rio %>%
  mutate(
    OPL_Academisch_jaar = academic_year(Datum_begin_opleiding),
    OPL_Academisch_jaar_einde_opleiding = academic_year(Datum_einde_opleiding),
    OPL_Academisch_jaar_einde_opleiding = pmin(OPL_Academisch_jaar_einde_opleiding, nMax_jaar)
  ) %>%
  group_by(OPL_Code_in_jaar, OPL_Instellingscode, INS_Opleidingsvorm, OPL_Academisch_jaar) %>%
  arrange(desc(Datum_begin_opleiding)) %>%
  slice(1) %>%
  ungroup() %>%
  # First group this to get the correct current codes and names regardless of form
  # Institution code is important because historical programmes may differ for
  # different institutions
  group_by(OPL_Code_historisch, OPL_Instellingscode) %>%
  arrange(Datum_begin_opleiding) %>%
  # Determine most recent code and name
  mutate(OPL_Code_actueel = last(OPL_Code_in_jaar),
         OPL_Opleidingsnaam_CROHO_actueel = last(OPL_Opleidingsnaam_CROHO)) %>%
  ungroup() %>%
  #'*INFO* This now includes form, so not just CROHO per year, but CROHO_form_per_year
  group_by(OPL_Code_in_jaar, OPL_Instellingscode, INS_Opleidingsvorm) %>%
  mutate(
    temp_max_jaar = if_else(all(Code_stand_record == "HISTORISCH"),
                            max(OPL_Academisch_jaar_einde_opleiding),
                            nMax_jaar),
    volgend_jaar = lead(OPL_Academisch_jaar - 1, default = max(temp_max_jaar))) %>%
  mutate(Opvolgende_jaren = map2(OPL_Academisch_jaar, volgend_jaar, ~seq(.x, .y))) %>%
  unnest(Opvolgende_jaren) %>%
  # Remove temporary variables
  select(-c(volgend_jaar, OPL_Academisch_jaar, temp_max_jaar)) %>%
  rename(OPL_Academisch_jaar = Opvolgende_jaren) %>%
  ungroup() %>%
  rename(OPL_Opleidingsnaam_CROHO_in_jaar = OPL_Opleidingsnaam_CROHO)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE-AND-CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(rio)

write_file_proj(rio_per_jaar)

clear_script_objects()
