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

enrollments_start <- read_file_proj("enrollments")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. MODIFY ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

enrollments <- enrollments_start %>%
  distinct() %>%
  filter(!is.na(INS_Studentnummer))

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.1 Rename verrijkte kolommen (Avans feature request data) ####

# De bron bevat al verrijkte kolommen (_label, _naam, _cat suffixes) die normaal
# via mapping tables worden aangemaakt. Hier renamen we ze naar standaard namen.
# enrollments <- enrollments %>%
#   rename(
#     DEM_Geslacht_naam = geslacht_label,
#     INS_Opleidingsvorm_naam = opleidingsvorm_label,
#     INS_Opleidingsfase_actueel_naam = opleidingsfase_actueel_label,
#     INS_Indicatie_actief_op_peildatum_omschrijving = indicatie_actief_op_peildatum_label,
#     DEM_Nationaliteit_1_naam = nationaliteit_1_omschrijving_nationaliteit,
#     DEM_Nationaliteit_2_naam = nationaliteit_2_omschrijving_nationaliteit,
#     DEM_Nationaliteit_3_naam = nationaliteit_3_omschrijving_nationaliteit,
#     INS_Indicatie_eerste_jaars_instelling_naam = indicatie_eerstejaars_actuele_instelling_label,
#     INS_Hoogste_vooropleiding_soort_1CHO = hoogste_vooropleiding_cat,
#     DEM_Leeftijd_peildatum_1_oktober_cat = leeftijd_per_peildatum_1_oktober_cat
#   )

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.2 Recoding ####

enrollments <- enrollments %>%
  # Convert to date so we can find the min and max date
  mutate(
    INS_Datum_inschrijving = as.Date(
      INS_Datum_inschrijving,
      format = "%d/%m/%Y",
      tryFormats = c("%d/%m/%Y", "%d-%m-%Y")),
    INS_Datum_uitschrijving = as.Date(
      INS_Datum_uitschrijving,
      format = "%d/%m/%Y",
      tryFormats = c("%d/%m/%Y", "%d-%m-%Y")
      ))

# Make Indicator variables Boolean
enrollments <- enrollments %>%
  # Convert the Indicator variables with "J" and "N" to boolean variables
  mutate_at(vars(DEM_Indicatie_internationale_student, DEM_Indicatie_nationaliteit_EER_actueel,
                 DEM_Indicatie_nationaliteit_EER_peildatum), ~if_else(. == "J", TRUE, FALSE))


## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.3 Mapping tables ####

# Fill in missing data type of previous education
# Replace NAs with 0 in the previous education codes
enrollments <- enrollments %>%
  mutate(
    INS_Vooropleiding_binnen_HO_code = replace_na(INS_Vooropleiding_binnen_HO_code, 0),
    INS_Vooropleiding_voor_HO_code = replace_na(INS_Vooropleiding_voor_HO_code, 0),
    INS_Hoogste_vooropleiding_code_1CHO = replace_na(INS_Hoogste_vooropleiding_code_1CHO, 0)
  )

# Zero-pad binnen_HO codes for _naam mapping (expects 5-digit codes like "00607")
enrollments <- enrollments %>%
  mutate(INS_Vooropleiding_binnen_HO_code_padded = sprintf("%05d", as.integer(INS_Vooropleiding_binnen_HO_code))) %>%
  mapping_translate(
    "INS_Vooropleiding_binnen_HO_code_padded",
    "INS_Vooropleiding_binnen_HO_sector",
    mapping_table_name = "Mapping_INS_Vooropleiding_code_INS_Vooropleiding_naam"
  ) %>%
  select(-INS_Vooropleiding_binnen_HO_code_padded)

enrollments <- enrollments %>%
  mutate(INS_Hoogste_vooropleiding_code_1CHO = as.numeric(INS_Hoogste_vooropleiding_code_1CHO)) %>%
  mapping_translate(
    "INS_Hoogste_vooropleiding_code_1CHO",
    "INS_Hoogste_vooropleiding_soort_1CHO",
    mapping_table_name = "Mapping_INS_Vooropleiding_code_INS_Vooropleiding_cat"
  )

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Vooropleiding_voor_HO_code",
    "INS_Vooropleiding_voor_HO_profiel",
    mapping_table_name = "Mapping_INS_Vooropleiding_code_INS_Vooropleiding_naam"
  )

# Convert to numeric for _cat mapping (strips leading zeros: "00174" -> 174)
enrollments <- enrollments %>%
  mutate(INS_Vooropleiding_voor_HO_code_numeric = as.numeric(INS_Vooropleiding_voor_HO_code)) %>%
  mapping_translate(
    "INS_Vooropleiding_voor_HO_code_numeric",
    "INS_Vooropleiding_voor_HO_omschrijving",
    mapping_table_name = "Mapping_INS_Vooropleiding_code_INS_Vooropleiding_cat"
  ) %>%
  select(-INS_Vooropleiding_voor_HO_code_numeric)


enrollments <- enrollments %>%
  mapping_translate(
    "INS_Vooropleiding_binnen_HO_code",
    "INS_Vooropleiding_binnen_HO_soort",
    mapping_table_name = "Mapping_INS_Vooropleiding_code_INS_Vooropleiding_cat"
  )

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Hoogste_vooropleiding_code_1CHO",
    "INS_Hoogste_vooropleiding_nieuw_cat",
    mapping_table_name = "Mapping_INS_Vooropleiding_code_INS_Vooropleiding_cat"
  )

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Vooropleiding_voor_HO_BRIN",
    "INS_Vooropleiding_voor_HO_naam",
    mapping_table_name = "Mapping_BRIN_4_nummer_INS_Instellingsnaam"
  )

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Vooropleiding_binnen_HO_BRIN",
    "INS_Vooropleiding_binnen_HO_naam",
    mapping_table_name = "Mapping_BRIN_4_nummer_INS_Instellingsnaam"
  )

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Hoogste_vooropleiding_BRIN_1CHO",
    "INS_Hoogste_vooropleiding_instellingsnaam",
    mapping_table_name = "Mapping_BRIN_4_nummer_INS_Instellingsnaam"
  )

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Vooropleiding_voor_HO_BRIN",
    "INS_Vooropleiding_voor_HO_postcode",
    mapping_table_name = "Mapping_BRIN_4_nummer_INS_Postcode"
  ) %>%
  mutate(INS_Vooropleiding_voor_HO_postcode = as.double(INS_Vooropleiding_voor_HO_postcode))

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Vooropleiding_binnen_HO_BRIN",
    "INS_Vooropleiding_binnen_HO_postcode",
    mapping_table_name = "Mapping_BRIN_4_nummer_INS_Postcode"
  ) %>%
  mutate(INS_Vooropleiding_binnen_HO_postcode = as.double(INS_Vooropleiding_binnen_HO_postcode))

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Hoogste_vooropleiding_BRIN_1CHO",
    "INS_Hoogste_vooropleiding_postcode",
    mapping_table_name = "Mapping_BRIN_4_nummer_INS_Postcode"
  ) %>%
  mutate(INS_Hoogste_vooropleiding_postcode = as.double(INS_Hoogste_vooropleiding_postcode))

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Vooropleiding_voor_HO_BRIN",
    "INS_Vooropleiding_voor_HO_plaats",
    mapping_table_name = "Mapping_BRIN_4_nummer_INS_Plaats"
  )

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Vooropleiding_binnen_HO_BRIN",
    "INS_Vooropleiding_binnen_HO_plaats",
    mapping_table_name = "Mapping_BRIN_4_nummer_INS_Plaats"
  )

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Hoogste_vooropleiding_BRIN_1CHO",
    "INS_Hoogste_vooropleiding_plaats",
    mapping_table_name = "Mapping_BRIN_4_nummer_INS_Plaats"
  )

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Examenresultaat_code",
    "INS_Examenresultaat_omschrijving"
  )

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Indicatie_eerste_jaars_instelling",
    "INS_Indicatie_eerste_jaars_instelling_cat"
  )

# Uitgecomment: kolom al aanwezig via rename (indicatie_eerstejaars_actuele_instelling_label)
enrollments <- enrollments %>%
  mapping_translate(
    "INS_Indicatie_eerste_jaars_instelling",
    "INS_Indicatie_eerste_jaars_instelling_naam"
  )

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Indicatie_eerste_jaars_opleiding_en_instelling",
    "INS_Indicatie_eerste_jaars_opleiding_en_instelling_naam"
  )

# Uitgecomment: kolommen al aanwezig via rename (nationaliteit_*_omschrijving_nationaliteit)
enrollments <- enrollments %>%
  mapping_translate(
    "DEM_Nationaliteit_1",
    "DEM_Nationaliteit_1_naam",
    mapping_table_name = "Mapping_DEM_Nationaliteit_code_DEM_Nationaliteit_naam"
  )

enrollments <- enrollments %>%
  mapping_translate(
    "DEM_Nationaliteit_2",
    "DEM_Nationaliteit_2_naam",
    mapping_table_name = "Mapping_DEM_Nationaliteit_code_DEM_Nationaliteit_naam"
  )

enrollments <- enrollments %>%
  mapping_translate(
    "DEM_Nationaliteit_3",
    "DEM_Nationaliteit_3_naam",
    mapping_table_name = "Mapping_DEM_Nationaliteit_code_DEM_Nationaliteit_naam"
  )

# Because the univariate plots work per category, continuous variables
# like age are also transformed into categorical variables.
# This operation is also documented in the documentation and
# uses a mapping table.

enrollments <- mapping_category(
  enrollments,
  "DEM_Leeftijd_peildatum_1_jan",
  "DEM_Leeftijd_peildatum_1_jan_cat",
  mapping_table_name = "Mapping_DEM_Leeftijd_cat"
)

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Soort_inschrijving_1CHO_code",
    "INS_Soort_inschrijving_1CHO_cat"
  )

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Vooropleiding_binnen_HO_BRIN",
    "INS_Vooropleiding_binnen_HO_Instellingsnaam",
    mapping_table_name = "Mapping_BRIN_4_nummer_INS_Instellingsnaam"
  )

enrollments <- enrollments %>%
  mapping_translate(
    "INS_Vooropleiding_voor_HO_BRIN",
    "INS_Vooropleiding_voor_HO_Instellingsnaam",
    mapping_table_name = "Mapping_BRIN_4_nummer_INS_Instellingsnaam"
  )

# Uitgecomment: kolom al aanwezig via rename (indicatie_actief_op_peildatum_label)
enrollments <- mapping_translate(
  enrollments,
  "INS_Indicatie_actief_op_peildatum_code",
  "INS_Indicatie_actief_op_peildatum_omschrijving"
)

# Uitgecomment: kolom al aanwezig via rename (opleidingsfase_actueel_label)
enrollments <- mapping_translate(
  enrollments,
  "INS_Opleidingsfase_actueel_code",
  "INS_Opleidingsfase_actueel_naam"
)

# Uitgecomment: kolom al aanwezig via rename (opleidingsvorm_label)
enrollments <- mapping_translate(
  enrollments,
  "INS_Opleidingsvorm_code",
  "INS_Opleidingsvorm_naam"
)

# Uitgecomment: kolom al aanwezig via rename (leeftijd_per_peildatum_1_oktober_cat)
enrollments <- mapping_category(
  enrollments,
  "DEM_Leeftijd_peildatum_1_oktober",
  "DEM_Leeftijd_peildatum_1_oktober_cat",
  mapping_table_name = "Mapping_DEM_Leeftijd_cat"
)

# Uitgecomment: kolom al aanwezig via rename (geslacht_label)
enrollments <- mapping_translate(
  enrollments,
  "DEM_Geslacht_code",
  "DEM_Geslacht_naam"
)

# This variable leads to many categories because it's a number.
# For the univariate plots we therefore create a categorical variable from this.
enrollments <- enrollments %>%
  mapping_category(
    "INS_Verblijfsjaren_wetenschappelijk_onderwijs",
    "INS_Verblijfsjaren_wetenschappelijk_onderwijs_vanaf_0_cat",
    mapping_table_name = "Mapping_INS_Verblijfsjaren_wetenschappelijk_onderwijs_cat"
  )
enrollments <- enrollments %>%
  mapping_category(
    "INS_Verblijfsjaren_hoger_onderwijs",
    "INS_Verblijfsjaren_hoger_onderwijs_vanaf_0_cat",
    mapping_table_name = "Mapping_INS_Verblijfsjaren_wetenschappelijk_onderwijs_cat"
  )

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE & CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(enrollments, "enrollments_1")

clear_script_objects()
