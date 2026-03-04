# Suppress R CMD check notes about tidyverse NSE
# These are column names used in dplyr/tidyr operations

#' @importFrom stats setNames
#' @importFrom utils read.csv read.table
NULL

utils::globalVariables(c(
  # Common column names from 1CHO data
  "INS_Studentnummer", "INS_Inschrijvingsjaar", "INS_Opleidingscode",
  "INS_Opleidingsnaam", "INS_Opleidingsvorm", "INS_Opleidingsvorm_naam",
  "INS_Opleidingsvorm_code", "OPL_Code_in_jaar",
  "OPL_Code_historisch", "OPL_Code_actueel", "OPL_Opleidingsnaam_CROHO",
  "OPL_Opleidingsnaam_CROHO_in_jaar", "OPL_Opleidingsnaam_CROHO_actueel",
  "OPL_Academisch_jaar", "OPL_Instellingscode", "OPL_Nominale_studielast_EC_aantal",
  "OPL_Nominale_studieduur", "OPL_Fase", "Graad", "Datum_begin_opleiding",
  "Datum_einde_opleiding", "Datum_einde_instroom", "Code_stand_record",
  "OPL_Academisch_jaar_einde_opleiding", "Opvolgende_jaren", "temp_max_jaar",
  "volgend_jaar", "inschrijvingsjaar", "datum_uitslag_1_oktober", "geboortejaar",

  # Enrollment data columns
  "INS_Postcode_student_1okt_peildatum", "INS_Postcode_student_voor_HO",
  "INS_Datum_inschrijving", "INS_Datum_uitschrijving", "INS_Datum_tekening_diploma",
  "INS_Inschrijvingsjaar_EOI", "INS_Inschrijvingsjaar_EI",
  "INS_Indicatie_eerste_jaars_opleiding_en_instelling",
  "INS_Hoogste_vooropleiding_jaar_1CHO", "INS_Hoogste_vooropleiding_BRIN_1CHO",
  "INS_Verblijfsjaren_wetenschappelijk_onderwijs_vanaf_0_cat",
  "INS_Verblijfsjaren_hoger_onderwijs_vanaf_0_cat",
  "INS_Verblijfsjaar_type_onderwijs_binnen_HO",
  "INS_Diplomajaar", "INS_Opleidingsfase_actueel_naam",
  "INS_Vooropleiding_voor_HO_code", "INS_Vooropleiding_voor_HO_profiel",
  "INS_Vooropleiding_voor_HO_profiel_standaard",
  "INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO",
  "INS_Vooropleiding_voor_HO_profiel_standaard_NT",
  "INS_Vooropleiding_voor_HO_profiel_standaard_NG",
  "INS_Vooropleiding_voor_HO_profiel_standaard_EM",
  "INS_Vooropleiding_voor_HO_profiel_standaard_CM",
  "INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_combinatieprofiel",
  "INS_Vooropleiding_voor_HO_profiel_standaard_afk",
  "INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_afk",
  "INS_Vooropleiding_voor_HO_profiel_standaard_zonder_combinatie",
  "INS_Vooropleiding_voor_HO_profiel_standaard_alleen_VWO_zonder_combinatie",

  # Calculated variables
  "INS_Studiejaar", "INS_Tussenjaren_binnen_opleiding", "INS_Inschrijvingsjaar_is_EOI",
  "INS_Uitschrijving_voor_1_feb", "INS_Uitschrijving_voor_1_feb_EOI",
  "INS_Herinschrijving_jaar_2_na_uitschrijving_voor_1_feb",
  "INS_Studiejaar_gecorrigeerd_uitschrijving_1_feb_EOI",
  "INS_Direct", "INS_Indicatie_Tussenjaar_voor_B", "INS_Indicatie_Tussenjaar_voor_P",
  "INS_Indicatie_Tussenjaar_voor_M", "INS_Indicatie_Tussenjaar",
  "INS_Aansluiting", "INS_Aantal_inschrijvingen", "INS_Diploma",
  "INS_Aantal_inschrijvingen_tot_diploma", "INS_Eerste_datum_inschrijving",
  "INS_Laatste_datum_uitschrijving", "INS_Tijd_tot_diploma_in_maanden",
  "INS_Inschrijvingsjaar_max", "INS_Actief_in_max_jaar", "INS_Uitval",
  "INS_Uitwonend", "INS_Indicatie_voltijd",
  "INS_Aantal_inschrijvingen_jaar_instelling",
  "INS_Aantal_EOI_inschrijvingen_jaar_instelling",
  "INS_Dubbele_studie_instelling",

  # Success variables
  "SUC_Type_uitstroom", "SUC_Type_uitstroom_studiejaar",
  "SUC_Uitval_switch_studie", "SUC_Uitval_switch_studiejaar",
  "SUC_Uitval_switch_binnen_instelling_aantal_jaar",
  "SUC_Instroom_switch_instelling", "SUC_Type_uitstroom_incl_switch",
  "SUC_Type_uitstroom_studiejaar_incl_switch",
  "INS_Jaar_na_uitval",

  # Common mapping/processing variables
  "CURRENT", "to", "from", "from1", "from2",

  # Generic dplyr/tidyr variables
  ".", "n", "Veldnaam", "Veldtype", "p_na", "lower", "upper",
  "pattern", "Uniek", "Kolomwaarden"
))
