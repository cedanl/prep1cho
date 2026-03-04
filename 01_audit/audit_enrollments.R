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

enrollments <- read_delim(config::get("data_1cho_enrollments_file_path"),
                          delim = ";",
                          col_types = cols(
                            .default = col_guess(),
                            `nationaliteit_3` = col_double(),
                            `datum_inschrijving` = col_date(format = "%Y%m%d"),
                            # Behoud leading zeros voor vooropleiding codes
                            `hoogste_vooropleiding_voor_het_ho_oorspronkelijke_code` = col_character(),
                            `hoogste_vooropleiding_binnen_het_ho` = col_character(),
                            `hoogste_vooropleiding_binnen_het_ho_oorspronkelijke_code` = col_character(),
                            `hoogste_vooropleiding` = col_character()))

enrollments_naming <- read_documentation("Documentatie_enrollments_avans_feature_requests.csv")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. ASSEERT ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# TODO Not yet able to run
# assert_naming(enrollments, enrollments_naming, "enrollments")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 3. MODIFY ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

enrollments <- wrapper_translate_colnames_documentation(
  enrollments,
  enrollments_naming
)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE-AND-CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_file_proj(enrollments)

clear_script_objects()
