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

rio_per_jaar <- read_file_proj("rio_per_jaar")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## 2. MODIFY ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
### 2.1 Add CROHO to enrollments ####

# TODO Not entirely sure about this. When I keep academic year and join additionally on
# INS_Inschrijvingsjaar and OPL_Academisch_jaar I get NA values with synthetic data, but maybe not
# with real data.
rio_per_jaar_enrollments <- rio_per_jaar %>%
  select(
    OPL_Opleidingsnaam_CROHO_actueel,
    OPL_Code_in_jaar,
    OPL_Nominale_studieduur,
    OPL_Academisch_jaar,
    OPL_Bekostiging,
    OPL_Bekostigingsduur,
    Relatie_soort
  ) %>%
  filter(OPL_Academisch_jaar >= config::get("first_year")) %>%
  select(-OPL_Academisch_jaar) %>% # Remove this line if you want to join on academic year
  group_by(OPL_Code_in_jaar) %>%
  mutate(OPL_Nominale_studieduur = min(OPL_Nominale_studieduur),
         OPL_Bekostigingsduur = min(OPL_Bekostigingsduur)) %>%
  distinct()

enrollments <- enrollments_start %>%
  left_join(croho_per_jaar_enrollments,
            by = c("OPL_Code_in_jaar" #, # remove this if you want to join
                   # on academic year as well
                   # "INS_Inschrijvingsjaar" = "OPL_Academisch_jaar"
                   )
  )


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## WRITE-AND-CLEAR ####
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


write_file_proj(enrollments, "enrollments_1")

clear_script_objects()
