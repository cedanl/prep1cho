## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## R code for Npuls CEDA (Centre for Educational Data Analytics)
## Web Page: https://edu.nl/twt84
## Contact: corneel.denhartogh@surf.nl
##
##' *INFO*:
## 1) ___
##
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



download_rio <- function() {
  # Get the croho ascii file from DUO
  rio <- read.csv(
    url(
      "https://onderwijsdata.duo.nl/datastore/dump/28a4d89b-c223-4dbc-8deb-9d02a533f215?format=csv"
    ),
    na.strings = c("")
  )

  # Determine the Croho network directory
  raw_data_dir <- "data/00_raw/"
  # Determine the location where the zip file will be stored, with today's date in the filename.

  write.csv(rio,
            file = paste0(raw_data_dir, "rio_actueel.csv"),
            row.names = FALSE)

}
