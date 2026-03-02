


# optional hash_var vvmover
# make codebook
# filter on relative year
# do.call(lines, c(list(x = x, y = y), line_options))


test2 = function(x = rnorm(20), y = rnorm(20), ..., line_options){
  plot(x, y, ...)
  if (missing(line_options)) {
    lines(x, y)
  } else {
    do.call(lines, c(list(x = x, y = y), line_options))
  }
}

# Hash the data


write_file_proj_out <- function(df,
                                name = NULL,
                                make_codebook = TRUE,
                                codebook_location = Sys.getenv("METADATA_DICTIONARY_END_DIR"),
                                hash_cols = Sys.getenv("MAIN_IDENTIFIER"),
                                filter_years = TRUE,
                                filter_year_col = "INS_Inschrijvingsjaar",
                                filter_num_years = 10,
                                settings_df = write_config_proj(),
                                settings_type = NULL,
                                sub_dir = NA_character_,
                                base_dir = NULL,
                                dir = NULL,
                                add_branch = NULL,
                                full_dir = NULL,
                                extensions = NULL,
                                extra_ext = NULL,
                                rds_version = 3,
                                csv_na = "",
                                csv_sep = ";",
                                csv_dec = ",",
                                fst_compress = 100,
                                ...) {

  write_file_proj(...)
}

# object, hash_var, make_codebook,




write_file_proj <- function(
    object,
    name = NULL,
    settings_df = write_config_proj(),
    settings_type = NULL,
    sub_dir = NA_character_,
    base_dir = NULL,
    dir = NULL,
    add_branch = NULL,
    full_dir = NULL,
    extensions = NULL,
    extra_ext = NULL,
    rds_version = 3,
    csv_na = "",
    csv_sep = ";",
    csv_dec = ",",
    fst_compress = 100,
    ...
)

# write_file_out
# make codebook
# hash_var





write_tableau <- function(df, name, output_path, current_year, student_number_col = "INS_Studentnummer",
                         enrolment_year_col = "INS_Inschrijvingsjaar",
                         mapping_file = "Documentatie_Tableau_vriendelijke_variabelnamen_UT.csv",
                         save_csv = TRUE, save_rds = FALSE, offset_years = 10){

makeCodebook(INS_Inschrijvingen_1CHO_VUdata,
             # Title of the report
             reportTitle = paste0("Quality report ", "INS_Inschrijvingen_1CHO_VUdata_cookbook"),
             # The name of the file
             file = paste0(
               getwd(),
               "/metadata/data_dictionary_end/",
               "INS_Inschrijvingen_1CHO_VUdata",
               ".Rmd"
             ),
             # what kind of output
             output = "html",
             # replace old file
             replace = TRUE,
             # output to be generated and saved
             render = TRUE)
}
