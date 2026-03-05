# Test Package - Full Pipeline
# Test prep1cho package met run_pipeline()

# Load package
message("Loading prep1cho package...")
devtools::load_all()

message("\n========================================")
message("Testing Full Pipeline")
message("========================================\n")

# Load test data
enrollments <- read.csv2("synth_data/EV299XX24_DEMO_decoded.csv")
message("Loaded enrollments: ", format(nrow(enrollments), big.mark = ","), " rows")

# Run full pipeline
data <- run_pipeline(
  enrollments = enrollments,
  year = 2024,
  institution_brin = "21XX",
  create_synthetic = TRUE,
  download_rio = FALSE  # Use bundled or cached RIO data
)

# Check results
message("\n========================================")
message("Pipeline Results")
message("========================================")
message("\nData available in: data")
message("\nUsers can save data themselves:")
message("  write.csv2(data, 'output/enrollments.csv', row.names = FALSE)")
message("  fst::write_fst(data, 'output/enrollments.fst')")
message("  saveRDS(data, 'output/enrollments.rds')")
