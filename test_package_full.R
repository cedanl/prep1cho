# Test Package - Full Pipeline
# Test prep1cho package met run_pipeline()

# Load package
message("Loading prep1cho package...")
devtools::load_all()

message("\n========================================")
message("Testing Full Pipeline")
message("========================================\n")

# Load test data
enrollments <- read.csv2("data/00_raw/EV299XX24_DEMO_decoded.csv")
message("Loaded enrollments: ", format(nrow(enrollments), big.mark = ","), " rows")

# Run full pipeline
result <- run_pipeline(
  enrollments = enrollments,
  year = 2024,
  institution_brin = "21XX",
  create_synthetic = TRUE,
  download_rio = FALSE  # Use existing RIO data
)

# Check results
message("\n========================================")
message("Pipeline Results")
message("========================================")
message("Final rows: ", format(result$n_enrollments, big.mark = ","))
message("Final columns: ", result$n_columns)
message("\nAudit report:")
message("  Empty columns: ", length(result$audit_report$empty_cols))
message("  High NA columns: ", length(result$audit_report$high_na_cols))

message("\n========================================")
message("Test Complete!")
message("========================================")
message("\nData available in: result$data")
message("\nUsers can save data themselves:")
message("  write.csv2(result$data, 'output/enrollments.csv', row.names = FALSE)")
message("  fst::write_fst(result$data, 'output/enrollments.fst')")
message("  saveRDS(result$data, 'output/enrollments.rds')")
