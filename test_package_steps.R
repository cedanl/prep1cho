# Test Package - Step by Step
# Test prep1cho package stap voor stap

# Load package
message("Loading prep1cho package...")
devtools::load_all()

message("\n========================================")
message("Testing Pipeline - Step by Step")
message("========================================\n")

# Load test data
enrollments <- read.csv2("data/00_raw/EV299XX24_DEMO_decoded.csv")
message("Loaded enrollments: ", format(nrow(enrollments), big.mark = ","), " rows")

# Step 1: Download RIO data
message("\n[1/5] Downloading RIO data...")
rio_data <- download_rio(force_download = FALSE)
message("  RIO rows: ", format(nrow(rio_data), big.mark = ","))

# Step 2: Audit enrollments
message("\n[2/5] Auditing enrollments...")
audit_result <- audit_enrollments(enrollments)
enrollments_clean <- audit_result$data
message("  Empty columns: ", length(audit_result$report$empty_cols))
message("  High NA columns: ", length(audit_result$report$high_na_cols))

# Step 3: Prepare RIO data
message("\n[3/5] Preparing RIO data...")
rio_prepared <- prepare_rio(
  rio_data,
  year = 2024,
  institution_brin = "21XX",
  create_synthetic = TRUE
)
message("  RIO prepared rows: ", format(nrow(rio_prepared$rio), big.mark = ","))
message("  RIO per jaar rows: ", format(nrow(rio_prepared$rio_per_jaar), big.mark = ","))

# Step 4: Prepare enrollments
message("\n[4/5] Preparing enrollments...")
enrollments_prep <- enrollments_clean |>
  prepare_enrollments_mapping() |>
  prepare_enrollments_supplemental(year = 2024, institution_brin = "21XX")
message("  Columns after mapping: ", ncol(enrollments_prep))

# Step 5: Combine with RIO
message("\n[5/5] Combining with RIO data...")
enrollments_combined <- enrollments_prep |>
  combine_enrollments_rio(rio_prepared$rio_per_jaar) |>
  combine_enrollments_calculations() |>
  combine_enrollments_final()

# Final results
message("\n========================================")
message("Pipeline Complete!")
message("========================================")
message("Final rows: ", format(nrow(enrollments_combined), big.mark = ","))
message("Final columns: ", ncol(enrollments_combined))

message("\n========================================")
message("Test Complete!")
message("========================================")
message("\nData available in: enrollments_combined")
message("\nUsers can save data themselves:")
message("  write.csv2(enrollments_combined, 'output/enrollments.csv', row.names = FALSE)")
message("  fst::write_fst(enrollments_combined, 'output/enrollments.fst')")
message("  saveRDS(enrollments_combined, 'output/enrollments.rds')")
