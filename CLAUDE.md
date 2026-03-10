# 1CHO Preparation - Claude Configuration

## Project Overview
R data pipeline for preparing 1CijferHO enrollment data for visualization dashboards.
Part of Npuls CEDA (Centre for Educational Data Analytics).

---

## ⚠️ CRITICAL: Data Quality Validation (MANDATORY)

**ALWAYS run these checks after `utils/build.R` - pipelines without errors can still produce WRONG data!**

### After Every Build - Check for Silent Failures
```r
# Run this IMMEDIATELY after build.R completes:
output <- fst::read_fst("data/03_combined/prepared_enrollments.fst")

# Check NA percentages - HIGH NA% = likely mapping failure
na_summary <- sapply(output, function(x) round(mean(is.na(x)) * 100, 1))
na_high <- na_summary[na_summary > 50]
if (length(na_high) > 0) {
  print("⚠️ VARIABLES WITH >50% NA - INVESTIGATE:")
  print(sort(na_high, decreasing = TRUE))
}
```

### Root Cause: How Mapping Failures Happen
`mapping_translate()` uses `left_join()` - if source value NOT in mapping table → result is NA.
- Source has codes (`1`, `2`) but mapping expects names (`Voltijd`, `Deeltijd`) → all NA
- Source has different format (`man` vs `M`) → all NA
- Wrong mapping file used → all NA

### When Source File Changes - Full Checklist
1. **Create source dictionary**: `utils/dev/create_source_dictionary.R`
2. **Compare actual VALUES**: Are they codes? Names? Different format?
3. **Check mapping tables**: Do `from` values match source values exactly?
4. **Verify NA patterns**: Source has data but output NA → mapping failure

---

## Directory Structure
- `00_download/` - Download CROHO reference data
- `01_audit/` - Validate raw data integrity
- `02_prepare/` - Transform and enrich data (3 stages)
- `03_combine/` - Integrate datasets and add calculated fields (2 stages)
- `04_export/` - Generate output files
- `data/` - Data directories (raw, audited, prepared, combined, exported)
- `metadata/` - Mapping tables, data dictionaries, assertions
- `utils/` - Shared utilities and helper functions
- `test/` - Testing and exploratory scripts

## Development Workflow
- **Environment setup**: `source("utils/00_set_up_environment.R")` (also handles package management via renv)
- **Full pipeline**: `source("utils/build.R")`
- **Configuration**: `config.yml` (default = synthetic data, vu = VU-specific)

## Code Conventions
- **Simplicity first**: Choose simple, clean, understandable code over clever solutions
  - Prefer clarity over brevity
  - Write code that others can understand at a glance
  - Avoid unnecessary abstractions or complex patterns
  - If you can solve it in 5 lines clearly, don't use 3 lines cryptically
- **Section headers**: `## ++++...++++` pattern (100+ chars)
- **Variable naming**: Dutch prefixes (INS_*, OPL_*, DEM_*)
- **Comments**: English
- **Line length**: 120 characters (.lintr)
- **Error handling**: Use `rlang::abort()` over `stop()`

## Key Dependencies
- **Data I/O**: fst (fast storage), LaF (ASCII files), readr
- **Pipeline**: tidyverse, config
- **Utilities**: Custom mapping functions (see R/utils.R)

## Data Pipeline
- **Input**: 1CHO raw enrollments + CROHO program data + mapping tables
- **Output**: Enriched dataset for Tableau visualization
- **Pattern**: audit → prepare → combine → export

## Configuration System
- `config.yml`: Institution-specific settings (year, paths, mappings)
- `renviron.csv`: System variables for paths
- Default config uses synthetic test data

## Validation
- Data assertions: `metadata/assertions/`
- Audit reports: dataReporter package

## Commit
- Proactively suggest /code-review or /commit action after completing a plan

## Slash Commands
- Based on plugins from the Claude Marketplace advise installation and usage if applicable
