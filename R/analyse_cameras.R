# analyse_cameras.R ------------------------------------------------------
# 1. Activate renv (if not already initialised)
#if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
#renv::activate()   # does nothing if already active

setwd("C:/git/camtrap-service")
# 2. Load functions and packages
source("R/camera_ops_functions.R")
import::here("readr", read_csv, write_csv)

# 3. Ingest data  --------------------------------------------------------
csv_in  <- "dat/hr_service_23_24.csv"
dat_raw <- read_csv(csv_in, show_col_types = FALSE)

# 4. Analyse -------------------------------------------------------------
result <- analyse_cameras(dat_raw)

services  <- result$services
op_matrix <- result$op_matrix

# 5. Persist outputs  ----------------------------------------------------
write_csv(services,  "service_summary.csv")
write_csv(op_matrix, "operation_matrix.csv")

message("✓ Wrote service_summary.csv and operation_matrix.csv")
