# Upload the reviewed import (data/import_sets.csv and
# data/import_training_max.csv, written by scripts/import_workbook.R) to the
# Google Sheet. Rows already in the Sheet (same set_id, or same round + lift
# for training maxes) are skipped, so it is safe to run again.
#
#   Rscript scripts/upload_import.R

suppressPackageStartupMessages(library(dplyr))
for (file in c("R/defaults.R", "R/db.R")) source(file)

sets <- readr::read_csv("data/import_sets.csv", col_types = readr::cols(.default = "c"))
db_write_sets(sets |> filter(!set_id %in% db_read_sets()$set_id))

training_max <- readr::read_csv("data/import_training_max.csv", col_types = readr::cols(.default = "c")) |>
  mutate(round = as.numeric(round))
db_write_training_max(training_max |> anti_join(db_read_training_max(), by = c("round", "lift")))
