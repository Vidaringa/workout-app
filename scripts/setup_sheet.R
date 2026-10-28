# One-time setup of the Google Sheet (see README, "Google Sheet setup").
# Needs GSHEET_ID and GOOGLE_SA_JSON in .Renviron.
#
#   Rscript scripts/setup_sheet.R
#
# Creates any missing tabs with their header row, and fills `settings` and
# `template` with the defaults if they are empty. Safe to run again.

suppressPackageStartupMessages(library(dplyr))
for (file in c("R/defaults.R", "R/db.R")) source(file)

db_setup_tabs()
if (nrow(db_read_settings()) == 0) db_write_settings(default_settings())
if (nrow(db_read_template()) == 0) db_write_template(default_template())

# Keys added to the defaults after the Sheet was set up are appended, so new
# settings show up without overwriting your edits.
missing_settings <- default_settings() |> filter(!key %in% db_read_settings()$key)
if (nrow(missing_settings) > 0) db_append("settings", missing_settings)
