# Data-access layer. Everything the app reads or writes goes through the
# db_read_* / db_write_* functions at the bottom of this file, so the storage
# (a Google Sheet today) can be swapped for Postgres, SQLite or DuckDB by
# rewriting only this file.
#
# Reads are cached per R process and the cache is updated in place on every
# write, so the Sheet is read once per tab until db_refresh() is called.

# Column names and types of every tab: c = text, d = number, l = TRUE/FALSE.
# Dates and timestamps are stored as ISO text so the Sheet's locale can't
# reformat them.
db_schema <- list(
  settings = c(key = "c", value = "c", description = "c"),
  template = c(day = "c", day_label = "c", order = "d", exercise = "c", lift = "c",
               set_type = "c", scheme = "c", sets = "d", reps_min = "d", reps_max = "d",
               pct_w1 = "d", pct_w2 = "d", pct_w3 = "d", weight_kg = "d", rest_s = "d",
               notes = "c"),
  training_max = c(round = "d", lift = "c", tm = "d", method = "c", decided_at = "c"),
  sets = c(set_id = "c", date = "c", round = "d", week = "d", day = "c", exercise = "c",
           lift = "c", set_no = "d", set_type = "c", planned_kg = "d", planned_reps = "d",
           planned_reps_max = "d", actual_kg = "d", actual_reps = "d", rir = "d", notes = "c",
           logged_at = "c", date_estimated = "l"),
  runs = c(run_id = "c", date = "c", type = "c", distance_km = "d", ascent_m = "d",
           duration = "c", avg_hr = "d", rpe = "d", notes = "c", source = "c",
           external_id = "c", logged_at = "c"),
  bodyweight = c(date = "c", kg = "d"),
  ai_reviews = c(date = "c", model = "c", input_summary_json = "c", response_md = "c")
)

db_cache <- new.env(parent = emptyenv())

db_sheet_id <- function() {
  id <- Sys.getenv("GSHEET_ID")
  if (id == "") stop("GSHEET_ID is not set.")
  id
}

# Authenticate with the service account. GOOGLE_SA_JSON holds either the path
# to the key file or the key's JSON itself.
db_init <- function() {
  if (googlesheets4::gs4_has_token()) return(invisible(TRUE))
  options(googlesheets4_quiet = TRUE, gargle_quiet = TRUE)
  key <- Sys.getenv("GOOGLE_SA_JSON")
  if (key == "") stop("GOOGLE_SA_JSON is not set (service-account key file path or JSON).")
  googlesheets4::gs4_auth(path = key, scopes = "https://www.googleapis.com/auth/spreadsheets")
  invisible(TRUE)
}

# Give a data frame exactly the tab's columns, in order, with the right types.
db_coerce <- function(df, tab) {
  types <- db_schema[[tab]]
  df <- tibble::as_tibble(df)
  n <- nrow(df)
  cols <- purrr::imap(types, \(type, col) {
    value <- if (col %in% names(df)) df[[col]] else rep(NA, n)
    if (is.list(value)) value <- purrr::map_chr(value, \(v) if (length(v) == 0 || all(is.na(v))) NA_character_ else as.character(v[[1]]))
    switch(type,
      c = as.character(value),
      d = suppressWarnings(as.numeric(value)),
      l = as.logical(value)
    )
  })
  tibble::as_tibble(cols)
}

db_read <- function(tab, refresh = FALSE) {
  if (!refresh && exists(tab, envir = db_cache, inherits = FALSE)) {
    return(get(tab, envir = db_cache))
  }
  db_init()
  raw <- googlesheets4::read_sheet(db_sheet_id(), sheet = tab,
                                   col_types = paste(db_schema[[tab]], collapse = ""),
                                   na = c("", "NA"))
  df <- db_coerce(raw, tab)
  assign(tab, df, envir = db_cache)
  df
}

db_cache_set <- function(tab, df) {
  if (exists(tab, envir = db_cache, inherits = FALSE)) assign(tab, df, envir = db_cache)
}

db_append <- function(tab, rows) {
  rows <- db_coerce(rows, tab)
  if (nrow(rows) == 0) return(invisible(rows))
  db_init()
  googlesheets4::sheet_append(db_sheet_id(), rows, sheet = tab)
  if (exists(tab, envir = db_cache, inherits = FALSE)) {
    db_cache_set(tab, dplyr::bind_rows(get(tab, envir = db_cache), rows))
  }
  invisible(rows)
}

db_overwrite <- function(tab, df) {
  df <- db_coerce(df, tab)
  db_init()
  googlesheets4::sheet_write(df, db_sheet_id(), sheet = tab)
  assign(tab, df, envir = db_cache)
  invisible(df)
}

# Update the row whose `key` matches, or append it. The row position comes
# from the cached read, so don't sort or delete rows in the Sheet while the
# app is open (or press Refresh in Settings afterwards).
db_upsert <- function(tab, row, key) {
  current <- db_read(tab)
  row <- db_coerce(row, tab)
  idx <- match(row[[key]], current[[key]])
  if (is.na(idx)) return(db_append(tab, row))
  db_init()
  googlesheets4::range_write(db_sheet_id(), row, sheet = tab, range = paste0("A", idx + 1),
                             col_names = FALSE, reformat = FALSE)
  current[idx, ] <- row
  db_cache_set(tab, current)
  invisible(row)
}

# Create any missing tabs with their header row.
db_setup_tabs <- function() {
  db_init()
  existing <- googlesheets4::sheet_names(db_sheet_id())
  for (tab in setdiff(names(db_schema), existing)) {
    googlesheets4::sheet_write(db_coerce(tibble::tibble(), tab), db_sheet_id(), sheet = tab)
  }
  invisible(setdiff(names(db_schema), existing))
}

db_refresh <- function() rm(list = ls(db_cache), envir = db_cache)

db_read_settings <- function() db_read("settings")
db_read_template <- function() db_read("template")
db_read_training_max <- function() db_read("training_max")
db_read_sets <- function() db_read("sets")
db_read_runs <- function() db_read("runs")
db_read_bodyweight <- function() db_read("bodyweight")
db_read_ai_reviews <- function() db_read("ai_reviews")

db_write_settings <- function(settings) db_overwrite("settings", settings)
db_write_template <- function(template) db_overwrite("template", template)
db_write_training_max <- function(rows) db_append("training_max", rows)
db_write_set <- function(row) db_upsert("sets", row, "set_id")
db_write_sets <- function(rows) db_append("sets", rows)
db_write_run <- function(row) db_upsert("runs", row, "run_id")
db_write_runs <- function(rows) db_append("runs", rows)
db_write_bodyweight <- function(row) db_append("bodyweight", row)
db_write_ai_review <- function(row) db_append("ai_reviews", row)
