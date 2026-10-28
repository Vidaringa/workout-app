# One-off import of the old Excel workbook (rounds 1-5) into the `sets` format.
#
# Usage (from the project root):
#   Rscript scripts/import_workbook.R ["path/to/wendler_2026_1.xlsx"]
#
# Writes, without touching the Google Sheet:
#   data/import_sets.csv          rows for the `sets` tab
#   data/import_training_max.csv  rows for the `training_max` tab
#   data/import_review.csv        every parsed row next to its original cell text,
#                                 with a `flags` column for anything guessed
# Check the review file, fix import_sets.csv by hand if needed, then upload
# both with scripts/upload_import.R.

suppressPackageStartupMessages(library(dplyr))
library(readxl)

for (file in c("R/defaults.R", "R/engine_rounds.R", "R/parse_actuals.R", "R/db.R")) source(file)

args <- commandArgs(trailingOnly = TRUE)
workbook <- if (length(args) > 0) args[1] else list.files(".", "^wendler.*\\.xlsx$")[1]
if (is.na(workbook) || !file.exists(workbook)) stop("Workbook not found; pass its path as the first argument.")

import_rounds <- 1:5
# No dates exist in the workbook. Estimated from round 5 week 1 Monday being
# 2026-10-26 and three-week rounds back to back; every row is flagged
# date_estimated = TRUE.
round1_start <- as.Date("2026-08-03")
day_offset <- c(mon = 0, tue = 1, wed = 2, thu = 3, fri = 4, sat = 5, sun = 6)
day_codes <- c("Mánudagur" = "mon", "Miðvikudagur" = "wed", "Fimmtudagur" = "thu", "Föstudagur" = "fri")
tm_labels <- c("Deadlift" = "deadlift", "Bekkur" = "bench", "OHP" = "ohp", "Front squat" = "front_squat")
main_exercise <- c(deadlift = "Deadlift", bench = "Bench press", ohp = "OHP")
exercise_names <- c(
  "Hanging leg raises / plank" = "Hanging leg raises",
  "Standing Calf Raise (3 sec niður)" = "Standing calf raise",
  "Overhead cable triceps extensoin" = "Overhead cable triceps extension"
)

blank <- function(x) is.na(x) || trimws(x) == ""

# "1 til 2", "6 til 8", "10-15 / 45s", "3x5", "2x8-12", "5", Excel date serials.
parse_planned <- function(x) {
  x <- clean_cell(x)
  out <- list(sets = NA_real_, reps_min = NA_real_, reps_max = NA_real_)
  if (is.na(x)) return(out)
  x <- trimws(sub("/.*$", "", gsub(" til ", "-", x)))
  if (grepl("^\\d+\\s*x\\s*", x)) {
    out$sets <- as.numeric(sub("^(\\d+)\\s*x.*$", "\\1", x))
    x <- sub("^\\d+\\s*x\\s*", "", x)
  }
  if (grepl("^\\d+(\\s*-\\s*\\d+)?$", x)) {
    reps <- as.numeric(strsplit(x, "\\s*-\\s*")[[1]])
    out$reps_min <- min(reps)
    out$reps_max <- max(reps)
  }
  out
}

is_serial <- function(x) !blank(x) && grepl("^\\d{5}(\\.0+)?$", trimws(x))

parse_sheet <- function(path, sheet, round) {
  cells <- read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text",
                      .name_repair = "minimal")
  cells <- cells[, 1:7]
  names(cells) <- LETTERS[1:7]
  # A shifted header row put "Actual kg" etc. into data rows (round 5, week 2).
  for (col in c("E", "F", "G")) {
    cells[[col]][cells[[col]] %in% c("Actual kg", "Actual reps", "Notes")] <- NA
  }

  tms <- cells[2:5, ] |>
    filter(A %in% names(tm_labels)) |>
    transmute(lift = unname(tm_labels[A]), tm = as.numeric(B))
  tm_of <- rlang::set_names(tms$tm, tms$lift)

  records <- list()
  week <- NA
  day <- NA
  lift <- NA
  section <- "main"
  set1_pct <- NA
  block_accessories <- list()

  for (i in seq_len(nrow(cells))) {
    row <- as.list(cells[i, ])
    label <- trimws(dplyr::coalesce(row$A, ""))
    has_actual <- !blank(row$E) || !blank(row$F)

    if (grepl("^WEEK \\d", label)) {
      week <- as.integer(sub("^WEEK (\\d).*$", "\\1", label))
      next
    }
    weekday <- names(day_codes)[startsWith(label, names(day_codes))]
    if (length(weekday) == 1) {
      day <- unname(day_codes[weekday])
      lift <- dplyr::case_when(
        grepl("Deadlift", label) ~ "deadlift",
        grepl("Bekk", label) ~ "bench",
        grepl("Axlir|OPH|OHP", label) ~ "ohp"
      )
      section <- "main"
      set1_pct <- NA
      block_accessories <- list()
      next
    }
    if (label %in% c("Set", "Max") || is.na(week)) next
    if (startsWith(label, "Auka")) {
      section <- "accessory"
      next
    }

    row_flags <- character()
    rep_cells <- c(row$C, row$D, row$F)
    if (any(purrr::map_lgl(rep_cells, is_serial))) row_flags <- c(row_flags, "rep range was an Excel date")
    planned_kg <- NA_real_
    planned <- list(sets = 1, reps_min = NA_real_, reps_max = NA_real_)
    kg_text <- row$E
    reps_text <- row$F
    exercise <- NA
    row_lift <- NA
    set_type <- NA

    main_type <- dplyr::case_when(
      grepl("^Warm-up \\d", label) ~ "warmup",
      grepl("^Set \\d", label) ~ "work",
      grepl("^Top set", label) ~ "top",
      grepl("^Joker", label) ~ "joker",
      grepl("^back-off", label, ignore.case = TRUE) ~ "backoff"
    )

    if (section == "main" && !is.na(main_type)) {
      pct <- suppressWarnings(as.numeric(row$B))
      if (label == "Set 1") set1_pct <- pct
      if (main_type == "backoff") pct <- set1_pct
      exercise <- main_exercise[[lift]]
      row_lift <- lift
      set_type <- main_type
      planned_kg <- round(pct * tm_of[[lift]], 3)
      planned <- parse_planned(row$D)
      if (is.na(planned$sets)) planned$sets <- 1
    } else if (grepl("^Bench press warmup|^secondary bench", label, ignore.case = TRUE)) {
      exercise <- "Bench press (secondary)"
      set_type <- if (grepl("warmup", label)) "warmup" else "work"
      planned_kg <- suppressWarnings(as.numeric(row$C))
      planned <- parse_planned(row$D)
    } else if (grepl("^Front squat", label)) {
      exercise <- "Front squat"
      row_lift <- "front_squat"
      set_type <- if (grepl("As needed", dplyr::coalesce(row$C, ""))) "warmup" else "work"
      planned <- parse_planned(dplyr::coalesce(row$D, if (grepl("3x5", label)) "3x5" else NA))
    } else if (section == "accessory") {
      set_type <- "accessory"
      by_effort <- grepl("choose by effort", dplyr::coalesce(row$C, ""))
      if (by_effort) {
        planned <- parse_planned(row$D)
      } else {
        planned <- parse_planned(row$C)
        planned$sets <- suppressWarnings(as.numeric(row$B))
        # In the old layout the unlabelled D column held the actual reps, unless
        # it has decimals ("23.5-27-27"), which can only be kg.
        if (blank(reps_text) && !blank(row$D)) {
          if (blank(kg_text) && grepl("\\d\\.[1-9]", clean_cell(row$D))) {
            kg_text <- row$D
            row_flags <- c(row_flags, "kg read from the reps column (has decimals)")
          } else {
            reps_text <- row$D
          }
        }
        has_actual <- has_actual || !blank(row$D)
      }
      if (label != "") {
        exercise <- dplyr::coalesce(exercise_names[label], label)
        block_accessories[[length(block_accessories) + 1]] <- list(exercise = exercise, logged = has_actual, planned = planned)
      } else if (has_actual) {
        unlogged <- purrr::keep(block_accessories, \(a) !a$logged)
        guess <- if (length(unlogged)) unlogged[[1]] else block_accessories[[length(block_accessories)]]
        exercise <- guess$exercise
        planned <- guess$planned
        row_flags <- c(row_flags, "row had no exercise name; exercise guessed")
      }
    } else if (has_actual) {
      exercise <- label
      set_type <- "work"
      row_flags <- c(row_flags, "unrecognised row")
    }

    if (!has_actual || is.na(exercise)) next
    if (is.na(planned$sets)) planned$sets <- 1

    parsed <- parse_actuals(kg_text, reps_text, planned_sets = planned$sets)
    if (nrow(parsed) == 0) next
    records[[length(records) + 1]] <- parsed |>
      mutate(
        sheet = sheet, excel_row = i, round = round, week = week, day = day,
        exercise = exercise, lift = row_lift, set_type = set_type,
        planned_kg = planned_kg, planned_reps = planned$reps_min, planned_reps_max = planned$reps_max,
        raw_label = row$A, raw_planned = paste(c(row$B, row$C, row$D), collapse = " | "),
        raw_actual_kg = row$E, raw_actual_reps = row$F, raw_notes = row$G,
        flags = purrr::map_chr(flags, \(f) paste(c(row_flags, f[f != ""]), collapse = "; "))
      )
  }

  list(
    sets = bind_rows(records),
    training_max = tms |> mutate(round = round)
  )
}

parsed <- purrr::map(import_rounds, \(r) parse_sheet(workbook, paste("round", r), r))

round_start <- function(round) round1_start + 21 * (round - 1)

review <- purrr::map(parsed, "sets") |>
  bind_rows() |>
  mutate(
    date = round_start(round) + 7 * (week - 1) + day_offset[day],
    notes = purrr::map2_chr(raw_notes, note, \(a, b) {
      parts <- c(a, b)
      parts <- parts[!is.na(parts) & parts != ""]
      if (length(parts)) paste(parts, collapse = "; ") else NA_character_
    }),
    set_id = sprintf("imp-r%d-w%d-%s-row%03d-%d", round, week, day, excel_row, set_no)
  )

sets <- review |>
  transmute(
    set_id, date = format(date), round, week, day, exercise, lift, set_no, set_type,
    planned_kg, planned_reps, planned_reps_max, actual_kg, actual_reps,
    rir = NA_real_, notes, logged_at = NA_character_, date_estimated = TRUE
  )
stopifnot(identical(names(sets), names(db_schema$sets)))

training_max <- purrr::map(parsed, "training_max") |>
  bind_rows() |>
  transmute(round, lift, tm, method = if_else(round == 1, "manual", "standard"),
            decided_at = format(round_start(round)))

dir.create("data", showWarnings = FALSE)
readr::write_csv(sets, "data/import_sets.csv", na = "")
readr::write_csv(training_max, "data/import_training_max.csv", na = "")
review |>
  transmute(sheet, excel_row, date = format(date), week, day, exercise, set_type, set_no,
            raw_label, raw_planned, raw_actual_kg, raw_actual_reps, raw_notes,
            actual_kg, actual_reps, planned_kg, planned_reps, planned_reps_max, notes, flags) |>
  readr::write_csv("data/import_review.csv", na = "")
