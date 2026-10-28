# Default settings and program template. These seed the Google Sheet on setup;
# at runtime the app reads the Sheet and only falls back to these for keys
# that are missing there.

default_settings <- function() {
  tibble::tribble(
    ~key,                   ~value,               ~description,
    "progression_mode",     "conditional",        "standard / conditional / e1rm",
    "progression_lifts",    "deadlift,bench,ohp", "Lifts that get TM progression (others carry over)",
    "increment_deadlift",   "5",                  "TM increment per round (kg)",
    "increment_bench",      "2.5",                "TM increment per round (kg)",
    "increment_ohp",        "2.5",                "TM increment per round (kg)",
    "increment_front_squat", "0",                 "TM increment per round (kg)",
    "rounding_deadlift",    "2.5",                "Round planned weights to nearest (kg)",
    "rounding_bench",       "1",                  "Round planned weights to nearest (kg)",
    "rounding_ohp",         "1",                  "Round planned weights to nearest (kg)",
    "rounding_front_squat", "2.5",                "Round planned weights to nearest (kg)",
    "rounding_default",     "2.5",                "Rounding for anything without its own key",
    "conditional_min_rir",  "1",                  "Conditional mode: week-3 top set must hit all reps with at least this RIR (blank RIR counts as OK)",
    "reset_after_misses",   "2",                  "Conditional mode: suggest a reset after this many missed rounds in a row",
    "reset_pct",            "0.90",               "Reset TM to this share of smoothed e1RM",
    "e1rm_cap_pct",         "0.90",               "e1RM mode: TM capped at this share of smoothed e1RM",
    "deload_frequency",     "every_2",            "off / every / every_2 (4th week after the round)",
    "deload_type",          "deload",             "deload / tm_test",
    "week4_deload_pcts",    "0.70,0.80,0.90,1.00", "7th-week deload: %TM per set",
    "week4_deload_reps",    "5,5,1,1",            "7th-week deload: reps per set",
    "week4_tm_test_pcts",   "0.70,0.80,0.90,1.00", "7th-week TM test: %TM per set",
    "week4_tm_test_reps",   "5,5,5,3-5",          "7th-week TM test: reps per set",
    "tm_warn",              "0.92",               "TM health: warn above this TM / e1RM",
    "tm_flag",              "0.95",               "TM health: flag above this TM / e1RM",
    "e1rm_max_reps",        "10",                 "Only sets with at most this many reps feed e1RM",
    "trend_obs_sd",         "0.03",               "e1RM trend: measurement noise (log scale, ~3 %)",
    "trend_level_sd",       "0.005",              "e1RM trend: level noise per week (log scale)",
    "trend_slope_sd",       "0.002",              "e1RM trend: slope noise per week (log scale)",
    "trend_lower_bound_factor", "3",              "e1RM trend: sets without RIR below the trend pull it down with this x the noise",
    "accessory_increment",  "2.5",                "Double progression: add this once all sets hit the top of the range",
    "rest_timer",           "on",                 "on / off",
    "claude_model",         "claude-sonnet-5-5",  "Model for the weekly review",
    "review_weeks",         "6",                  "Weeks of history sent to the review",
    "goal_deadlift",        "200",                "e1RM goal (kg)",
    "goal_bench",           "100",                "e1RM goal (kg)",
    "goal_10k",             "0:50:00",            "10 km goal time",
    "goal_half",            "2:00:00",            "Half-marathon goal time",
    "gap_ascent_factor",    "8",                  "Grade-adjusted pace: 1 m of climb counts as this many metres of flat",
    "riegel_exponent",      "1.06",               "Riegel race-time exponent",
    "session_load_km",      "8",                  "Combined load: one lifting session counts as this many run km",
    "load_spike_ratio",     "1.3",                "Flag weeks whose load exceeds this x the previous 4-week average",
    "garmin_sync_days",     "90",                 "Garmin import (intervals.icu): how many days back the first sync looks",
    "timezone",             "Atlantic/Reykjavik", "Time zone for dates",
    "bodyweight_kg",        "",                   "Current bodyweight (optional)",
    "constraints",          "3 lifting sessions a week, max 60 min each (45 preferred). Avoids heavy squats because of the leg load from deadlifts and running.", "Shared with the AI review",
    "run_plan",             "Thursday 9-10 km (sometimes trail with 200-300 m ascent, otherwise hilly-flat ~70-100 m). Saturday 10-15 km.", "Shared with the AI review"
  )
}

default_template <- function() {
  main_lift <- function(day, day_label, exercise, lift, backoff_sets) {
    rows <- tibble::tribble(
      ~set_type, ~sets, ~reps_min, ~reps_max, ~pct_w1, ~pct_w2, ~pct_w3, ~rest_s, ~notes,
      "warmup",  1, 5, 5, 0.40, 0.40, 0.40,  60, "",
      "warmup",  1, 5, 5, 0.50, 0.50, 0.50,  60, "",
      "warmup",  1, 5, 5, 0.60, 0.60, 0.60,  90, "",
      "work",    1, 5, 5, 0.65, 0.70, 0.75, 120, "",
      "work",    1, 5, 5, 0.75, 0.80, 0.85, 150, "",
      "top",     1, 5, 5, 0.85, 0.90, 0.95, 180, "",
      "joker",   1, 1, 2, 0.90, 0.95,   NA, 180, "Optional",
      "joker",   1, 1, 2, 0.95, 1.00,   NA, 180, "Optional",
      "backoff", backoff_sets, 5, 5, 0.65, 0.70, 0.75, 120, "At Set 1 weight"
    )
    rows |>
      dplyr::filter(sets > 0) |>
      dplyr::mutate(day = day, day_label = day_label, exercise = exercise, lift = lift,
                    scheme = "pct", weight_kg = NA_real_)
  }

  other <- tibble::tribble(
    ~day,  ~exercise,                         ~lift,         ~set_type,   ~scheme,  ~sets, ~reps_min, ~reps_max, ~weight_kg, ~rest_s, ~notes,
    "mon", "Bench press (secondary)",         NA,            "warmup",    "fixed",  1,     5,         5,         20,         60,      "",
    "mon", "Bench press (secondary)",         NA,            "warmup",    "fixed",  1,     5,         5,         40,         60,      "",
    "mon", "Bench press (secondary)",         NA,            "work",      "fixed",  3,     5,         5,         60,         120,     "",
    "mon", "Chest-supported row",             NA,            "accessory", "effort", 3,     8,         8,         NA,         90,      "",
    "mon", "Barbell or DB curls",             NA,            "accessory", "effort", 2,     8,         12,        NA,         60,      "",
    "wed", "Front squat",                     "front_squat", "work",      "effort", 3,     5,         5,         NA,         120,     "Warm up as needed; comfortable weight",
    "wed", "Face pulls",                      NA,            "accessory", "effort", 3,     15,        20,        NA,         60,      "",
    "wed", "Hanging leg raises",              NA,            "accessory", "effort", 3,     10,        15,        0,          60,      "Bodyweight",
    "fri", "Chin-ups or lat pulldown",        NA,            "accessory", "effort", 3,     6,         10,        NA,         90,      "",
    "fri", "Overhead cable triceps extension", NA,           "accessory", "effort", 2,     10,        15,        NA,         60,      "",
    "fri", "Incline DB curls",                NA,            "accessory", "effort", 2,     10,        12,        NA,         60,      ""
  ) |>
    dplyr::mutate(
      day_label = c(mon = "Monday - Deadlift", wed = "Wednesday - OHP + front squat",
                    fri = "Friday - Bench")[day],
      pct_w1 = NA_real_, pct_w2 = NA_real_, pct_w3 = NA_real_
    )

  dplyr::bind_rows(
    main_lift("mon", "Monday - Deadlift", "Deadlift", "deadlift", 0),
    main_lift("wed", "Wednesday - OHP + front squat", "OHP", "ohp", 2),
    main_lift("fri", "Friday - Bench", "Bench press", "bench", 3),
    other
  ) |>
    dplyr::mutate(day = factor(day, levels = weekday_codes())) |>
    dplyr::arrange(day) |>
    dplyr::group_by(day) |>
    dplyr::mutate(order = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(day = as.character(day)) |>
    dplyr::select(dplyr::all_of(template_columns()))
}

template_columns <- function() {
  c("day", "day_label", "order", "exercise", "lift", "set_type", "scheme", "sets",
    "reps_min", "reps_max", "pct_w1", "pct_w2", "pct_w3", "weight_kg", "rest_s", "notes")
}

weekday_codes <- function() c("mon", "tue", "wed", "thu", "fri", "sat", "sun")

# Settings tibble (key/value) merged over the defaults, as a named list of strings.
settings_list <- function(settings_df = NULL) {
  defaults <- default_settings()
  values <- rlang::set_names(defaults$value, defaults$key)
  if (!is.null(settings_df) && nrow(settings_df) > 0) {
    given <- settings_df |> dplyr::filter(!is.na(key), key != "")
    values[given$key] <- dplyr::coalesce(given$value, "")
  }
  as.list(values)
}

setting_chr <- function(settings, key) {
  value <- settings[[key]]
  if (is.null(value) || is.na(value)) "" else trimws(value)
}

setting_num <- function(settings, key) suppressWarnings(as.numeric(setting_chr(settings, key)))

setting_vec <- function(settings, key) {
  value <- setting_chr(settings, key)
  if (value == "") character() else trimws(strsplit(value, ",", fixed = TRUE)[[1]])
}

lift_setting <- function(settings, prefix, lift) {
  value <- setting_num(settings, paste0(prefix, "_", lift))
  if (is.na(value) && prefix == "rounding") value <- setting_num(settings, "rounding_default")
  if (is.na(value)) 0 else value
}
