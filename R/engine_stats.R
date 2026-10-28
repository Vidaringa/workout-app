# Running metrics, tonnage, accessory progress and weekly load.

# "50:00", "0:50:00", "1:02:03" -> seconds
parse_duration <- function(x) {
  purrr::map_dbl(x, \(d) {
    if (is.na(d) || trimws(d) == "") return(NA_real_)
    parts <- suppressWarnings(as.numeric(strsplit(trimws(d), ":", fixed = TRUE)[[1]]))
    if (anyNA(parts) || length(parts) > 3) return(NA_real_)
    sum(parts * 60^rev(seq_along(parts) - 1))
  })
}

format_duration <- function(seconds) {
  purrr::map_chr(seconds, \(s) {
    if (is.na(s)) return(NA_character_)
    s <- round(s)
    if (s >= 3600) sprintf("%d:%02d:%02d", s %/% 3600, (s %% 3600) %/% 60, s %% 60)
    else sprintf("%d:%02d", s %/% 60, s %% 60)
  })
}

week_start <- function(date) lubridate::floor_date(as.Date(date), "week", week_start = 1)

# Pace and grade-adjusted pace. GAP uses a flat-equivalent distance:
# every metre climbed counts as `gap_ascent_factor` metres of flat running
# (default 8, close to the 1:8 rule used for hill running and Naismith-style
# effort equivalents). Descents are ignored, so on loops GAP slightly
# overstates the help from going downhill.
run_metrics <- function(runs, settings) {
  factor <- setting_num(settings, "gap_ascent_factor")
  runs |>
    dplyr::mutate(
      date = as.Date(date),
      seconds = parse_duration(duration),
      pace_s = seconds / distance_km,
      flat_km = distance_km + factor * dplyr::coalesce(ascent_m, 0) / 1000,
      gap_s = seconds / flat_km
    ) |>
    dplyr::arrange(date)
}

# Riegel: T2 = T1 * (D2 / D1)^k
riegel <- function(seconds, km, target_km, exponent = 1.06) seconds * (target_km / km)^exponent

# 10 km and half-marathon predictions from runs in the last `weeks` weeks of
# at least 5 km, using the flat-equivalent distance. Training runs are not
# races, so these are conservative: the "best" column is the most useful.
race_predictions <- function(runs_m, settings, today, weeks = 8) {
  k <- setting_num(settings, "riegel_exponent")
  recent <- runs_m |>
    dplyr::filter(date > today - weeks * 7, distance_km >= 5, !is.na(seconds))
  if (nrow(recent) == 0) return(NULL)
  recent <- recent |>
    dplyr::mutate(pred_10k = riegel(seconds, flat_km, 10, k),
                  pred_half = riegel(seconds, flat_km, 21.0975, k))
  list(
    runs = recent,
    summary = tibble::tibble(
      race = c("10 km", "Half marathon"),
      best = c(min(recent$pred_10k), min(recent$pred_half)),
      median = c(stats::median(recent$pred_10k), stats::median(recent$pred_half)),
      goal = parse_duration(c(setting_chr(settings, "goal_10k"), setting_chr(settings, "goal_half")))
    )
  )
}

# Volume (kg x reps) per week and lift, warm-ups excluded.
weekly_tonnage <- function(sets) {
  sets |>
    dplyr::filter(!is.na(lift), set_type != "warmup", !is.na(actual_kg), !is.na(actual_reps)) |>
    dplyr::mutate(week_of = week_start(date)) |>
    dplyr::group_by(week_of, lift) |>
    dplyr::summarise(tonnage = sum(actual_kg * actual_reps), .groups = "drop")
}

# Per accessory and session: heaviest set and total reps.
accessory_progress <- function(sets) {
  sets |>
    dplyr::filter(set_type == "accessory" | (set_type == "work" & is.na(lift)), !is.na(actual_kg)) |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::group_by(exercise, date) |>
    dplyr::summarise(top_kg = max(actual_kg), total_reps = sum(actual_reps, na.rm = TRUE),
                     sets = dplyr::n(), .groups = "drop")
}

# Lifting sessions + run km per week, with a combined index where one session
# counts as `session_load_km` km. A week is flagged when the index is more than
# `load_spike_ratio` x the average of the previous four weeks.
weekly_load <- function(sets, runs_m, settings) {
  session_km <- setting_num(settings, "session_load_km")
  ratio <- setting_num(settings, "load_spike_ratio")
  lifting <- sets |>
    dplyr::filter(!is.na(actual_kg) | !is.na(actual_reps)) |>
    dplyr::distinct(date) |>
    dplyr::mutate(week_of = week_start(date)) |>
    dplyr::count(week_of, name = "sessions")
  running <- runs_m |>
    dplyr::mutate(week_of = week_start(date)) |>
    dplyr::group_by(week_of) |>
    dplyr::summarise(run_km = sum(distance_km, na.rm = TRUE), ascent_m = sum(ascent_m, na.rm = TRUE),
                     runs = dplyr::n(), .groups = "drop")
  weeks <- dplyr::full_join(lifting, running, by = "week_of")
  if (nrow(weeks) == 0) return(weeks)
  tibble::tibble(week_of = seq(min(weeks$week_of), max(weeks$week_of), by = 7)) |>
    dplyr::left_join(weeks, by = "week_of") |>
    dplyr::mutate(
      dplyr::across(c(sessions, run_km, ascent_m, runs), \(x) dplyr::coalesce(as.numeric(x), 0)),
      load = run_km + session_km * sessions,
      baseline = purrr::map_dbl(dplyr::row_number(), \(i) {
        prev <- load[pmax(1, i - 4):(i - 1)]
        if (i < 3) NA_real_ else mean(prev)
      }),
      spike = !is.na(baseline) & baseline > 0 & load > ratio * baseline
    )
}
