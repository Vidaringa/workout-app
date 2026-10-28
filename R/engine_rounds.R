# Round generation: template + training maxes -> planned sets.

# Round to the nearest `step` kg, halves rounding up. The small epsilon guards
# against floating point (0.95 * 90 is 85.49999999999999 in binary).
round_to <- function(x, step) {
  rounded <- dplyr::if_else(is.na(step) | step <= 0, x, floor(x / step + 0.5 + 1e-9) * step)
  round(rounded, 3)
}

round_down_to <- function(x, step) {
  rounded <- dplyr::if_else(is.na(step) | step <= 0, x, floor(x / step + 1e-9) * step)
  round(rounded, 3)
}

# "5", "1-2", "3-5" -> list(min, max)
parse_rep_range <- function(x) {
  parts <- strsplit(trimws(x), "\\s*[-\u2013]\\s*")
  tibble::tibble(
    reps_min = purrr::map_dbl(parts, \(p) as.numeric(p[1])),
    reps_max = purrr::map_dbl(parts, \(p) as.numeric(p[length(p)]))
  )
}

deload_due <- function(round, settings) {
  switch(setting_chr(settings, "deload_frequency"),
    every = TRUE,
    every_2 = round %% 2 == 0,
    FALSE
  )
}

round_weeks <- function(round, settings) {
  if (deload_due(round, settings)) 1:4 else 1:3
}

make_set_id <- function(round, week, day, order, set_no) {
  sprintf("r%d-w%d-%s-%02d-%d", as.integer(round), as.integer(week), day,
          as.integer(order), as.integer(set_no))
}

# Week 4 ("7th week protocol"): the main-lift work sets are replaced by the
# deload or TM-test sets from settings; warm-ups, fixed work and accessories stay.
week4_template <- function(template, settings) {
  type <- setting_chr(settings, "deload_type")
  pcts <- as.numeric(setting_vec(settings, paste0("week4_", type, "_pcts")))
  reps <- parse_rep_range(setting_vec(settings, paste0("week4_", type, "_reps")))
  is_main_work <- template$scheme == "pct" & template$set_type != "warmup"

  protocol <- template[is_main_work, ] |>
    dplyr::group_by(day, exercise) |>
    dplyr::slice_min(order, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(-c(set_type, sets, reps_min, reps_max, pct_w1, pct_w2, pct_w3, notes)) |>
    tidyr::crossing(tibble::tibble(step = seq_along(pcts), pct = pcts, reps)) |>
    dplyr::mutate(
      set_type = dplyr::if_else(step == length(pcts), "top", "work"),
      sets = 1, pct_w1 = pct, pct_w2 = pct, pct_w3 = pct,
      notes = dplyr::if_else(type == "tm_test", "TM test", "Deload"),
      order = order + (step - 1) / 100
    ) |>
    dplyr::select(-step, -pct)

  dplyr::bind_rows(template[!is_main_work, ], protocol) |>
    dplyr::arrange(factor(day, levels = weekday_codes()), order) |>
    dplyr::group_by(day) |>
    dplyr::mutate(order = dplyr::row_number()) |>
    dplyr::ungroup()
}

# Planned sets for one round. `tms` is a tibble with columns lift, tm.
generate_round <- function(template, tms, round, settings, weeks = round_weeks(round, settings)) {
  tm_lookup <- rlang::set_names(tms$tm, tms$lift)
  template <- template |> dplyr::filter(!is.na(exercise), exercise != "")

  purrr::map(weeks, \(week) {
    week_template <- if (week == 4) week4_template(template, settings) else template
    pct_col <- paste0("pct_w", min(week, 3))
    week_template |>
      dplyr::mutate(week = week, pct = .data[[pct_col]]) |>
      dplyr::filter(scheme != "pct" | !is.na(pct))
  }) |>
    purrr::list_rbind() |>
    dplyr::filter(sets > 0) |>
    tidyr::uncount(sets, .id = "set_no") |>
    dplyr::mutate(
      round = round,
      tm = unname(tm_lookup[lift]),
      step = purrr::map_dbl(dplyr::coalesce(lift, ""), \(l) lift_setting(settings, "rounding", if (l == "") "default" else l)),
      planned_kg = dplyr::case_when(
        scheme == "pct" ~ round_to(tm * pct, step),
        scheme == "fixed" ~ weight_kg,
        scheme == "effort" ~ weight_kg
      ),
      planned_reps = reps_min,
      planned_reps_max = reps_max,
      set_id = make_set_id(round, week, day, order, set_no)
    ) |>
    dplyr::arrange(week, factor(day, levels = weekday_codes()), order, set_no) |>
    dplyr::select(set_id, round, week, day, day_label, order, exercise, lift, set_type, scheme,
                  set_no, pct, planned_kg, planned_reps, planned_reps_max, rest_s, notes)
}

# The sessions of a planned round, in training order.
round_sessions <- function(plan) {
  plan |>
    dplyr::distinct(round, week, day, day_label) |>
    dplyr::arrange(round, week, factor(day, levels = weekday_codes())) |>
    dplyr::mutate(session_key = sprintf("r%d-w%d-%s", round, week, day))
}

# First session of the round that has no sets logged before today. Sessions
# logged today stay "current" so a reload mid-session doesn't jump ahead.
next_session <- function(plan, sets, today) {
  done <- sets |>
    dplyr::filter(!is.na(actual_kg) | !is.na(actual_reps), as.Date(date) < today) |>
    dplyr::distinct(round, week, day) |>
    dplyr::mutate(done = TRUE)
  round_sessions(plan) |>
    dplyr::left_join(done, by = c("round", "week", "day")) |>
    dplyr::filter(is.na(done)) |>
    dplyr::slice_head(n = 1) |>
    dplyr::select(-done)
}

# Training maxes in effect for a round (latest decision per lift).
round_tms <- function(training_max, round) {
  training_max |>
    dplyr::filter(round == !!round) |>
    dplyr::arrange(decided_at) |>
    dplyr::group_by(lift) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(lift, tm)
}

current_round <- function(training_max) {
  if (nrow(training_max) == 0) NA_integer_ else as.integer(max(training_max$round))
}
