# The compact summary sent to Claude for the weekly review. Every number is
# computed here; the model only reads and comments.

review_summary <- function(sets, runs_m, training_max, settings, e1rm, today, bodyweight) {
  weeks <- setting_num(settings, "review_weeks")
  since <- today - weeks * 7
  lifts <- setting_vec(settings, "progression_lifts")
  round <- current_round(training_max)
  tms <- round_tms(training_max, round)
  tm_by_round <- training_max |>
    dplyr::arrange(decided_at) |>
    dplyr::group_by(round, lift) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(round, lift, tm)

  recent <- sets |> dplyr::filter(as.Date(date) >= since, !is.na(actual_kg) | !is.na(actual_reps))

  top_sets <- recent |>
    dplyr::filter(lift %in% lifts, set_type %in% c("top", "joker")) |>
    dplyr::left_join(tm_by_round, by = c("round", "lift")) |>
    dplyr::transmute(
      date, round, week, lift, set_type, planned_kg, planned_reps, actual_kg, actual_reps, rir,
      pct_of_tm = round(actual_kg / tm, 3),
      hit = !is.na(actual_reps) & actual_reps >= planned_reps,
      e1rm = round(epley(actual_kg, actual_reps + dplyr::coalesce(rir, 0)), 1),
      e1rm_is_lower_bound = is.na(rir)
    ) |>
    dplyr::arrange(date)

  trend_change <- if (is.null(e1rm$trends)) {
    tibble::tibble(lift = character())
  } else {
    e1rm$trends |>
      dplyr::group_by(lift) |>
      dplyr::summarise(
        e1rm_now = round(dplyr::last(level), 1),
        e1rm_at_window_start = round(level[max(1, sum(date <= since))], 1),
        .groups = "drop"
      )
  }
  health <- tms |>
    dplyr::filter(lift %in% lifts) |>
    dplyr::left_join(e1rm$current, by = "lift")
  health <- dplyr::bind_cols(health, tm_health(health$tm, health$e1rm, settings)) |>
    dplyr::transmute(lift, tm, e1rm = round(e1rm, 1), tm_pct_of_e1rm = round(ratio, 3), status,
                     weekly_change_pct = round(100 * weekly_change, 2)) |>
    dplyr::left_join(trend_change, by = "lift")

  accessories <- accessory_progress(sets) |>
    dplyr::filter(date >= since) |>
    dplyr::group_by(exercise) |>
    dplyr::arrange(date, .by_group = TRUE) |>
    dplyr::summarise(sessions = dplyr::n(), first_date = min(date), first_top_kg = dplyr::first(top_kg),
                     first_total_reps = dplyr::first(total_reps), last_date = max(date),
                     last_top_kg = dplyr::last(top_kg), last_total_reps = dplyr::last(total_reps), .groups = "drop")

  runs <- runs_m |>
    dplyr::filter(date >= since) |>
    dplyr::transmute(date, type, distance_km, ascent_m, duration,
                     pace = format_duration(pace_s), grade_adjusted_pace = format_duration(gap_s),
                     avg_hr, rpe, notes)
  predictions <- race_predictions(runs_m, settings, today)
  load <- weekly_load(sets, runs_m, settings)
  if (nrow(load) > 0) load <- load |> dplyr::filter(week_of >= since)

  bw <- bodyweight |> dplyr::arrange(dplyr::desc(date)) |> utils::head(1)

  list(
    today = format(today),
    window_weeks = weeks,
    athlete = list(
      constraints = setting_chr(settings, "constraints"),
      running_plan = setting_chr(settings, "run_plan"),
      bodyweight_kg = if (nrow(bw) == 1) bw$kg else setting_num(settings, "bodyweight_kg")
    ),
    goals = list(
      deadlift_e1rm_kg = setting_num(settings, "goal_deadlift"),
      bench_e1rm_kg = setting_num(settings, "goal_bench"),
      run_10k = setting_chr(settings, "goal_10k"),
      half_marathon = setting_chr(settings, "goal_half")
    ),
    program = list(
      style = "Wendler 5/3/1, 5's PRO (all working sets x5, no AMRAP), optional jokers, back-off sets at Set 1 weight",
      current_round = round,
      progression_mode = setting_chr(settings, "progression_mode"),
      training_maxes = tms,
      tm_warn_above = setting_num(settings, "tm_warn"),
      tm_flag_above = setting_num(settings, "tm_flag")
    ),
    tm_health = health,
    top_sets = top_sets,
    missed_targets = top_sets |> dplyr::filter(set_type == "top", !hit),
    accessories = accessories,
    runs = runs,
    race_predictions = if (is.null(predictions)) NULL else predictions$summary |>
      dplyr::transmute(race, best_prediction = format_duration(best), median_prediction = format_duration(median),
                       goal = format_duration(goal)),
    weekly_load = load,
    data_notes = c(
      "e1RM = Epley on sets of 10 reps or fewer, reps + RIR when RIR is logged; sets without RIR are lower bounds.",
      "e1rm_now / weekly_change_pct come from a smoothed local linear trend, not single sets.",
      "Dates for rounds 1-5 were estimated when the old spreadsheet was imported.",
      "Weekly load = run km + session_load_km per lifting session; spike = above load_spike_ratio x the previous 4-week average."
    )
  )
}

review_system_prompt <- function() {
  paste(
    "You are a strength and endurance coach reviewing one athlete's last few weeks of training.",
    "The athlete runs Wendler 5/3/1 (5's PRO) three days a week alongside two runs a week.",
    "All numbers in the JSON were computed by the app; use them as given and do not recompute or invent data.",
    "",
    "Write a short coaching note in Markdown (about 200-350 words) with these headings:",
    "## Progressing / stalling",
    "## Fatigue signals",
    "  Look especially for rising RIR cost at the same loads (lower RIR at a similar % of TM over time), missed top sets, and heavy load weeks.",
    "## Training maxes",
    "  For each main lift say whether to keep progressing, hold, or reset the TM, with the reason (TM as % of e1RM, missed week-3 targets).",
    "## Next week",
    "  Concrete suggestions for the lifting sessions and the runs.",
    "",
    "Respect the stated constraints (session length, no heavy squats). Be direct and specific; say when the data is too thin to tell.",
    "These are suggestions for the athlete to decide on: the app does not change the plan from your note."
  )
}

# Calls Claude through ellmer. Model name comes from settings; the key from
# ANTHROPIC_API_KEY. Server-side refusal fallback is enabled so a rare
# safety-classifier refusal is retried on another model instead of failing.
run_review <- function(summary, model) {
  if (!nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) stop("ANTHROPIC_API_KEY is not set on the server.")
  input_json <- jsonlite::toJSON(summary, auto_unbox = TRUE, dataframe = "rows", digits = NA,
                                 na = "null", null = "null", pretty = FALSE)
  chat <- ellmer::chat_anthropic(
    system_prompt = review_system_prompt(),
    model = model,
    params = ellmer::params(max_tokens = 8000),
    beta_headers = "server-side-fallback-2026-07-01",
    api_args = list(fallbacks = "default"),
    echo = "none"
  )
  response <- chat$chat(paste0("Here is my training summary as JSON:\n\n", input_json))
  list(input_json = as.character(input_json), response_md = as.character(response))
}
