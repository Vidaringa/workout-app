# What the Today screen pre-fills: planned values, already-logged values, and
# for "choose by effort" exercises the previous session's numbers plus a
# double-progression hint.

# Sets of `exercise` from the most recent session before `before_date`.
last_performance <- function(sets, exercise, before_date) {
  history <- sets |>
    dplyr::filter(exercise == !!exercise, !is.na(actual_kg) | !is.na(actual_reps),
                  as.Date(date) < before_date)
  if (nrow(history) == 0) return(history)
  last_date <- max(as.Date(history$date))
  history |>
    dplyr::filter(as.Date(date) == last_date) |>
    dplyr::arrange(set_no)
}

# Double progression: once every set of the last session reached the top of
# the rep range, add `increment` kg and drop back to the bottom of the range.
double_progression <- function(last, reps_min, reps_max, increment) {
  if (nrow(last) == 0 || is.na(reps_max) || all(is.na(last$actual_kg))) return(NULL)
  top_kg <- max(last$actual_kg, na.rm = TRUE)
  if (top_kg <= 0) return(NULL)
  if (all(!is.na(last$actual_reps) & last$actual_reps >= reps_max)) {
    list(kg = top_kg + increment, reps = reps_min,
         text = sprintf("All sets hit %d last time: try %s kg x %d", reps_max, format_kg(top_kg + increment), reps_min))
  } else {
    NULL
  }
}

format_kg <- function(kg) {
  ifelse(is.na(kg), "", as.character(round(kg, 3)))
}

# The planned rows of one session with pre-fill values for the logging cards.
prefill_session <- function(plan_rows, sets, today, settings) {
  logged <- sets |>
    dplyr::filter(set_id %in% plan_rows$set_id) |>
    dplyr::select(set_id, logged_kg = actual_kg, logged_reps = actual_reps, logged_rir = rir)
  increment <- setting_num(settings, "accessory_increment")

  effort <- plan_rows |>
    dplyr::filter(scheme == "effort") |>
    dplyr::distinct(exercise, planned_reps, planned_reps_max)
  history <- purrr::pmap(effort, \(exercise, planned_reps, planned_reps_max) {
    last <- last_performance(sets, exercise, today)
    suggestion <- double_progression(last, planned_reps, planned_reps_max, increment)
    last_text <- if (nrow(last) == 0) NA_character_ else paste0(
      "Last (", format(as.Date(last$date[1]), "%d %b"), "): ",
      paste0(format_kg(last$actual_kg), " x ", dplyr::coalesce(as.character(last$actual_reps), "?"), collapse = ", ")
    )
    tibble::tibble(
      exercise = exercise,
      last = list(last),
      hint = if (is.null(suggestion)) NA_character_ else suggestion$text,
      hint_kg = if (is.null(suggestion)) NA_real_ else suggestion$kg,
      hint_reps = if (is.null(suggestion)) NA_real_ else suggestion$reps,
      last_text = last_text
    )
  }) |>
    purrr::list_rbind()
  if (nrow(history) == 0) {
    history <- tibble::tibble(exercise = character(), last = list(), hint = character(),
                              hint_kg = numeric(), hint_reps = numeric(), last_text = character())
  }

  plan_rows |>
    dplyr::left_join(logged, by = "set_id") |>
    dplyr::left_join(history, by = "exercise") |>
    dplyr::mutate(
      last_kg = purrr::map2_dbl(last, set_no, \(l, n) {
        if (is.null(l) || nrow(l) == 0) return(NA_real_)
        l$actual_kg[min(n, nrow(l))]
      }),
      last_reps = purrr::map2_dbl(last, set_no, \(l, n) {
        if (is.null(l) || nrow(l) == 0) return(NA_real_)
        l$actual_reps[min(n, nrow(l))]
      }),
      prefill_kg = dplyr::coalesce(logged_kg, planned_kg, last_kg),
      prefill_reps = dplyr::coalesce(logged_reps, dplyr::if_else(scheme == "effort", last_reps, NA_real_), planned_reps),
      is_logged = !is.na(logged_kg) | !is.na(logged_reps)
    ) |>
    dplyr::select(-last)
}

# End-of-session summary from the logged sets of that session.
session_summary <- function(sets, round, week, day) {
  done <- sets |>
    dplyr::filter(round == !!round, week == !!week, day == !!day,
                  !is.na(actual_kg) | !is.na(actual_reps))
  tops <- done |>
    dplyr::filter(set_type %in% c("top", "joker")) |>
    dplyr::mutate(
      result = paste0(format_kg(actual_kg), " x ", dplyr::coalesce(as.character(actual_reps), "?"),
                      dplyr::if_else(is.na(rir), "", paste0(" @ RIR ", rir))),
      target = paste0(format_kg(planned_kg), " x ", planned_reps),
      hit = !is.na(actual_reps) & actual_reps >= planned_reps,
      e1rm = epley(actual_kg, actual_reps + dplyr::coalesce(rir, 0))
    )
  list(
    sets = nrow(done),
    tonnage = sum(done$actual_kg * done$actual_reps, na.rm = TRUE),
    exercises = dplyr::n_distinct(done$exercise),
    tops = tops
  )
}
