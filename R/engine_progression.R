# Training-max progression and TM health.

# Outcome of the week-3 top set of a round: "hit", "miss" or "missing".
# Hit means all prescribed reps, with at least `min_rir` in reserve when RIR
# was logged (sets logged without RIR count on reps alone).
week3_result <- function(sets, round, lift, min_rir) {
  top <- sets |>
    dplyr::filter(round == !!round, week == 3, lift == !!lift, set_type == "top",
                  !is.na(actual_reps))
  if (nrow(top) == 0) return("missing")
  top <- top[1, ]
  hit <- isTRUE(top$actual_reps >= dplyr::coalesce(top$planned_reps, 5)) &&
    (is.na(top$rir) || top$rir >= min_rir)
  if (hit) "hit" else "miss"
}

# Misses in a row, counting back from `round` and stopping at the first round
# that was hit or has no week-3 top set.
consecutive_misses <- function(sets, round, lift, min_rir) {
  misses <- 0
  for (r in rev(seq_len(round))) {
    if (week3_result(sets, r, lift, min_rir) != "miss") break
    misses <- misses + 1
  }
  misses
}

# Proposed training maxes for round + 1. `e1rm_now` has columns lift, e1rm.
# Lifts not listed in `progression_lifts` carry over unchanged.
propose_next_tms <- function(sets, training_max, round, settings, e1rm_now,
                             mode = setting_chr(settings, "progression_mode")) {
  tms <- round_tms(training_max, round)
  progress_lifts <- setting_vec(settings, "progression_lifts")
  min_rir <- setting_num(settings, "conditional_min_rir")
  reset_after <- setting_num(settings, "reset_after_misses")
  reset_pct <- setting_num(settings, "reset_pct")
  cap_pct <- setting_num(settings, "e1rm_cap_pct")

  tms |>
    dplyr::left_join(dplyr::select(e1rm_now, lift, e1rm), by = "lift") |>
    dplyr::mutate(proposal = purrr::pmap(list(lift, tm, e1rm), \(lift, tm, e1rm) {
      inc <- lift_setting(settings, "increment", lift)
      step <- lift_setting(settings, "rounding", lift)
      if (!lift %in% progress_lifts) {
        return(list(tm = tm, method = "manual", reason = "Not progressed (carried over)"))
      }
      switch(mode,
        standard = list(tm = tm + inc, method = "standard", reason = sprintf("+%s kg", inc)),
        conditional = {
          result <- week3_result(sets, round, lift, min_rir)
          misses <- consecutive_misses(sets, round, lift, min_rir)
          if (result == "hit") {
            list(tm = tm + inc, method = "conditional", reason = sprintf("Week-3 top set hit: +%s kg", inc))
          } else if (result == "missing") {
            list(tm = tm, method = "conditional", reason = "No week-3 top set logged: hold")
          } else if (misses >= reset_after && !is.na(e1rm) && round_down_to(reset_pct * e1rm, step) < tm) {
            list(tm = round_down_to(reset_pct * e1rm, step), method = "conditional",
                 reason = sprintf("Missed %d rounds in a row: reset to %d %% of e1RM %.1f", misses, round(reset_pct * 100), e1rm))
          } else {
            list(tm = tm, method = "conditional", reason = sprintf("Week-3 top set missed (%d in a row): hold", misses))
          }
        },
        e1rm = {
          if (is.na(e1rm)) {
            list(tm = tm + inc, method = "e1rm", reason = sprintf("No e1RM yet: +%s kg", inc))
          } else {
            cap <- round_down_to(cap_pct * e1rm, step)
            if (tm + inc <= cap) {
              list(tm = tm + inc, method = "e1rm", reason = sprintf("+%s kg (cap %s = %d %% of e1RM)", inc, cap, round(cap_pct * 100)))
            } else {
              list(tm = cap, method = "e1rm", reason = sprintf("Capped at %d %% of e1RM %.1f", round(cap_pct * 100), e1rm))
            }
          }
        },
        stop("Unknown progression mode: ", mode)
      )
    })) |>
    dplyr::mutate(
      proposed_tm = purrr::map_dbl(proposal, "tm"),
      method = purrr::map_chr(proposal, "method"),
      reason = purrr::map_chr(proposal, "reason")
    ) |>
    dplyr::select(lift, current_tm = tm, e1rm, proposed_tm, method, reason)
}

# TM as a share of e1RM, with the warn / flag thresholds from settings.
tm_health <- function(tm, e1rm, settings) {
  ratio <- tm / e1rm
  status <- dplyr::case_when(
    is.na(ratio) ~ "unknown",
    ratio > setting_num(settings, "tm_flag") ~ "flag",
    ratio > setting_num(settings, "tm_warn") ~ "warn",
    TRUE ~ "ok"
  )
  tibble::tibble(ratio = ratio, status = status)
}
