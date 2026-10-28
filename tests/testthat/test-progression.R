tm_history <- tibble::tribble(
  ~round, ~lift, ~tm, ~decided_at,
  3, "deadlift", 190, "2026-09-14",
  3, "bench", 85, "2026-09-14",
  3, "ohp", 57.5, "2026-09-14",
  3, "front_squat", 60, "2026-09-14"
)

week3_tops <- function(round, lift, reps, rir = NA_real_) {
  tibble(round = round, week = 3, lift = lift, set_type = "top",
         planned_reps = 5, actual_kg = 100, actual_reps = reps, rir = rir)
}

e1rm_now <- tibble(lift = c("deadlift", "bench", "ohp"), e1rm = c(215, 88, 62))

with_mode <- function(mode, ...) {
  s <- settings_list()
  s$progression_mode <- mode
  overrides <- list(...)
  s[names(overrides)] <- as.character(overrides)
  s
}

test_that("standard adds +5 lower / +2.5 upper and carries front squat over", {
  out <- propose_next_tms(tibble(), tm_history, 3, with_mode("standard"), e1rm_now) |> arrange(lift)
  expect_equal(out$lift, c("bench", "deadlift", "front_squat", "ohp"))
  expect_equal(out$proposed_tm, c(87.5, 195, 60, 60))
  expect_equal(out$method[out$lift == "front_squat"], "manual")
})

test_that("conditional adds only when the week-3 top set was hit", {
  sets <- bind_rows(
    week3_tops(3, "deadlift", 5),
    week3_tops(3, "bench", 3),
    week3_tops(3, "ohp", 5, rir = 0)
  )
  out <- propose_next_tms(sets, tm_history, 3, with_mode("conditional"), e1rm_now) |> arrange(lift)
  expect_equal(out$proposed_tm[out$lift == "deadlift"], 195)
  expect_equal(out$proposed_tm[out$lift == "bench"], 85)
  # 5 reps but RIR 0 is below the default target of RIR >= 1
  expect_equal(out$proposed_tm[out$lift == "ohp"], 57.5)
})

test_that("conditional holds when nothing was logged", {
  out <- propose_next_tms(tibble(round = numeric(), week = numeric(), lift = character(),
                                 set_type = character(), planned_reps = numeric(),
                                 actual_reps = numeric(), rir = numeric()),
                          tm_history, 3, with_mode("conditional"), e1rm_now)
  expect_true(all(out$proposed_tm == out$current_tm))
  expect_match(out$reason[out$lift == "bench"], "No week-3")
})

test_that("conditional suggests a reset after two misses in a row", {
  sets <- bind_rows(week3_tops(2, "bench", 4), week3_tops(3, "bench", 3))
  expect_equal(consecutive_misses(sets, 3, "bench", 1), 2)
  out <- propose_next_tms(sets, tm_history, 3, with_mode("conditional", rounding_bench = 2.5), e1rm_now)
  # 0.90 * 88 = 79.2 -> rounded down to 77.5
  expect_equal(out$proposed_tm[out$lift == "bench"], 77.5)
  expect_match(out$reason[out$lift == "bench"], "reset")

  one_miss <- week3_tops(3, "bench", 3)
  out <- propose_next_tms(one_miss, tm_history, 3, with_mode("conditional"), e1rm_now)
  expect_equal(out$proposed_tm[out$lift == "bench"], 85)
})

test_that("e1rm mode caps the TM at the configured share of e1RM", {
  out <- propose_next_tms(tibble(), tm_history, 3, with_mode("e1rm", rounding_bench = 2.5), e1rm_now) |>
    arrange(lift)
  # deadlift: 190 + 5 = 195 <= 0.9 * 215 = 193.5? no -> capped at 192.5
  expect_equal(out$proposed_tm[out$lift == "deadlift"], 192.5)
  # bench: 0.9 * 88 = 79.2 -> 77.5
  expect_equal(out$proposed_tm[out$lift == "bench"], 77.5)
  # ohp: 0.9 * 62 = 55.8 -> rounded down to 55 with 1 kg steps
  expect_equal(out$proposed_tm[out$lift == "ohp"], 55)

  roomy <- tibble(lift = c("deadlift", "bench", "ohp"), e1rm = c(250, 120, 80))
  out <- propose_next_tms(tibble(), tm_history, 3, with_mode("e1rm"), roomy)
  expect_equal(out$proposed_tm[out$lift == "deadlift"], 195)
})

test_that("TM health warns above 92 % and flags above 95 %", {
  health <- tm_health(c(90, 85, 80, 90), c(93, 91, 100, NA), settings_list())
  expect_equal(health$status, c("flag", "warn", "ok", "unknown"))
  expect_equal(round(health$ratio[1], 3), 0.968)
})
