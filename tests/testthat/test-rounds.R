settings_with <- function(...) {
  s <- settings_list()
  overrides <- list(...)
  s[names(overrides)] <- as.character(overrides)
  s
}

# The nth planned set of a type (e.g. the 2nd joker), in session order.
planned_kg <- function(plan, lift, week, set_type, nth = 1) {
  plan |>
    filter(lift == !!lift, week == !!week, set_type == !!set_type) |>
    arrange(order, set_no) |>
    pull(planned_kg) |>
    purrr::pluck(nth)
}

test_that("round_to rounds to the nearest step, halves up", {
  expect_equal(round_to(170, 2.5), 170)
  expect_equal(round_to(0.95 * 90, 1), 86)
  expect_equal(round_to(0.95 * 90, 2.5), 85)
  expect_equal(round_to(0.95 * 90, 0.5), 85.5)
  expect_equal(round_to(53.125, 1.25), 53.75)
  expect_equal(round_to(41.2, 0), 41.2)
  expect_equal(round_down_to(87.9, 2.5), 87.5)
})

test_that("round 5 matches the spreadsheet", {
  s <- settings_with(rounding_deadlift = 2.5, rounding_bench = 0.5, rounding_ohp = 0.125)
  plan <- generate_round(default_template(), round5_tms, 5, s, weeks = 1:3)

  expect_equal(planned_kg(plan, "deadlift", 1, "top"), 170)
  expect_equal(planned_kg(plan, "deadlift", 3, "top"), 190)
  expect_equal(planned_kg(plan, "deadlift", 1, "joker", 1), 180)
  expect_equal(planned_kg(plan, "deadlift", 2, "joker", 1), 190)
  expect_equal(planned_kg(plan, "bench", 3, "top"), 85.5)
  expect_equal(planned_kg(plan, "bench", 1, "warmup"), 36)
  expect_equal(planned_kg(plan, "ohp", 1, "top"), 53.125)
})

test_that("bench week-3 top set follows the rounding setting", {
  for (case in list(c(1, 86), c(2.5, 85), c(0.5, 85.5))) {
    plan <- generate_round(default_template(), round5_tms, 5,
                           settings_with(rounding_bench = case[1]), weeks = 3)
    expect_equal(planned_kg(plan, "bench", 3, "top"), case[2])
  }
})

test_that("week 3 has no jokers, weeks 1-2 do", {
  plan <- generate_round(default_template(), round5_tms, 5, settings_list(), weeks = 1:3)
  jokers <- plan |> filter(set_type == "joker") |> count(week)
  expect_equal(jokers$week, c(1, 2))
  expect_equal(jokers$n, c(6, 6))
})

test_that("back-off sets are at Set 1 weight", {
  plan <- generate_round(default_template(), round5_tms, 5, settings_list(), weeks = 1:3)
  set1 <- plan |> filter(lift == "bench", set_type == "work", order == 4)
  backoff <- plan |> filter(lift == "bench", set_type == "backoff")
  expect_equal(nrow(backoff), 9)
  expect_equal(unique(backoff$planned_kg[backoff$week == 2]), set1$planned_kg[set1$week == 2])
  expect_equal(plan |> filter(lift == "ohp", set_type == "backoff", week == 1) |> nrow(), 2)
  expect_equal(plan |> filter(lift == "deadlift", set_type == "backoff") |> nrow(), 0)
})

test_that("fixed and effort exercises are planned", {
  plan <- generate_round(default_template(), round5_tms, 5, settings_list(), weeks = 1)
  secondary <- plan |> filter(exercise == "Bench press (secondary)", set_type == "work")
  expect_equal(secondary$planned_kg, c(60, 60, 60))
  curls <- plan |> filter(exercise == "Barbell or DB curls")
  expect_equal(nrow(curls), 2)
  expect_true(all(is.na(curls$planned_kg)))
  expect_equal(unique(curls$planned_reps_max), 12)
})

test_that("set ids are unique and stable", {
  plan <- generate_round(default_template(), round5_tms, 5, settings_list(), weeks = 1:4)
  expect_false(anyDuplicated(plan$set_id) > 0)
  expect_equal(plan$set_id[1], "r5-w1-mon-01-1")
})

test_that("deload week follows the frequency setting", {
  expect_false(deload_due(5, settings_with(deload_frequency = "off")))
  expect_true(deload_due(5, settings_with(deload_frequency = "every")))
  expect_false(deload_due(5, settings_with(deload_frequency = "every_2")))
  expect_true(deload_due(6, settings_with(deload_frequency = "every_2")))
  expect_equal(round_weeks(6, settings_list()), 1:4)
})

test_that("week 4 replaces main work with the 7th-week protocol", {
  plan <- generate_round(default_template(), round5_tms, 6, settings_list(), weeks = 4)
  dl <- plan |> filter(lift == "deadlift", set_type != "warmup")
  expect_equal(dl$planned_kg, c(140, 160, 180, 200))
  expect_equal(dl$planned_reps, c(5, 5, 1, 1))
  expect_equal(nrow(filter(plan, set_type %in% c("joker", "backoff"))), 0)
  expect_equal(nrow(filter(plan, lift == "deadlift", set_type == "warmup")), 3)
  expect_true(nrow(filter(plan, exercise == "Chest-supported row")) > 0)

  test <- generate_round(default_template(), round5_tms, 6,
                         settings_with(deload_type = "tm_test"), weeks = 4)
  top <- test |> filter(lift == "bench", set_type == "top")
  expect_equal(top$planned_kg, 90)
  expect_equal(c(top$planned_reps, top$planned_reps_max), c(3, 5))
})

test_that("next_session skips sessions finished on earlier days", {
  plan <- generate_round(default_template(), round5_tms, 5, settings_list(), weeks = 1:3)
  none <- tibble(round = numeric(), week = numeric(), day = character(), date = character(),
                 actual_kg = numeric(), actual_reps = numeric())
  expect_equal(next_session(plan, none, as.Date("2026-10-28"))$session_key, "r5-w1-mon")

  monday <- tibble(round = 5, week = 1, day = "mon", date = "2026-10-26", actual_kg = 170, actual_reps = 5)
  expect_equal(next_session(plan, monday, as.Date("2026-10-28"))$session_key, "r5-w1-wed")
  expect_equal(next_session(plan, monday, as.Date("2026-10-26"))$session_key, "r5-w1-mon")
})

test_that("round_tms takes the latest decision per lift", {
  tm <- tibble::tribble(
    ~round, ~lift, ~tm, ~decided_at,
    5, "bench", 90, "2026-10-01T10:00:00",
    5, "bench", 85, "2026-10-02T10:00:00",
    5, "deadlift", 200, "2026-10-01T10:00:00",
    4, "bench", 87.5, "2026-09-01T10:00:00"
  )
  expect_equal(round_tms(tm, 5) |> arrange(lift) |> pull(tm), c(85, 200))
  expect_equal(current_round(tm), 5)
})
