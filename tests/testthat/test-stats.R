test_that("durations parse and format", {
  expect_equal(parse_duration(c("50:00", "0:50:00", "1:02:03", "", NA, "abc")),
               c(3000, 3000, 3723, NA, NA, NA))
  expect_equal(format_duration(c(3000, 3723, NA)), c("50:00", "1:02:03", NA))
})

test_that("Riegel scales time by distance ratio to the 1.06 power", {
  expect_equal(riegel(3000, 10, 10), 3000)
  expect_equal(riegel(3000, 10, 21.0975), 3000 * 2.10975^1.06)
})

test_that("grade-adjusted pace counts climbing as extra flat distance", {
  runs <- tibble(date = "2026-10-01", distance_km = 10, ascent_m = 250, duration = "1:00:00")
  m <- run_metrics(runs, settings_list())
  expect_equal(m$pace_s, 360)
  expect_equal(m$flat_km, 12)
  expect_equal(m$gap_s, 300)
})

test_that("race predictions use recent runs only", {
  runs <- tibble(date = c("2026-10-01", "2026-06-01"), distance_km = c(10, 10),
                 ascent_m = 0, duration = c("55:00", "45:00"))
  pred <- race_predictions(run_metrics(runs, settings_list()), settings_list(), as.Date("2026-10-27"))
  expect_equal(nrow(pred$runs), 1)
  expect_equal(pred$summary$best[1], 3300)
  expect_equal(pred$summary$goal, c(3000, 7200))
})

test_that("weekly load fills empty weeks and flags spikes", {
  sets <- tibble(date = c("2026-09-07", "2026-09-14", "2026-09-21", "2026-10-05"),
                 actual_kg = 100, actual_reps = 5)
  runs <- tibble(date = c("2026-09-08", "2026-10-06", "2026-10-07"), distance_km = c(10, 15, 20),
                 ascent_m = 0, duration = "1:00:00")
  load <- weekly_load(sets, run_metrics(runs, settings_list()), settings_list())
  expect_equal(nrow(load), 5)
  expect_equal(load$sessions, c(1, 1, 1, 0, 1))
  expect_equal(load$load, c(18, 8, 8, 0, 43))
  expect_equal(load$spike, c(FALSE, FALSE, FALSE, FALSE, TRUE))
})

test_that("tonnage excludes warm-ups and accessories without a lift", {
  sets <- tibble(date = "2026-10-05", lift = c("deadlift", "deadlift", NA),
                 set_type = c("warmup", "top", "accessory"),
                 actual_kg = c(80, 170, 20), actual_reps = c(5, 5, 10))
  expect_equal(weekly_tonnage(sets)$tonnage, 850)
})
