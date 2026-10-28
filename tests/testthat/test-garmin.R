test_that("intervals.icu activities become runs rows", {
  activities <- list(
    list(id = "i101", start_date_local = "2026-10-22T17:05:12", type = "TrailRun", distance = 9512.3,
         moving_time = 3305L, elapsed_time = 3400L, total_elevation_gain = 241.6, average_heartrate = 151L,
         icu_rpe = 6L, description = "  Esja loop ", source = "GARMIN_CONNECT"),
    list(id = "i102", start_date_local = "2026-10-24T09:00:00", type = "Run", distance = 12000,
         elapsed_time = 3900L, source = "GARMIN_CONNECT"),
    list(id = "i103", start_date_local = "2026-10-23T12:00:00", type = "Ride", distance = 30000,
         moving_time = 3600L, source = "GARMIN_CONNECT"),
    list(id = "i104", start_date_local = "2026-10-25T12:00:00", type = "Run", source = "STRAVA"),
    list(id = "i105", start_date_local = "2026-10-26T07:00:00", type = "VirtualRun", distance = 5000,
         moving_time = 1500L, perceived_exertion = 4, source = "GARMIN_CONNECT")
  )
  runs <- garmin_runs(activities, "Atlantic/Reykjavik")

  expect_equal(runs$external_id, c("i101", "i102", "i105"))
  expect_equal(runs$run_id[1], "garmin-i101")
  expect_equal(runs$date, c("2026-10-22", "2026-10-24", "2026-10-26"))
  expect_equal(runs$type, c("trail", "road", "road"))
  expect_equal(runs$distance_km, c(9.51, 12, 5))
  expect_equal(runs$ascent_m, c(242, NA, NA))
  expect_equal(runs$duration, c("55:05", "1:05:00", "25:00"))
  expect_equal(runs$avg_hr, c(151, NA, NA))
  expect_equal(runs$rpe, c(6, NA, 4))
  expect_equal(runs$notes, c("Esja loop", NA, NA))
  expect_true(all(runs$source == "garmin"))
  expect_equal(names(db_coerce(runs, "runs")), names(runs))
})

test_that("no activities gives an empty runs table", {
  runs <- garmin_runs(list(), "Atlantic/Reykjavik")
  expect_equal(nrow(runs), 0)
  expect_true("external_id" %in% names(runs))
})
