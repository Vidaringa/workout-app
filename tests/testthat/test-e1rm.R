test_that("Epley matches the formula and treats a single as the 1RM", {
  expect_equal(epley(100, 5), 100 * (1 + 5 / 30))
  expect_equal(epley(180, 1), 180)
  expect_equal(epley(c(90, 90), c(3, 10)), c(99, 120))
})

test_that("e1RM points add RIR and mark sets without RIR as lower bounds", {
  sets <- tibble(
    date = "2026-10-05", lift = c("bench", "bench", "bench", "bench", NA),
    set_type = c("top", "top", "warmup", "top", "accessory"),
    actual_kg = c(80, 80, 40, 60, 20), actual_reps = c(5, 5, 5, 12, 10),
    rir = c(2, NA, NA, NA, NA)
  )
  pts <- e1rm_points(sets, "bench", max_reps = 10)
  expect_equal(nrow(pts), 2)
  expect_equal(pts$e1rm, c(epley(80, 7), epley(80, 5)))
  expect_equal(pts$lower_bound, c(FALSE, TRUE))
})

test_that("daily best keeps one point per lift and day", {
  pts <- tibble(lift = "bench", date = as.Date(c("2026-10-05", "2026-10-05", "2026-10-12")),
                e1rm = c(90, 95, 96), lower_bound = TRUE, actual_kg = 80, actual_reps = 5, rir = NA)
  daily <- e1rm_daily(pts)
  expect_equal(daily$e1rm, c(95, 96))
})

daily_series <- function(values, lower_bound = FALSE, start = as.Date("2026-01-05")) {
  tibble(lift = "deadlift", date = start + 7 * (seq_along(values) - 1), e1rm = values,
         lower_bound = lower_bound)
}

test_that("the trend smooths noise and stays inside the data range", {
  set.seed(1)
  truth <- 180 * 1.005^(0:19)
  daily <- daily_series(truth * exp(rnorm(20, 0, 0.03)))
  trend <- e1rm_trend(daily)
  expect_equal(nrow(trend), 20)
  expect_true(all(trend$lo95 <= trend$level & trend$level <= trend$hi95))
  # smoothed series is closer to the truth than the raw points
  expect_lt(mean(abs(trend$level - truth)), mean(abs(daily$e1rm - truth)))
  final <- attr(trend, "final")
  expect_gt(final$slope, 0)
})

test_that("lower bounds below the trend pull it down only weakly", {
  daily <- daily_series(c(200, 200, 200, 150, 200), lower_bound = c(FALSE, FALSE, FALSE, TRUE, FALSE))
  trend <- e1rm_trend(daily)
  expect_gt(min(trend$level), 190)

  exact <- daily_series(c(200, 200, 200, 150, 200))
  expect_lt(min(e1rm_trend(exact)$level), min(trend$level))
})

test_that("an all-lower-bound series does not ratchet far above its best point", {
  # The imported deadlift history: every set without RIR.
  daily <- daily_series(c(180, 198, 198, 204, 198, 210, 216, 198), lower_bound = TRUE)
  trend <- e1rm_trend(daily)
  expect_lt(utils::tail(trend$level, 1), max(daily$e1rm) * 1.01)
})

test_that("a single observation gives a trend with that level", {
  trend <- e1rm_trend(daily_series(190))
  expect_equal(trend$level, 190)
})

test_that("goal projection returns a date range for a rising trend", {
  daily <- daily_series(180 * 1.01^(0:11))
  final <- attr(e1rm_trend(daily, obs_sd = 0.01), "final")
  proj <- goal_projection(final, 220)
  expect_true(proj$status %in% c("range", "open"))
  expect_true(proj$earliest > final$date)
  if (proj$status == "range") expect_true(proj$latest >= proj$earliest)

  expect_equal(goal_projection(final, 150)$status, "reached")
  flat <- attr(e1rm_trend(daily_series(rep(180, 12))), "final")
  expect_true(goal_projection(flat, 220)$status %in% c("no_trend", "open"))
})
