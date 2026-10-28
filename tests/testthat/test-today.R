history_sets <- tibble::tribble(
  ~set_id, ~date,        ~round, ~week, ~day,  ~exercise,             ~set_no, ~set_type,   ~actual_kg, ~actual_reps, ~rir, ~planned_kg, ~planned_reps,
  "a1",    "2026-10-19", 4,      3,     "mon", "Barbell or DB curls", 1,       "accessory", 20,         12,           NA,   NA,          8,
  "a2",    "2026-10-19", 4,      3,     "mon", "Barbell or DB curls", 2,       "accessory", 20,         12,           NA,   NA,          8,
  "b1",    "2026-10-12", 4,      2,     "mon", "Chest-supported row", 1,       "accessory", 70,         8,            NA,   NA,          8,
  "b2",    "2026-10-12", 4,      2,     "mon", "Chest-supported row", 2,       "accessory", 72.5,       7,            NA,   NA,          8
)

test_that("last performance is the most recent earlier session", {
  last <- last_performance(history_sets, "Barbell or DB curls", as.Date("2026-10-26"))
  expect_equal(last$actual_kg, c(20, 20))
  expect_equal(nrow(last_performance(history_sets, "Barbell or DB curls", as.Date("2026-10-19"))), 0)
})

test_that("double progression adds weight only when every set hit the top of the range", {
  curls <- last_performance(history_sets, "Barbell or DB curls", as.Date("2026-10-26"))
  hint <- double_progression(curls, 8, 12, 2.5)
  expect_equal(hint$kg, 22.5)
  expect_equal(hint$reps, 8)

  row <- last_performance(history_sets, "Chest-supported row", as.Date("2026-10-26"))
  expect_null(double_progression(row, 8, 8, 2.5))
})

test_that("prefill uses last session for effort exercises and the plan otherwise", {
  plan <- generate_round(default_template(), round5_tms, 5, settings_list(), weeks = 1) |>
    filter(day == "mon")
  logged <- bind_rows(history_sets, tibble(
    set_id = "r5-w1-mon-06-1", date = "2026-10-26", round = 5, week = 1, day = "mon",
    exercise = "Deadlift", set_no = 1, set_type = "top", actual_kg = 172.5, actual_reps = 5, rir = 2
  ))
  filled <- prefill_session(plan, logged, as.Date("2026-10-26"), settings_list())

  top <- filled |> filter(set_type == "top")
  expect_true(top$is_logged)
  expect_equal(c(top$prefill_kg, top$prefill_reps, top$logged_rir), c(172.5, 5, 2))

  curls <- filled |> filter(exercise == "Barbell or DB curls")
  expect_equal(curls$prefill_kg, c(20, 20))
  expect_equal(curls$prefill_reps, c(12, 12))
  expect_match(curls$hint[1], "22.5")

  set1 <- filled |> filter(lift == "deadlift", order == 4)
  expect_equal(c(set1$prefill_kg, set1$prefill_reps), c(130, 5))
  expect_false(set1$is_logged)
})

test_that("session summary reports top sets against target", {
  sets <- tibble(round = 5, week = 1, day = "mon", exercise = "Deadlift",
                 set_type = c("work", "top"), actual_kg = c(150, 170), actual_reps = c(5, 4),
                 planned_kg = c(150, 170), planned_reps = 5, rir = c(NA, 0))
  s <- session_summary(sets, 5, 1, "mon")
  expect_equal(s$sets, 2)
  expect_equal(s$tonnage, 150 * 5 + 170 * 4)
  expect_false(s$tops$hit)
  expect_equal(s$tops$result, "170 x 4 @ RIR 0")
})
