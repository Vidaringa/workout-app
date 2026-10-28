parsed <- function(kg, reps = NA, planned_sets = 1) {
  parse_actuals(kg, reps, planned_sets)
}

test_that("cleaning fixes Excel artefacts", {
  expect_equal(clean_cell(c(".10-8-8", ",6-6-8", "8.0", "72.5", "72.-72.5", "46310", "46368.0", "  ")),
               c("10-8-8", "6-6-8", "8", "72.5", "72-72.5", "10-15", "12-12", NA))
})

test_that("dash-separated weights become one row per set", {
  out <- parsed("70-70-72.5")
  expect_equal(out$actual_kg, c(70, 70, 72.5))
  expect_true(all(is.na(out$actual_reps)))

  out <- parsed("27.5-30.5-30.5", "8")
  expect_equal(out$actual_kg, c(27.5, 30.5, 30.5))
  expect_equal(out$actual_reps, c(8, 8, 8))
})

test_that("reps lists with a leading dot are parsed", {
  out <- parsed("68", ".10-8-8")
  expect_equal(out$actual_kg, c(68, 68, 68))
  expect_equal(out$actual_reps, c(10, 8, 8))
})

test_that("reps x kg pairs joined by + or -", {
  out <- parsed("12x20+11x20")
  expect_equal(out$actual_reps, c(12, 11))
  expect_equal(out$actual_kg, c(20, 20))

  out <- parsed("12*20-9*20")
  expect_equal(out$actual_reps, c(12, 9))
  expect_equal(out$actual_kg, c(20, 20))

  out <- parsed("10x12-12x12-12x12")
  expect_equal(out$actual_reps, c(10, 12, 12))
  expect_equal(out$actual_kg, c(12, 12, 12))
})

test_that("sets x reps in the reps cell", {
  out <- parsed("60", "4x6")
  expect_equal(nrow(out), 4)
  expect_equal(unique(out$actual_reps), 6)
  expect_equal(unique(out$actual_kg), 60)
  expect_equal(out$flags[1], "")
})

test_that("kg written into the reps cell", {
  out <- parsed(NA, "12kg x 12-10-10")
  expect_equal(out$actual_kg, c(12, 12, 12))
  expect_equal(out$actual_reps, c(12, 10, 10))
})

test_that("weight followed by a remark keeps the remark as a note", {
  out <- parsed("33 með bar", "10", planned_sets = 3)
  expect_equal(out$actual_kg, c(33, 33, 33))
  expect_equal(out$note[1], "með bar")
  expect_match(out$flags[1], "assumed from plan")
})

test_that("date-serial reps are converted back", {
  out <- parsed("14-14", "46368")
  expect_equal(out$actual_reps, c(12, 12))
  expect_equal(out$actual_kg, c(14, 14))
})

test_that("swapped kg and reps are detected", {
  out <- parsed("5", "180")
  expect_equal(c(out$actual_kg, out$actual_reps), c(180, 5))
  expect_match(out$flags, "swapped")

  out <- parsed("4", "80")
  expect_equal(c(out$actual_kg, out$actual_reps), c(80, 4))
})

test_that("blank reps stay NA and a single top set stays one row", {
  out <- parsed("175")
  expect_equal(nrow(out), 1)
  expect_true(is.na(out$actual_reps))
})

test_that("non-numeric reps text becomes a note", {
  out <- parsed("25", "Ath með að létta og gera oftar")
  expect_equal(out$actual_kg, 25)
  expect_equal(out$note, "Ath með að létta og gera oftar")
})

test_that("empty cells give no rows", {
  expect_equal(nrow(parsed(NA, NA)), 0)
})

test_that("mismatched list lengths are flagged and padded", {
  out <- parsed("17.5-21-21", "15-12")
  expect_equal(nrow(out), 3)
  expect_true(is.na(out$actual_reps[3]))
  expect_match(out$flags[1], "differ")
})
