library(dplyr)

for (file in list.files(test_path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(file, local = FALSE)
}

round5_tms <- tibble::tribble(
  ~lift,         ~tm,
  "deadlift",    200,
  "bench",       90,
  "ohp",         62.5,
  "front_squat", 60
)
