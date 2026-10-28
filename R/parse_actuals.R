# Parser for the free-text "actual" cells of the old Excel workbook, e.g.
# "70-70-72.5", ".10-8-8", "12x20+11x20", "12*20-9*20", "33 með bar".

num_re <- "\\d+(?:\\.\\d+)?"

# Excel turned rep ranges into dates: serial 46310 is 2026-10-15, i.e. "10-15".
excel_serial_to_range <- function(x) {
  is_serial <- !is.na(x) & grepl("^\\d{5}(\\.0+)?$", x)
  serial <- suppressWarnings(as.numeric(x[is_serial]))
  dates <- as.Date(serial, origin = "1899-12-30")
  x[is_serial] <- paste0(as.integer(format(dates, "%m")), "-", as.integer(format(dates, "%d")))
  x
}

clean_cell <- function(x) {
  x <- trimws(x)
  x[x == ""] <- NA
  x <- excel_serial_to_range(x)
  # A leading "." or "," was typed to stop Excel from making "10-8-8" a date.
  x <- sub("^[.,](?=\\d)", "", x, perl = TRUE)
  # "8.0" -> "8", "72." -> "72"
  x <- gsub("(\\d)\\.0+(?!\\d)", "\\1", x, perl = TRUE)
  gsub("(\\d)\\.(?!\\d)", "\\1", x, perl = TRUE)
}

# "70-70-72.5" -> c(70, 70, 72.5); NULL if it isn't a plain number list.
parse_number_list <- function(x) {
  if (is.na(x) || !grepl(paste0("^", num_re, "(\\s*[-+,]\\s*", num_re, ")*$"), x)) return(NULL)
  as.numeric(strsplit(x, "\\s*[-+,]\\s*")[[1]])
}

# Parse an "Actual kg" cell. Returns list(kg, reps, note).
parse_kg_text <- function(x) {
  out <- list(kg = numeric(), reps = numeric(), note = NA_character_)
  if (is.na(x)) return(out)
  pair_re <- paste0(num_re, "\\s*[x*]\\s*", num_re)

  if (grepl(paste0("^", pair_re, "(\\s*[-+]\\s*", pair_re, ")*$"), x)) {
    # "12x20+11x20": reps x kg per set
    pairs <- strsplit(strsplit(x, "\\s*[-+]\\s*")[[1]], "\\s*[x*]\\s*")
    out$reps <- purrr::map_dbl(pairs, \(p) as.numeric(p[1]))
    out$kg <- purrr::map_dbl(pairs, \(p) as.numeric(p[2]))
  } else if (!is.null(numbers <- parse_number_list(x))) {
    out$kg <- numbers
  } else if (grepl(paste0("^", num_re, "\\s+\\D"), x)) {
    # "33 með bar": weight followed by a remark
    out$kg <- as.numeric(regmatches(x, regexpr(num_re, x)))
    out$note <- trimws(sub(paste0("^", num_re), "", x))
  } else {
    out$note <- x
  }
  out
}

# Parse a reps cell. Returns list(reps, sets, kg, note); `sets` is set by
# "4x6" (4 sets of 6), `kg` by "12kg x 12-10-10".
parse_reps_text <- function(x) {
  out <- list(reps = numeric(), sets = NA_real_, kg = numeric(), note = NA_character_)
  if (is.na(x)) return(out)
  sets_x_reps <- paste0("^(", num_re, ")\\s*[x*]\\s*(", num_re, ")$")
  kg_x_reps <- paste0("^(", num_re, ")\\s*kg\\s*[x*]\\s*(.+)$")

  if (grepl(sets_x_reps, x)) {
    out$sets <- as.numeric(sub(sets_x_reps, "\\1", x))
    out$reps <- as.numeric(sub(sets_x_reps, "\\2", x))
  } else if (grepl(kg_x_reps, x)) {
    out$kg <- as.numeric(sub(kg_x_reps, "\\1", x))
    out$reps <- parse_number_list(sub(kg_x_reps, "\\2", x)) %||% numeric()
  } else if (!is.null(numbers <- parse_number_list(x))) {
    out$reps <- numbers
  } else {
    out$note <- x
  }
  out
}

# Combine the kg and reps cells into one row per set. `planned_sets` is used
# when a single value was logged for a multi-set exercise (flagged).
parse_actuals <- function(kg_text, reps_text, planned_sets = 1) {
  kg_text <- clean_cell(kg_text)
  reps_text <- clean_cell(reps_text)
  kg_p <- parse_kg_text(kg_text)
  reps_p <- parse_reps_text(reps_text)

  kg <- if (length(kg_p$kg)) kg_p$kg else reps_p$kg
  reps <- if (length(kg_p$reps)) kg_p$reps else reps_p$reps
  notes <- stats::na.omit(c(kg_p$note, reps_p$note))
  flags <- character()

  n <- max(length(kg), length(reps), dplyr::coalesce(reps_p$sets, 0))
  if (n == 0) {
    return(tibble::tibble(set_no = integer(), actual_kg = numeric(), actual_reps = numeric(),
                          flags = character(), note = character()))
  }
  if (length(kg) > 1 && length(reps) > 1 && length(kg) != length(reps)) {
    flags <- c(flags, "kg and reps lists differ in length")
  }
  if (n == 1 && is.na(reps_p$sets) && planned_sets > 1) {
    n <- planned_sets
    flags <- c(flags, sprintf("set count assumed from plan (%d)", planned_sets))
  }
  spread <- function(v) {
    if (length(v) == 0) rep(NA_real_, n) else if (length(v) == 1) rep(v, n) else c(v, rep(NA_real_, n))[seq_len(n)]
  }
  kg <- spread(kg)
  reps <- spread(reps)

  swapped <- !is.na(kg) & !is.na(reps) & kg <= 20 & reps > 30
  if (any(swapped)) {
    flags <- c(flags, "kg and reps swapped")
    tmp <- kg[swapped]
    kg[swapped] <- reps[swapped]
    reps[swapped] <- tmp
  }

  tibble::tibble(
    set_no = seq_len(n),
    actual_kg = kg,
    actual_reps = reps,
    flags = paste(flags, collapse = "; "),
    note = if (length(notes)) paste(notes, collapse = "; ") else NA_character_
  )
}
