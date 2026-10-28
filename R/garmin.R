# Garmin runs, fetched through intervals.icu (which syncs with Garmin Connect
# and has a personal API key). Needs INTERVALS_API_KEY; INTERVALS_ATHLETE_ID
# defaults to 0, meaning the key's own athlete. Imported rows get
# source = "garmin" and the intervals.icu activity id as external_id, so a run
# is never imported twice.

garmin_enabled <- function() nzchar(Sys.getenv("INTERVALS_API_KEY"))

garmin_fields <- c("id", "start_date_local", "type", "distance", "moving_time", "elapsed_time",
                   "total_elevation_gain", "average_heartrate", "icu_rpe", "perceived_exertion",
                   "description", "source")

# Activities between two dates, as a list of lists (one per activity).
garmin_fetch <- function(oldest, newest) {
  athlete <- Sys.getenv("INTERVALS_ATHLETE_ID", "0")
  httr2::request("https://intervals.icu/api/v1") |>
    httr2::req_url_path_append("athlete", if (nzchar(athlete)) athlete else "0", "activities") |>
    httr2::req_url_query(oldest = format(oldest), newest = format(newest),
                         fields = paste(garmin_fields, collapse = ",")) |>
    httr2::req_auth_basic("API_KEY", Sys.getenv("INTERVALS_API_KEY")) |>
    httr2::req_timeout(20) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

# intervals.icu activities -> rows for the `runs` tab. Keeps runs only, and
# skips activities without distance and time (Strava-sourced activities come
# back empty through the API).
garmin_runs <- function(activities, tz) {
  field <- function(a, name) if (is.null(a[[name]])) NA else a[[name]]
  rows <- purrr::map(activities, \(a) tibble::tibble(
    id = as.character(field(a, "id")),
    date = substr(field(a, "start_date_local"), 1, 10),
    activity_type = as.character(field(a, "type")),
    source_app = as.character(field(a, "source")),
    distance_m = as.numeric(field(a, "distance")),
    seconds = as.numeric(dplyr::coalesce(field(a, "moving_time"), field(a, "elapsed_time"))),
    ascent_m = as.numeric(field(a, "total_elevation_gain")),
    avg_hr = as.numeric(field(a, "average_heartrate")),
    rpe = as.numeric(dplyr::coalesce(field(a, "icu_rpe"), field(a, "perceived_exertion"))),
    notes = as.character(field(a, "description"))
  ))
  dplyr::bind_rows(rows, tibble::tibble(
    id = character(), date = character(), activity_type = character(), source_app = character(),
    distance_m = numeric(), seconds = numeric(), ascent_m = numeric(), avg_hr = numeric(),
    rpe = numeric(), notes = character()
  )) |>
    dplyr::filter(grepl("Run$", activity_type), dplyr::coalesce(source_app, "") != "STRAVA",
                  distance_m > 0, seconds > 0) |>
    dplyr::transmute(
      run_id = paste0("garmin-", id),
      date,
      type = dplyr::if_else(activity_type == "TrailRun", "trail", "road"),
      distance_km = round(distance_m / 1000, 2),
      ascent_m = round(ascent_m),
      duration = format_duration(seconds),
      avg_hr = round(avg_hr),
      rpe = dplyr::if_else(rpe >= 1 & rpe <= 10, round(rpe), NA_real_),
      notes = dplyr::na_if(trimws(notes), ""),
      source = "garmin",
      external_id = id,
      logged_at = now_iso(tz)
    )
}

# Fetch runs since the last imported one (a few days back, in case an upload
# was late) or `garmin_sync_days` back on the first sync, and append the new
# ones. Returns the number of runs added.
garmin_sync <- function(runs, settings, today) {
  imported <- runs |> dplyr::filter(source == "garmin")
  oldest <- if (nrow(imported) > 0) {
    max(as.Date(imported$date), na.rm = TRUE) - 3
  } else {
    today - setting_num(settings, "garmin_sync_days")
  }
  new <- garmin_fetch(oldest, today) |>
    garmin_runs(setting_chr(settings, "timezone")) |>
    dplyr::filter(!external_id %in% imported$external_id)
  db_write_runs(new)
  nrow(new)
}
