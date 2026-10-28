# Reactive access to the data for all screens. Each tab has a version counter;
# writers call store$touch("<tab>") so only dependents of that tab update.

read_or_explain <- function(read) {
  tryCatch(read(), error = function(e) {
    shiny::validate(shiny::need(FALSE, paste("Couldn't read the Google Sheet:", conditionMessage(e))))
  })
}

app_store <- function() {
  tabs <- names(db_schema)
  versions <- do.call(shiny::reactiveValues, rlang::set_names(as.list(rep(0, length(tabs))), tabs))
  read_tab <- function(tab) {
    force(tab)
    shiny::reactive({
      versions[[tab]]
      read_or_explain(\() db_read(tab))
    })
  }

  store <- purrr::map(rlang::set_names(tabs), read_tab)
  store$settings_df <- store$settings
  store$settings <- shiny::reactive(settings_list(store$settings_df()))

  store$touch <- function(tab) versions[[tab]] <- shiny::isolate(versions[[tab]]) + 1
  store$refresh <- function() {
    db_refresh()
    for (tab in tabs) store$touch(tab)
  }

  store$tz <- shiny::reactive(setting_chr(store$settings(), "timezone"))
  store$today <- shiny::reactive(today_in(store$tz()))
  store$round <- shiny::reactive({
    round <- current_round(store$training_max())
    shiny::validate(shiny::need(!is.na(round), "No training maxes yet: run scripts/upload_import.R or add a row to the training_max tab."))
    round
  })
  store$plan <- shiny::reactive({
    round <- store$round()
    generate_round(store$template(), round_tms(store$training_max(), round), round, store$settings())
  })

  store$e1rm <- shiny::reactive({
    settings <- store$settings()
    points <- e1rm_points(store$sets(), setting_vec(settings, "progression_lifts"),
                          setting_num(settings, "e1rm_max_reps"))
    daily <- e1rm_daily(points)
    trends <- if (nrow(daily) > 0) e1rm_trends(daily, settings) else NULL
    list(points = points, daily = daily, trends = trends,
         current = if (is.null(trends)) tibble::tibble(lift = character(), e1rm = numeric()) else e1rm_current(trends))
  })

  # TM health for the current round's progressed lifts.
  store$health <- shiny::reactive({
    settings <- store$settings()
    tms <- round_tms(store$training_max(), store$round()) |>
      dplyr::filter(lift %in% setting_vec(settings, "progression_lifts")) |>
      dplyr::left_join(store$e1rm()$current, by = "lift")
    dplyr::bind_cols(tms, tm_health(tms$tm, tms$e1rm, settings))
  })

  store$runs_m <- shiny::reactive(run_metrics(store$runs(), store$settings()))
  store
}
