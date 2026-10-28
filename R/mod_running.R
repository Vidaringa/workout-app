# Running: log runs, weekly volume, pace / grade-adjusted pace, race
# predictions and combined lifting + running load.

# The "Log a run" form, used from both Today and Running. Registers the
# observers inside the calling module; `button` is that module's button id.
run_logger <- function(input, session, store, button) {
  ns <- session$ns
  output <- session$output

  shiny::observeEvent(input[[button]], {
    output$run_error <- shiny::renderText("")
    shiny::showModal(shiny::modalDialog(
      title = "Log a run",
      easyClose = FALSE,
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(ns("run_save"), "Save run", class = "btn-primary")
      ),
      shiny::dateInput(ns("run_date"), "Date", value = store$today(), width = "100%"),
      shiny::radioButtons(ns("run_type"), NULL, c(Road = "road", Trail = "trail"), inline = TRUE),
      shiny::div(
        class = "form-row-2",
        shiny::numericInput(ns("run_km"), "Distance (km)", value = NA, min = 0, step = 0.1),
        shiny::numericInput(ns("run_ascent"), "Ascent (m)", value = NA, min = 0, step = 10)
      ),
      shiny::div(
        class = "form-row-2",
        shiny::numericInput(ns("run_min"), "Time: minutes", value = NA, min = 0, step = 1),
        shiny::numericInput(ns("run_sec"), "seconds", value = NA, min = 0, max = 59, step = 1)
      ),
      shiny::div(
        class = "form-row-2",
        shiny::numericInput(ns("run_hr"), "Avg HR (optional)", value = NA, min = 0, step = 1),
        shiny::selectInput(ns("run_rpe"), "RPE", choices = c("", 1:10), selected = "")
      ),
      shiny::textInput(ns("run_notes"), "Notes", width = "100%"),
      shiny::div(class = "text-danger", shiny::textOutput(ns("run_error")))
    ))
  })

  shiny::observeEvent(input$run_save, {
    km <- as_number(input$run_km)
    seconds <- 60 * dplyr::coalesce(as_number(input$run_min), 0) + dplyr::coalesce(as_number(input$run_sec), 0)
    if (is.na(km) || km <= 0 || seconds <= 0) {
      output$run_error <- shiny::renderText("Distance and time are needed.")
      return()
    }
    tz <- store$tz()
    row <- tibble::tibble(
      run_id = paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S", tz = tz)),
      date = format(input$run_date), type = input$run_type, distance_km = km,
      ascent_m = as_number(input$run_ascent), duration = format_duration(seconds),
      avg_hr = as_number(input$run_hr), rpe = as_number(input$run_rpe),
      notes = dplyr::na_if(input$run_notes, ""), source = "manual", external_id = NA_character_,
      logged_at = now_iso(tz)
    )
    error <- tryCatch({
      db_write_run(row)
      NULL
    }, error = function(e) conditionMessage(e))
    if (is.null(error)) {
      shiny::removeModal()
      store$touch("runs")
      shiny::showNotification(sprintf("Run saved: %s km in %s", format_kg(km), format_duration(seconds)), type = "message")
    } else {
      output$run_error <- shiny::renderText(paste("Not saved, your entries are kept. Tap Save to retry.", error))
    }
  })
}

mod_running_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "screen",
    shiny::actionButton(ns("log_run"), "Log a run", icon = shiny::icon("person-running"),
                        class = "btn-primary btn-lg w-100"),
    shiny::h3(class = "section-title", "Race predictions"),
    shiny::uiOutput(ns("predictions")),
    shiny::h3(class = "section-title", "Weekly distance"),
    shiny::plotOutput(ns("weekly_km"), height = "220px"),
    shiny::h3(class = "section-title", "Weekly ascent"),
    shiny::plotOutput(ns("weekly_ascent"), height = "200px"),
    shiny::h3(class = "section-title", "Pace and grade-adjusted pace"),
    shiny::plotOutput(ns("pace"), height = "240px", click = ns("pace_click")),
    shiny::uiOutput(ns("pace_detail")),
    shiny::div(class = "chart-note",
               "Grade-adjusted pace counts every metre climbed as 8 m of flat running (setting gap_ascent_factor). Lower is faster."),
    shiny::h3(class = "section-title", "Combined weekly load"),
    shiny::plotOutput(ns("load"), height = "220px"),
    shiny::div(class = "chart-note", shiny::textOutput(ns("load_note"), inline = TRUE)),
    shiny::uiOutput(ns("load_table")),
    shiny::h3(class = "section-title", "Recent runs"),
    shiny::uiOutput(ns("recent"))
  )
}

mod_running_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    run_logger(input, session, store, "log_run")

    weekly <- shiny::reactive({
      runs <- store$runs_m()
      shiny::validate(shiny::need(nrow(runs) > 0, "No runs logged yet."))
      runs |>
        dplyr::mutate(week_of = week_start(date)) |>
        dplyr::group_by(week_of) |>
        dplyr::summarise(km = sum(distance_km, na.rm = TRUE), ascent = sum(ascent_m, na.rm = TRUE), .groups = "drop")
    })

    week_bars <- function(data, y, label) {
      ggplot2::ggplot(data, ggplot2::aes(week_of, .data[[y]])) +
        ggplot2::geom_col(fill = lift_colors[["deadlift"]], width = 5) +
        ggplot2::scale_x_date(date_labels = "%d %b") +
        ggplot2::labs(x = NULL, y = label) +
        theme_app()
    }
    output$weekly_km <- shiny::renderPlot(week_bars(weekly(), "km", "km"), res = 96)
    output$weekly_ascent <- shiny::renderPlot(week_bars(weekly(), "ascent", "m climbed"), res = 96)

    pace_long <- shiny::reactive({
      runs <- store$runs_m() |> dplyr::filter(!is.na(pace_s))
      shiny::validate(shiny::need(nrow(runs) > 0, "No runs with a time yet."))
      runs |>
        tidyr::pivot_longer(c(pace_s, gap_s), names_to = "measure", values_to = "per_km") |>
        dplyr::mutate(measure = dplyr::if_else(measure == "pace_s", "Pace", "Grade-adjusted"))
    })

    output$pace <- shiny::renderPlot({
      ggplot2::ggplot(pace_long(), ggplot2::aes(date, per_km, colour = measure)) +
        ggplot2::geom_line(linewidth = 0.7) +
        ggplot2::geom_point(size = 2.6, stroke = 0) +
        ggplot2::scale_colour_manual(values = c("Pace" = "#2a78d6", "Grade-adjusted" = "#eb6834")) +
        ggplot2::scale_y_continuous(labels = format_duration) +
        ggplot2::scale_x_date(date_labels = "%d %b") +
        ggplot2::labs(x = NULL, y = "min / km") +
        theme_app()
    }, res = 96)

    output$pace_detail <- shiny::renderUI({
      shiny::req(input$pace_click)
      near <- shiny::nearPoints(pace_long(), input$pace_click, xvar = "date", yvar = "per_km",
                                threshold = 25, maxpoints = 1)
      shiny::req(nrow(near) == 1)
      shiny::div(class = "chart-detail", sprintf(
        "%s · %s %s km, %s m up · %s · pace %s, GAP %s",
        format_date_short(near$date), near$type, format_kg(near$distance_km),
        format_kg(dplyr::coalesce(near$ascent_m, 0)), near$duration,
        format_pace(near$pace_s), format_pace(near$gap_s)
      ))
    })

    output$predictions <- shiny::renderUI({
      pred <- race_predictions(store$runs_m(), store$settings(), store$today())
      if (is.null(pred)) return(empty_state("Log runs of 5 km or more to get predictions."))
      s <- pred$summary
      shiny::tagList(
        shiny::div(
          class = "summary-tiles",
          purrr::map(seq_len(nrow(s)), \(i) {
            on_track <- s$best[i] <= s$goal[i]
            stat_tile(
              s$race[i], format_duration(s$best[i]),
              shiny::tagList(
                paste0("goal ", format_duration(s$goal[i]), " · median ", format_duration(s$median[i]), " "),
                if (on_track) status_badge("ok", "on track") else status_badge("warn", paste0(format_duration(s$best[i] - s$goal[i]), " to go"))
              )
            )
          })
        ),
        shiny::div(class = "chart-note", sprintf(
          "Riegel (exponent %s) from %d run(s) of 5 km+ in the last 8 weeks, using grade-adjusted distance. Best = fastest prediction. Training runs aren't races, so these are conservative.",
          setting_chr(store$settings(), "riegel_exponent"), nrow(pred$runs)))
      )
    })

    load <- shiny::reactive({
      load <- weekly_load(store$sets(), store$runs_m(), store$settings())
      shiny::validate(shiny::need(nrow(load) > 0, "Nothing logged yet."))
      load
    })

    output$load <- shiny::renderPlot({
      data <- load() |> dplyr::mutate(flag = dplyr::if_else(spike, "Spike", "Normal"))
      ggplot2::ggplot(data, ggplot2::aes(week_of, load, fill = flag)) +
        ggplot2::geom_col(width = 5) +
        ggplot2::geom_line(ggplot2::aes(week_of, baseline, linetype = "4-wk avg"), inherit.aes = FALSE,
                           colour = ink$secondary, linewidth = 0.5, na.rm = TRUE) +
        ggplot2::scale_fill_manual(values = c(Normal = "#86b6ef", Spike = status_colors[["flag"]])) +
        ggplot2::scale_linetype_manual(values = c("4-wk avg" = "22")) +
        ggplot2::scale_x_date(date_labels = "%d %b") +
        ggplot2::labs(x = NULL, y = "load (km-equivalent)") +
        theme_app()
    }, res = 96)

    output$load_note <- shiny::renderText(sprintf(
      "Load = run km + %s km per lifting session. Red weeks are more than %s x the previous 4-week average.",
      setting_chr(store$settings(), "session_load_km"), setting_chr(store$settings(), "load_spike_ratio")))

    output$load_table <- shiny::renderUI({
      data <- load() |> dplyr::arrange(dplyr::desc(week_of)) |> utils::head(6)
      shiny::tags$table(
        class = "table table-sm data-table",
        shiny::tags$thead(shiny::tags$tr(purrr::map(c("Week of", "Sessions", "Run km", "Ascent", "Load"), shiny::tags$th))),
        shiny::tags$tbody(purrr::map(seq_len(nrow(data)), \(i) shiny::tags$tr(
          shiny::tags$td(format_date_short(data$week_of[i])),
          shiny::tags$td(data$sessions[i]),
          shiny::tags$td(format_kg(round(data$run_km[i], 1))),
          shiny::tags$td(format_kg(round(data$ascent_m[i]))),
          shiny::tags$td(format_kg(round(data$load[i])), " ", if (data$spike[i]) status_badge("flag", "spike"))
        )))
      )
    })

    output$recent <- shiny::renderUI({
      runs <- store$runs_m() |> dplyr::arrange(dplyr::desc(date)) |> utils::head(10)
      if (nrow(runs) == 0) return(empty_state("No runs logged yet."))
      shiny::tags$table(
        class = "table table-sm data-table",
        shiny::tags$thead(shiny::tags$tr(purrr::map(c("Date", "Type", "km", "Up", "Time", "Pace", "GAP"), shiny::tags$th))),
        shiny::tags$tbody(purrr::map(seq_len(nrow(runs)), \(i) shiny::tags$tr(
          shiny::tags$td(format_date_short(runs$date[i])),
          shiny::tags$td(runs$type[i]),
          shiny::tags$td(format_kg(runs$distance_km[i])),
          shiny::tags$td(format_kg(runs$ascent_m[i])),
          shiny::tags$td(runs$duration[i]),
          shiny::tags$td(format_duration(runs$pace_s[i])),
          shiny::tags$td(format_duration(runs$gap_s[i]))
        )))
      )
    })
  })
}
