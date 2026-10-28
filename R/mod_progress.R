# Progress: e1RM trend per lift, TM vs e1RM, goals, tonnage, accessories.

mod_progress_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "screen",
    shiny::radioButtons(ns("lift"), NULL, inline = TRUE,
                        c("Deadlift" = "deadlift", "Bench" = "bench", "OHP" = "ohp")),
    shiny::uiOutput(ns("tiles")),
    shiny::plotOutput(ns("e1rm"), height = "280px", click = ns("e1rm_click")),
    shiny::uiOutput(ns("e1rm_detail")),
    shiny::tags$details(
      class = "chart-note",
      shiny::tags$summary("How the trend is estimated"),
      shiny::p("Each dot is the best estimated 1RM of a session (Epley, sets of 10 reps or fewer, reps + RIR when RIR is logged).",
               "Hollow dots had no RIR, so they are lower bounds: you could have done more."),
      shiny::p("The line is a local linear trend state-space model (level + weekly slope) on the log scale,",
               "fitted with a Kalman filter and smoother. It treats each session as a noisy reading (about 3 %) of a slowly",
               "drifting strength level, widens the uncertainty across gaps between sessions, and only lets",
               "lower-bound dots pull the line up, never down. The band is the 80 % interval of the level.",
               "The noise settings are trend_obs_sd, trend_level_sd and trend_slope_sd.")
    ),
    shiny::h3(class = "section-title", "Goals"),
    shiny::uiOutput(ns("goals")),
    shiny::h3(class = "section-title", "Weekly volume (kg × reps, warm-ups excluded)"),
    shiny::plotOutput(ns("tonnage"), height = "300px"),
    shiny::h3(class = "section-title", "Accessories"),
    shiny::selectInput(ns("accessory"), NULL, choices = NULL, width = "100%"),
    shiny::plotOutput(ns("accessory_plot"), height = "220px"),
    shiny::uiOutput(ns("accessory_table"))
  )
}

projection_text <- function(proj, goal) {
  switch(proj$status,
    reached = "Reached — the trend is at or above the goal.",
    range = sprintf("Projected between %s and %s (80 %% range, if the current trend holds).",
                    format(proj$earliest, "%b %Y"), format(proj$latest, "%b %Y")),
    open = sprintf("Not before %s; the trend is too uncertain to give a latest date.", format(proj$earliest, "%b %Y")),
    no_trend = "No upward trend yet, so no projection.",
    "Not enough data yet."
  )
}

mod_progress_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    lift_data <- shiny::reactive({
      e <- store$e1rm()
      lift <- input$lift
      daily <- e$daily |> dplyr::filter(lift == !!lift)
      shiny::validate(shiny::need(nrow(daily) > 0, paste("No", lift_label(lift), "sets logged yet.")))
      trend <- e$trends |> dplyr::filter(lift == !!lift)
      list(daily = daily, trend = trend, final = attr(e$trends, "final")[[lift]])
    })

    tm_segments <- shiny::reactive({
      rounds <- store$sets() |>
        dplyr::filter(!is.na(actual_kg) | !is.na(actual_reps)) |>
        dplyr::group_by(round) |>
        dplyr::summarise(start = min(as.Date(date)), end = max(as.Date(date)), .groups = "drop")
      store$training_max() |>
        dplyr::filter(lift == input$lift) |>
        dplyr::arrange(decided_at) |>
        dplyr::group_by(round) |>
        dplyr::slice_tail(n = 1) |>
        dplyr::ungroup() |>
        dplyr::inner_join(rounds, by = "round")
    })

    output$tiles <- shiny::renderUI({
      d <- lift_data()
      health <- store$health() |> dplyr::filter(lift == input$lift)
      current <- exp(d$final$log_level)
      best <- d$daily |> dplyr::slice_max(e1rm, n = 1, with_ties = FALSE)
      shiny::div(
        class = "summary-tiles",
        stat_tile("e1RM (trend)", paste0(format_kg(round(current, 1)), " kg"),
                  sprintf("%+.1f %% / week", 100 * (exp(d$final$slope) - 1))),
        if (nrow(health) == 1) {
          stat_tile("TM / e1RM", if (is.na(health$ratio)) "–" else paste0(round(100 * health$ratio), " %"),
                    shiny::tagList(paste0("TM ", format_kg(health$tm), " "), status_badge(health$status)))
        },
        stat_tile("Best set", paste0(format_kg(best$actual_kg), " × ", best$actual_reps),
                  paste0(format_date_short(best$date), " · e1RM ", format_kg(round(best$e1rm, 1))))
      )
    })

    output$e1rm <- shiny::renderPlot({
      d <- lift_data()
      colour <- lift_colors[[input$lift]]
      goal <- setting_num(store$settings(), paste0("goal_", input$lift))
      points <- d$daily |> dplyr::mutate(kind = dplyr::if_else(lower_bound, "Lower bound (no RIR)", "With RIR"))
      ggplot2::ggplot() +
        ggplot2::geom_ribbon(data = d$trend, ggplot2::aes(date, ymin = lo80, ymax = hi80),
                             fill = colour, alpha = 0.15) +
        ggplot2::geom_line(data = d$trend, ggplot2::aes(date, level), colour = colour, linewidth = 0.8) +
        ggplot2::geom_segment(data = tm_segments(), ggplot2::aes(x = start, xend = end, y = tm, yend = tm, linetype = "Training max"),
                              colour = ink$secondary, linewidth = 0.5) +
        {
          if (!is.na(goal)) ggplot2::geom_hline(ggplot2::aes(yintercept = goal, linetype = "Goal"),
                                                colour = ink$muted, linewidth = 0.4)
        } +
        ggplot2::geom_point(data = points, ggplot2::aes(date, e1rm, shape = kind), colour = colour,
                            fill = ink$surface, size = 2.8, stroke = 1) +
        ggplot2::scale_shape_manual(values = c("With RIR" = 16, "Lower bound (no RIR)" = 21)) +
        ggplot2::scale_linetype_manual(values = c("Training max" = "solid", "Goal" = "22")) +
        ggplot2::scale_x_date(date_labels = "%d %b") +
        ggplot2::labs(x = NULL, y = "kg") +
        theme_app() +
        ggplot2::theme(legend.box = "vertical", legend.spacing.y = ggplot2::unit(0, "pt"))
    }, res = 96)

    output$e1rm_detail <- shiny::renderUI({
      shiny::req(input$e1rm_click)
      near <- shiny::nearPoints(lift_data()$daily, input$e1rm_click, xvar = "date", yvar = "e1rm",
                                threshold = 25, maxpoints = 1)
      if (nrow(near) == 0) return(shiny::div(class = "chart-detail", "Tap a dot to see the set."))
      shiny::div(class = "chart-detail", sprintf(
        "%s · %s kg × %s%s → e1RM %s%s",
        format_date_short(near$date), format_kg(near$actual_kg), near$actual_reps,
        if (is.na(near$rir)) "" else paste0(" @ RIR ", near$rir),
        format_kg(round(near$e1rm, 1)), if (near$lower_bound) " (lower bound)" else ""
      ))
    })

    output$goals <- shiny::renderUI({
      e <- store$e1rm()
      settings <- store$settings()
      goal_lifts <- purrr::keep(names(lift_labels), \(l) !is.na(setting_num(settings, paste0("goal_", l))))
      if (length(goal_lifts) == 0) return(empty_state("No lift goals set (goal_deadlift, goal_bench, ... in settings)."))
      shiny::div(
        class = "goal-list",
        purrr::map(goal_lifts, \(l) {
          goal <- setting_num(settings, paste0("goal_", l))
          final <- attr(e$trends, "final")[[l]]
          proj <- goal_projection(final, goal)
          current <- proj$current
          progress <- if (is.na(current)) 0 else min(1, current / goal)
          shiny::div(
            class = "goal-item",
            shiny::div(class = "goal-head",
                       shiny::span(class = "goal-name", paste0(lift_label(l), " ", format_kg(goal), " kg")),
                       shiny::span(class = "goal-gap", if (is.na(current)) "" else
                         if (current >= goal) "reached" else paste0(format_kg(round(goal - current, 1)), " kg to go"))),
            shiny::div(class = "goal-bar", shiny::div(class = "goal-fill",
                                                      style = sprintf("width:%.1f%%;background:%s", 100 * progress,
                                                                      dplyr::coalesce(unname(lift_colors[l]), "#2a78d6")))),
            shiny::div(class = "goal-text",
                       if (!is.na(current)) paste0("Trend e1RM ", format_kg(round(current, 1)), " kg. "),
                       projection_text(proj, goal))
          )
        })
      )
    })

    output$tonnage <- shiny::renderPlot({
      tonnage <- weekly_tonnage(store$sets()) |>
        dplyr::filter(lift %in% names(lift_colors)) |>
        dplyr::mutate(lift = factor(lift, levels = names(lift_colors), labels = lift_label(names(lift_colors))))
      shiny::validate(shiny::need(nrow(tonnage) > 0, "No working sets logged yet."))
      ggplot2::ggplot(tonnage, ggplot2::aes(week_of, tonnage, fill = lift)) +
        ggplot2::geom_col(width = 5, show.legend = FALSE) +
        ggplot2::facet_wrap(~lift, ncol = 1, scales = "free_y") +
        ggplot2::scale_fill_manual(values = rlang::set_names(unname(lift_colors), lift_label(names(lift_colors)))) +
        ggplot2::scale_x_date(date_labels = "%d %b") +
        ggplot2::labs(x = NULL, y = "kg") +
        theme_app()
    }, res = 96)

    accessories <- shiny::reactive(accessory_progress(store$sets()))

    shiny::observeEvent(accessories(), {
      choices <- accessories() |>
        dplyr::count(exercise, sort = TRUE) |>
        dplyr::pull(exercise)
      current <- shiny::isolate(input$accessory)
      shiny::updateSelectInput(session, "accessory", choices = choices,
                               selected = if (!is.null(current) && current %in% choices) current else choices[1])
    })

    accessory_rows <- shiny::reactive({
      shiny::req(input$accessory)
      accessories() |> dplyr::filter(exercise == input$accessory) |> dplyr::arrange(date)
    })

    output$accessory_plot <- shiny::renderPlot({
      a <- accessory_rows()
      ggplot2::ggplot(a, ggplot2::aes(date, top_kg)) +
        ggplot2::geom_line(colour = "#6250d6", linewidth = 0.7) +
        ggplot2::geom_point(colour = "#6250d6", size = 2.6) +
        ggplot2::scale_x_date(date_labels = "%d %b") +
        ggplot2::labs(x = NULL, y = "heaviest set (kg)") +
        theme_app()
    }, res = 96)

    output$accessory_table <- shiny::renderUI({
      a <- accessory_rows() |> dplyr::arrange(dplyr::desc(date)) |> utils::head(5)
      shiny::tags$table(
        class = "table table-sm data-table",
        shiny::tags$thead(shiny::tags$tr(purrr::map(c("Date", "Heaviest", "Sets", "Total reps"), shiny::tags$th))),
        shiny::tags$tbody(purrr::map(seq_len(nrow(a)), \(i) shiny::tags$tr(
          shiny::tags$td(format_date_short(a$date[i])), shiny::tags$td(paste0(format_kg(a$top_kg[i]), " kg")),
          shiny::tags$td(a$sets[i]), shiny::tags$td(a$total_reps[i])
        )))
      )
    })
  })
}
