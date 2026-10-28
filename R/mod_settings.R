# Settings / Program: TMs, the main settings, bodyweight, and the template
# (read-only here; edit it in the Sheet's `template` tab and press Refresh).

settings_fields <- list(
  list(key = "progression_mode", label = "Progression mode", type = "select",
       choices = c("Standard (+5 / +2.5)" = "standard", "Conditional (week-3 top set)" = "conditional",
                   "e1RM-anchored (cap)" = "e1rm")),
  list(key = "deload_frequency", label = "4th week (deload / TM test)", type = "select",
       choices = c("Off" = "off", "Every round" = "every", "Every 2 rounds" = "every_2")),
  list(key = "deload_type", label = "4th week type", type = "select",
       choices = c("Deload" = "deload", "TM test" = "tm_test")),
  list(key = "rounding_deadlift", label = "Rounding: deadlift (kg)", type = "number"),
  list(key = "rounding_bench", label = "Rounding: bench (kg)", type = "number"),
  list(key = "rounding_ohp", label = "Rounding: OHP (kg)", type = "number"),
  list(key = "increment_deadlift", label = "TM increment: deadlift", type = "number"),
  list(key = "increment_bench", label = "TM increment: bench", type = "number"),
  list(key = "increment_ohp", label = "TM increment: OHP", type = "number"),
  list(key = "conditional_min_rir", label = "Conditional: min RIR on week-3 top set", type = "number"),
  list(key = "e1rm_cap_pct", label = "e1RM cap (share of e1RM)", type = "number"),
  list(key = "rest_timer", label = "Rest timer", type = "select", choices = c("On" = "on", "Off" = "off")),
  list(key = "goal_deadlift", label = "Goal: deadlift e1RM", type = "number"),
  list(key = "goal_bench", label = "Goal: bench e1RM", type = "number"),
  list(key = "goal_10k", label = "Goal: 10 km (h:mm:ss)", type = "text"),
  list(key = "goal_half", label = "Goal: half marathon (h:mm:ss)", type = "text"),
  list(key = "claude_model", label = "Claude model for reviews", type = "text")
)

mod_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "screen",
    shiny::actionButton(ns("refresh"), "Refresh from Google Sheet", icon = shiny::icon("rotate"),
                        class = "btn-outline-primary w-100"),
    shiny::h3(class = "section-title", "Training maxes"),
    shiny::uiOutput(ns("tms")),
    shiny::h3(class = "section-title", "Settings"),
    shiny::uiOutput(ns("form")),
    shiny::actionButton(ns("save"), "Save settings", icon = shiny::icon("floppy-disk"), class = "btn-primary w-100"),
    shiny::div(class = "chart-note", "Every setting, with a description, is in the Sheet's settings tab."),
    shiny::h3(class = "section-title", "Bodyweight"),
    shiny::div(
      class = "form-row-2 align-items-end",
      shiny::numericInput(ns("bodyweight"), "kg", value = NA, step = 0.1),
      shiny::actionButton(ns("save_bodyweight"), "Log", class = "btn-outline-primary mb-3")
    ),
    shiny::uiOutput(ns("bodyweight_last")),
    shiny::h3(class = "section-title", "Program template"),
    shiny::div(class = "chart-note",
               "Edit the template tab in the Google Sheet (swap accessories, change sets, reps, % or rest), then press Refresh.",
               "Logged sets keep their own planned values, so history is not affected."),
    shiny::uiOutput(ns("template"))
  )
}

mod_settings_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$refresh, {
      store$refresh()
      shiny::showNotification("Reloaded from the Google Sheet.", type = "message")
    })

    output$tms <- shiny::renderUI({
      tms <- store$training_max() |>
        dplyr::arrange(decided_at) |>
        dplyr::group_by(round, lift) |>
        dplyr::slice_tail(n = 1) |>
        dplyr::ungroup() |>
        dplyr::arrange(dplyr::desc(round))
      lifts <- intersect(names(lift_labels), unique(tms$lift))
      rounds <- unique(tms$round)
      shiny::tags$table(
        class = "table table-sm data-table",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Round"), purrr::map(lift_label(lifts), shiny::tags$th),
                                         shiny::tags$th("Method"))),
        shiny::tags$tbody(purrr::map(rounds, \(r) {
          row <- tms[tms$round == r, ]
          shiny::tags$tr(
            shiny::tags$td(r),
            purrr::map(lifts, \(l) shiny::tags$td(format_kg(row$tm[row$lift == l]))),
            shiny::tags$td(paste(unique(row$method[row$lift %in% setting_vec(store$settings(), "progression_lifts")]), collapse = ", "))
          )
        }))
      )
    })

    output$form <- shiny::renderUI({
      s <- store$settings()
      purrr::map(settings_fields, \(f) {
        id <- ns(paste0("set_", f$key))
        value <- setting_chr(s, f$key)
        switch(f$type,
          select = shiny::selectInput(id, f$label, f$choices, selected = value, width = "100%"),
          number = shiny::numericInput(id, f$label, value = as.numeric(value), width = "100%"),
          text = shiny::textInput(id, f$label, value = value, width = "100%")
        )
      })
    })

    shiny::observeEvent(input$save, {
      current <- store$settings_df()
      new_values <- purrr::map_chr(settings_fields, \(f) {
        value <- input[[paste0("set_", f$key)]]
        if (is.null(value) || length(value) == 0 || is.na(value)) "" else as.character(value)
      })
      keys <- purrr::map_chr(settings_fields, "key")
      updated <- dplyr::bind_rows(current, dplyr::filter(default_settings(), !key %in% current$key)) |>
        dplyr::mutate(value = dplyr::if_else(key %in% keys, new_values[match(key, keys)], value))
      error <- tryCatch({
        db_write_settings(updated)
        NULL
      }, error = function(e) conditionMessage(e))
      if (is.null(error)) {
        store$touch("settings")
        shiny::showNotification("Settings saved.", type = "message")
      } else {
        shiny::showNotification(paste("Not saved:", error), type = "error", duration = NULL)
      }
    })

    shiny::observeEvent(input$save_bodyweight, {
      kg <- as_number(input$bodyweight)
      shiny::req(!is.na(kg), kg > 0)
      error <- tryCatch({
        db_write_bodyweight(tibble::tibble(date = format(store$today()), kg = kg))
        NULL
      }, error = function(e) conditionMessage(e))
      if (is.null(error)) {
        store$touch("bodyweight")
        shiny::showNotification("Bodyweight logged.", type = "message")
      } else {
        shiny::showNotification(paste("Not saved:", error), type = "error", duration = NULL)
      }
    })

    output$bodyweight_last <- shiny::renderUI({
      bw <- store$bodyweight() |> dplyr::arrange(dplyr::desc(date)) |> utils::head(1)
      if (nrow(bw) == 0) return(NULL)
      shiny::div(class = "chart-note", sprintf("Last: %s kg on %s", format_kg(bw$kg), format_date_short(bw$date)))
    })

    output$template <- shiny::renderUI({
      t <- store$template()
      days <- t |> dplyr::distinct(day, day_label) |> dplyr::arrange(factor(day, levels = weekday_codes()))
      purrr::map(seq_len(nrow(days)), \(i) {
        rows <- t[t$day == days$day[i], ] |> dplyr::arrange(order)
        bslib::card(
          bslib::card_header(days$day_label[i]),
          shiny::div(class = "table-scroll", shiny::tags$table(
            class = "table table-sm data-table template-table",
            shiny::tags$thead(shiny::tags$tr(purrr::map(c("#", "Exercise", "Type", "Sets x reps", "% (w1/w2/w3) or kg", "Rest"), shiny::tags$th))),
            shiny::tags$tbody(purrr::map(seq_len(nrow(rows)), \(j) {
              r <- rows[j, ]
              reps <- if (!is.na(r$reps_max) && r$reps_max != r$reps_min) paste0(r$reps_min, "–", r$reps_max) else r$reps_min
              load <- if (r$scheme == "pct") {
                paste(dplyr::coalesce(as.character(round(100 * c(r$pct_w1, r$pct_w2, r$pct_w3))), "–"), collapse = "/")
              } else if (!is.na(r$weight_kg)) paste(format_kg(r$weight_kg), "kg") else "by effort"
              shiny::tags$tr(
                shiny::tags$td(r$order), shiny::tags$td(r$exercise), shiny::tags$td(r$set_type),
                shiny::tags$td(paste0(r$sets, " × ", reps)), shiny::tags$td(load),
                shiny::tags$td(if (!is.na(r$rest_s)) paste0(r$rest_s, " s"))
              )
            }))
          ))
        )
      })
    })
  })
}
