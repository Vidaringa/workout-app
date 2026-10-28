# Plan: the current and the next round laid out like the spreadsheet, TM
# health, and "Finish round -> generate next round".

mod_plan_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "screen",
    shiny::uiOutput(ns("health")),
    shiny::radioButtons(ns("which"), NULL, inline = TRUE,
                        c("This round" = "current", "Next round (projected)" = "next")),
    shiny::uiOutput(ns("round_header")),
    shiny::uiOutput(ns("weeks")),
    shiny::actionButton(ns("finish_round"), "Finish round → generate next round",
                        icon = shiny::icon("forward"), class = "btn-primary btn-lg w-100 mt-3")
  )
}

health_line <- function(health) {
  shiny::div(
    class = "health-list",
    purrr::map(seq_len(nrow(health)), \(i) {
      h <- health[i, ]
      shiny::div(
        class = "health-item",
        shiny::span(class = "health-lift", lift_label(h$lift)),
        shiny::span(class = "health-detail", if (is.na(h$e1rm)) {
          paste0("TM ", format_kg(h$tm), " · no e1RM yet")
        } else {
          sprintf("TM %s = %d%% of e1RM %s", format_kg(h$tm), round(100 * h$ratio), format_kg(round(h$e1rm, 1)))
        }),
        status_badge(h$status)
      )
    })
  )
}

plan_day_table <- function(day_rows) {
  lines <- day_rows |>
    dplyr::group_by(exercise, order, set_type, pct, planned_kg, planned_reps, planned_reps_max) |>
    dplyr::summarise(sets = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(order) |>
    dplyr::group_by(exercise, set_type) |>
    dplyr::mutate(index = dplyr::row_number()) |>
    dplyr::ungroup()
  exercises <- unique(lines$exercise)

  shiny::tags$table(
    class = "table table-sm plan-table",
    shiny::tags$tbody(purrr::map(exercises, \(ex) {
      ex_lines <- lines[lines$exercise == ex, ]
      shiny::tagList(
        shiny::tags$tr(class = "plan-exercise", shiny::tags$td(colspan = 3, ex)),
        purrr::map(seq_len(nrow(ex_lines)), \(i) {
          l <- ex_lines[i, ]
          reps <- if (!is.na(l$planned_reps_max) && l$planned_reps_max != l$planned_reps) {
            paste0(l$planned_reps, "–", l$planned_reps_max)
          } else {
            l$planned_reps
          }
          label <- if (l$set_type %in% c("backoff", "accessory") || (l$set_type == "work" && is.na(l$pct))) {
            if (l$set_type == "backoff") "Back-off" else "Work"
          } else {
            set_label(l$set_type, l$index)
          }
          shiny::tags$tr(
            class = paste0("plan-", l$set_type),
            shiny::tags$td(label),
            shiny::tags$td(class = "plan-pct", if (!is.na(l$pct)) paste0(round(l$pct * 100), "%")),
            shiny::tags$td(class = "plan-load",
                           paste0(if (!is.na(l$planned_kg)) paste0(format_kg(l$planned_kg), " kg ") else "by effort ",
                                  if (l$sets > 1) paste0(l$sets, " × ") else "× ", reps))
          )
        })
      )
    }))
  )
}

plan_weeks <- function(plan) {
  weeks <- sort(unique(plan$week))
  panels <- purrr::map(weeks, \(w) {
    week_rows <- plan[plan$week == w, ]
    days <- round_sessions(week_rows)
    bslib::nav_panel(
      if (w == 4) "W4 deload" else paste0("Week ", w),
      purrr::map(seq_len(nrow(days)), \(i) bslib::card(
        class = "plan-day",
        bslib::card_header(days$day_label[i]),
        plan_day_table(week_rows[week_rows$day == days$day[i], ])
      ))
    )
  })
  do.call(bslib::navset_pill, panels)
}

mod_plan_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    proposals <- function(mode) {
      propose_next_tms(store$sets(), store$training_max(), store$round(), store$settings(),
                       store$e1rm()$current, mode = mode)
    }

    shown <- shiny::reactive({
      if (identical(input$which, "next")) {
        next_tms <- proposals(setting_chr(store$settings(), "progression_mode"))
        list(round = store$round() + 1,
             tms = dplyr::select(next_tms, lift, tm = proposed_tm),
             plan = generate_round(store$template(), dplyr::select(next_tms, lift, tm = proposed_tm),
                                   store$round() + 1, store$settings()),
             projected = TRUE)
      } else {
        list(round = store$round(), tms = round_tms(store$training_max(), store$round()),
             plan = store$plan(), projected = FALSE)
      }
    })

    output$health <- shiny::renderUI(health_line(store$health()))

    output$round_header <- shiny::renderUI({
      s <- shown()
      shiny::div(
        class = "round-header",
        shiny::h2(paste("Round", s$round)),
        shiny::div(class = "round-tms", paste(paste(lift_label(s$tms$lift), format_kg(s$tms$tm)), collapse = " · ")),
        if (s$projected) {
          shiny::div(class = "chart-note", sprintf("Projected with the %s mode. Nothing is saved until you finish the round.",
                                                   setting_chr(store$settings(), "progression_mode")))
        }
      )
    })

    output$weeks <- shiny::renderUI(plan_weeks(shown()$plan))

    shiny::observeEvent(input$finish_round, {
      next_round <- store$round() + 1
      shiny::showModal(shiny::modalDialog(
        title = paste("Generate round", next_round),
        size = "l",
        easyClose = FALSE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("confirm_round"), paste("Save round", next_round), class = "btn-primary")
        ),
        shiny::selectInput(ns("finish_mode"), "Progression mode",
                           c("Standard (+5 / +2.5)" = "standard", "Conditional (week-3 top set)" = "conditional",
                             "e1RM-anchored (cap)" = "e1rm"),
                           selected = setting_chr(store$settings(), "progression_mode"), width = "100%"),
        shiny::uiOutput(ns("proposal_table")),
        if (deload_due(next_round, store$settings())) {
          shiny::div(class = "chart-note", shiny::icon("bed"), sprintf(
            " Round %d includes a 4th week (%s), per the deload setting. You can skip it by finishing the round early.",
            next_round, gsub("_", " ", setting_chr(store$settings(), "deload_type"))))
        },
        shiny::div(class = "text-danger", shiny::textOutput(ns("round_error")))
      ))
    })

    finish_proposal <- shiny::reactive({
      shiny::req(input$finish_mode)
      proposals(input$finish_mode)
    })

    output$proposal_table <- shiny::renderUI({
      p <- finish_proposal()
      health <- tm_health(p$current_tm, p$e1rm, store$settings())
      shiny::tags$table(
        class = "table table-sm proposal-table",
        shiny::tags$thead(shiny::tags$tr(purrr::map(c("Lift", "Now", "e1RM", "New TM", "Why"), shiny::tags$th))),
        shiny::tags$tbody(purrr::map(seq_len(nrow(p)), \(i) shiny::tags$tr(
          shiny::tags$td(lift_label(p$lift[i])),
          shiny::tags$td(format_kg(p$current_tm[i]),
                         if (!is.na(p$e1rm[i])) shiny::div(status_badge(health$status[i], paste0(round(100 * health$ratio[i]), "%")))),
          shiny::tags$td(format_kg(round(p$e1rm[i], 1))),
          shiny::tags$td(shiny::numericInput(ns(paste0("tm_", p$lift[i])), NULL, value = p$proposed_tm[i],
                                             step = lift_setting(store$settings(), "rounding", p$lift[i]), width = "90px")),
          shiny::tags$td(class = "proposal-reason", p$reason[i])
        )))
      )
    })

    shiny::observeEvent(input$confirm_round, {
      p <- finish_proposal()
      chosen <- purrr::map_dbl(p$lift, \(l) as_number(input[[paste0("tm_", l)]]))
      if (anyNA(chosen) || any(chosen <= 0)) {
        output$round_error <- shiny::renderText("Every lift needs a training max.")
        return()
      }
      rows <- tibble::tibble(
        round = store$round() + 1, lift = p$lift, tm = chosen,
        method = dplyr::if_else(abs(chosen - p$proposed_tm) < 1e-9, p$method, "manual"),
        decided_at = now_iso(store$tz())
      )
      error <- tryCatch({
        db_write_training_max(rows)
        NULL
      }, error = function(e) conditionMessage(e))
      if (is.null(error)) {
        shiny::removeModal()
        store$touch("training_max")
        shiny::updateRadioButtons(session, "which", selected = "current")
        shiny::showNotification(paste("Round", rows$round[1], "saved."), type = "message")
      } else {
        output$round_error <- shiny::renderText(paste("Not saved:", error))
      }
    })
  })
}
