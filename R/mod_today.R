# Today: log the next session set by set.
#
# The set cards are plain HTML driven by www/app.js: steppers, RIR chips and
# the rest timer run in the browser, and a tap on a check sends
# {sets: [{id, kg, reps, rir}]} to input$log. The cards are rendered once per
# session choice and are not re-rendered when a set is saved, so unsaved
# values on other cards are never lost.

mod_today_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "screen",
    shiny::selectInput(ns("session"), NULL, choices = NULL, width = "100%"),
    shiny::uiOutput(ns("session_info")),
    shiny::uiOutput(ns("cards")),
    shiny::div(
      class = "today-actions",
      shiny::actionButton(ns("finish"), "Finish session", icon = shiny::icon("flag-checkered"),
                          class = "btn-primary btn-lg w-100"),
      shiny::actionButton(ns("log_run"), "Log a run", icon = shiny::icon("person-running"),
                          class = "btn-outline-secondary btn-lg w-100 mt-2")
    )
  )
}

session_key <- function(round, week, day) sprintf("r%d-w%d-%s", as.integer(round), as.integer(week), day)

session_choice_labels <- function(sessions) {
  week <- dplyr::if_else(sessions$week == 4, "Week 4 (deload/test)", paste("Week", sessions$week))
  paste0(week, " · ", sessions$day_label)
}

as_number <- function(x) {
  if (is.null(x) || length(x) == 0 || identical(x, "")) NA_real_ else suppressWarnings(as.numeric(x))
}

stepper <- function(field, value, step, unit) {
  shiny::div(
    class = "stepper", `data-field` = field, `data-step` = step,
    shiny::tags$button(type = "button", class = "step-btn", `data-dir` = "-1", `aria-label` = paste("Less", unit), "−"),
    shiny::tags$input(type = "text", inputmode = "decimal", class = "step-input",
                      value = format_kg(value), `aria-label` = unit, autocomplete = "off"),
    shiny::span(class = "step-unit", unit),
    shiny::tags$button(type = "button", class = "step-btn", `data-dir` = "1", `aria-label` = paste("More", unit), "+")
  )
}

set_card <- function(r, label, kg_step) {
  target_reps <- if (!is.na(r$planned_reps_max) && r$planned_reps_max != r$planned_reps) {
    paste0(r$planned_reps, "–", r$planned_reps_max)
  } else {
    as.character(r$planned_reps)
  }
  target <- paste0(if (!is.na(r$planned_kg)) paste0(format_kg(r$planned_kg), " kg ") else "", "× ", target_reps)
  with_rir <- r$set_type %in% c("top", "joker")

  shiny::div(
    class = paste("set-card", paste0("type-", r$set_type), if (isTRUE(r$is_logged)) "is-logged"),
    `data-set-id` = r$set_id,
    shiny::div(
      class = "set-top",
      shiny::div(
        class = "set-name",
        label,
        if (!is.na(r$pct)) shiny::span(class = "set-pct", paste0(round(r$pct * 100), "%")),
        shiny::div(class = "set-target", target)
      ),
      shiny::tags$button(type = "button", class = "done-btn", `aria-label` = "Log set", shiny::icon("check"))
    ),
    shiny::div(
      class = "set-inputs",
      stepper("kg", r$prefill_kg, kg_step, "kg"),
      stepper("reps", r$prefill_reps, 1, "reps")
    ),
    if (with_rir) {
      shiny::div(
        class = "rir-row",
        shiny::span(class = "rir-label", "RIR"),
        purrr::map(0:4, \(v) shiny::tags$button(
          type = "button", `data-rir` = v,
          class = paste("rir-btn", if (isTRUE(r$logged_rir == v)) "active"), v
        ))
      )
    },
    shiny::div(class = "set-status", role = "status")
  )
}

set_label <- function(set_type, index) {
  switch(set_type,
    warmup = paste("Warm-up", index),
    work = paste("Set", index),
    top = "Top set",
    joker = paste("Joker", index),
    backoff = paste("Back-off", index),
    accessory = paste("Set", index),
    paste(set_type, index)
  )
}

exercise_group <- function(rows, settings) {
  first <- rows[1, ]
  kg_step <- if (!is.na(first$lift)) lift_setting(settings, "rounding", first$lift) else setting_num(settings, "accessory_increment")
  rows <- rows |>
    dplyr::group_by(set_type) |>
    dplyr::mutate(index = dplyr::row_number()) |>
    dplyr::ungroup()
  cards <- function(type) {
    sub <- rows[rows$set_type %in% type, ]
    purrr::map(seq_len(nrow(sub)), \(i) set_card(sub[i, ], set_label(sub$set_type[i], sub$index[i]), kg_step))
  }
  kg_list <- function(type) paste(format_kg(rows$planned_kg[rows$set_type == type]), collapse = " · ")

  warmups <- rows$set_type == "warmup"
  jokers <- rows$set_type == "joker"
  sub_lines <- purrr::compact(list(
    if (!is.na(first$notes) && first$notes != "" && first$scheme != "pct") first$notes,
    if (!is.na(first$last_text)) first$last_text
  ))

  shiny::div(
    class = "set-group",
    shiny::h3(class = "exercise-name", first$exercise),
    purrr::map(sub_lines, \(line) shiny::div(class = "exercise-sub", line)),
    if (!is.na(first$hint)) {
      shiny::tags$button(type = "button", class = "hint-btn", `data-kg` = first$hint_kg,
                         `data-reps` = first$hint_reps, shiny::icon("arrow-trend-up"), first$hint)
    },
    if (any(warmups)) {
      shiny::tags$details(
        class = "set-fold",
        open = if (!all(rows$is_logged[warmups])) NA,
        shiny::tags$summary(paste0("Warm-ups · ", kg_list("warmup"), " kg"),
                            if (all(rows$is_logged[warmups])) shiny::span(class = "fold-done", shiny::icon("check"))),
        shiny::tags$button(type = "button", class = "btn btn-outline-primary w-100 log-all-btn",
                           shiny::icon("check-double"), " Log all warm-ups as planned"),
        cards("warmup")
      )
    },
    cards(c("work", "top", "accessory")),
    if (any(jokers)) {
      shiny::tags$details(
        class = "set-fold joker-fold",
        open = if (any(rows$is_logged[jokers])) NA,
        shiny::tags$summary(paste0("Jokers (optional) · ", kg_list("joker"), " kg")),
        cards("joker")
      )
    },
    cards("backoff")
  )
}

session_cards <- function(filled, log_input, settings) {
  groups <- split(filled, factor(filled$exercise, levels = unique(filled$exercise)))
  shiny::div(
    class = "session-cards", `data-log-input` = log_input,
    `data-rest-timer` = setting_chr(settings, "rest_timer"),
    purrr::map(groups, \(g) exercise_group(g, settings))
  )
}

mod_today_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(store$plan(), {
      sessions <- round_sessions(store$plan())
      choices <- rlang::set_names(sessions$session_key, session_choice_labels(sessions))
      current <- shiny::isolate(input$session)
      selected <- if (!is.null(current) && current %in% choices) {
        current
      } else {
        next_up <- next_session(store$plan(), shiny::isolate(store$sets()), shiny::isolate(store$today()))
        if (nrow(next_up) == 1) next_up$session_key else utils::tail(choices, 1)
      }
      shiny::updateSelectInput(session, "session", choices = choices, selected = selected)
    })

    session_rows <- shiny::reactive({
      shiny::req(input$session)
      store$plan() |> dplyr::filter(session_key(round, week, day) == input$session)
    })

    rendered <- shiny::reactiveVal(NULL)

    output$cards <- shiny::renderUI({
      rows <- session_rows()
      shiny::req(nrow(rows) > 0)
      filled <- prefill_session(rows, shiny::isolate(store$sets()), shiny::isolate(store$today()),
                                store$settings())
      rendered(filled)
      session_cards(filled, ns("log"), store$settings())
    })

    output$session_info <- shiny::renderUI({
      rows <- session_rows()
      shiny::req(nrow(rows) > 0)
      sets <- store$sets()
      done <- sets |>
        dplyr::filter(set_id %in% rows$set_id, !is.na(actual_kg) | !is.na(actual_reps))
      required <- sum(rows$set_type != "joker")
      next_up <- next_session(store$plan(), sets, store$today())
      tms <- round_tms(store$training_max(), store$round())
      main <- rows |> dplyr::filter(!is.na(lift), scheme == "pct") |> dplyr::distinct(lift)
      tm_text <- main |>
        dplyr::left_join(tms, by = "lift") |>
        dplyr::mutate(text = paste0(lift_label(lift), " TM ", format_kg(tm))) |>
        dplyr::pull(text)
      shiny::div(
        class = "session-info",
        shiny::span(paste0("Round ", rows$round[1], " · ", paste(tm_text, collapse = " · "))),
        shiny::span(class = "session-progress", sprintf("%d / %d sets", nrow(done), required)),
        if (nrow(next_up) == 0) {
          shiny::div(class = "round-done", shiny::icon("flag-checkered"),
                     " Round done. Generate the next one on the Plan tab.")
        }
      )
    })

    shiny::observeEvent(input$log, {
      rows <- rendered()
      shiny::req(rows)
      tz <- store$tz()
      logged <- shiny::isolate(store$sets())
      saved_any <- FALSE
      for (s in input$log$sets) {
        row <- rows[rows$set_id == s$id, ]
        if (nrow(row) != 1) next
        previous <- logged |> dplyr::filter(set_id == s$id)
        new <- tibble::tibble(
          set_id = row$set_id,
          date = if (nrow(previous) > 0 && !is.na(previous$date[1])) previous$date[1] else format(store$today()),
          round = row$round, week = row$week, day = row$day, exercise = row$exercise,
          lift = row$lift, set_no = row$set_no, set_type = row$set_type,
          planned_kg = dplyr::coalesce(row$planned_kg, row$last_kg),
          planned_reps = row$planned_reps, planned_reps_max = row$planned_reps_max,
          actual_kg = as_number(s$kg), actual_reps = as_number(s$reps), rir = as_number(s$rir),
          notes = NA_character_, logged_at = now_iso(tz), date_estimated = FALSE
        )
        error <- tryCatch({
          db_write_set(new)
          NULL
        }, error = function(e) conditionMessage(e))
        if (is.null(error)) {
          saved_any <- TRUE
          rest <- if (length(input$log$sets) == 1 && setting_chr(store$settings(), "rest_timer") == "on") row$rest_s else NA
          session$sendCustomMessage("set-saved", list(id = s$id, rest = if (is.na(rest)) NULL else rest))
        } else {
          session$sendCustomMessage("set-failed", list(id = s$id, message = error))
        }
      }
      if (saved_any) store$touch("sets")
    })

    shiny::observeEvent(input$finish, {
      rows <- session_rows()
      summary <- session_summary(store$sets(), rows$round[1], rows$week[1], rows$day[1])
      required <- sum(rows$set_type != "joker")
      tops <- summary$tops
      shiny::showModal(shiny::modalDialog(
        title = paste("Session summary ·", rows$day_label[1]),
        easyClose = TRUE,
        footer = shiny::modalButton("Close"),
        shiny::div(
          class = "summary-tiles",
          stat_tile("Sets", sprintf("%d / %d", summary$sets, required)),
          stat_tile("Exercises", summary$exercises),
          stat_tile("Volume", paste0(format(round(summary$tonnage), big.mark = " "), " kg"))
        ),
        if (nrow(tops) > 0) {
          shiny::tags$table(
            class = "table table-sm summary-table",
            shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Set"), shiny::tags$th("Target"),
                                             shiny::tags$th("Done"), shiny::tags$th("e1RM"))),
            shiny::tags$tbody(purrr::map(seq_len(nrow(tops)), \(i) shiny::tags$tr(
              shiny::tags$td(paste(tops$exercise[i], tops$set_type[i])),
              shiny::tags$td(tops$target[i]),
              shiny::tags$td(tops$result[i], " ", if (tops$hit[i]) status_badge("ok", "hit") else status_badge("warn", "short")),
              shiny::tags$td(format_kg(round(tops$e1rm[i], 1)))
            )))
          )
        } else {
          empty_state("No top sets logged in this session yet.")
        }
      ))
    })

    run_logger(input, session, store, "log_run")
  })
}
