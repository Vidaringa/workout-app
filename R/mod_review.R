# Review: a weekly coaching note from Claude, saved to ai_reviews. The note
# is advice only; nothing here changes the plan or the training maxes.

mod_review_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "screen",
    shiny::actionButton(ns("generate"), "Get this week's review", icon = shiny::icon("comments"),
                        class = "btn-primary btn-lg w-100"),
    shiny::div(class = "chart-note", shiny::textOutput(ns("model_note"), inline = TRUE)),
    shiny::uiOutput(ns("latest")),
    shiny::tags$details(
      class = "chart-note",
      shiny::tags$summary("What is sent to Claude"),
      shiny::p("A compact JSON summary computed in R: the last weeks of top sets and jokers with RIR, e1RM trends,",
               "TM / e1RM ratios, missed targets, accessory trends, runs, weekly load, race predictions, your goals",
               "and constraints. No raw spreadsheet, no personal data beyond that.")
    ),
    shiny::h3(class = "section-title", "Earlier reviews"),
    shiny::uiOutput(ns("history"))
  )
}

mod_review_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    output$model_note <- shiny::renderText(paste("Model:", setting_chr(store$settings(), "claude_model"),
                                                 "· takes 10-40 seconds"))

    shiny::observeEvent(input$generate, {
      settings <- store$settings()
      model <- setting_chr(settings, "claude_model")
      summary <- review_summary(store$sets(), store$runs_m(), store$training_max(), settings,
                                store$e1rm(), store$today(), store$bodyweight())
      result <- shiny::withProgress(message = "Asking Claude for a review...", value = 0.3, {
        tryCatch(run_review(summary, model), error = function(e) e)
      })
      if (inherits(result, "error")) {
        shiny::showNotification(paste("Review failed:", conditionMessage(result)), type = "error", duration = NULL)
        return()
      }
      # A Google Sheets cell holds at most 50,000 characters.
      row <- tibble::tibble(
        date = now_iso(store$tz()), model = model,
        input_summary_json = substr(result$input_json, 1, 49000),
        response_md = substr(result$response_md, 1, 49000)
      )
      error <- tryCatch({
        db_write_ai_review(row)
        NULL
      }, error = function(e) conditionMessage(e))
      if (!is.null(error)) {
        shiny::showNotification(paste("Review shown but not saved:", error), type = "warning", duration = NULL)
        pending(row)
      } else {
        pending(NULL)
        store$touch("ai_reviews")
      }
    })

    pending <- shiny::reactiveVal(NULL)

    reviews <- shiny::reactive({
      dplyr::bind_rows(pending(), store$ai_reviews()) |> dplyr::arrange(dplyr::desc(date))
    })

    output$latest <- shiny::renderUI({
      r <- reviews()
      if (nrow(r) == 0) return(empty_state("No reviews yet."))
      bslib::card(
        class = "review-card",
        bslib::card_header(paste0(format(as.Date(substr(r$date[1], 1, 10)), "%d %b %Y"), " · ", r$model[1])),
        shiny::markdown(r$response_md[1])
      )
    })

    output$history <- shiny::renderUI({
      r <- reviews()
      if (nrow(r) <= 1) return(empty_state("Older reviews will show here."))
      purrr::map(2:nrow(r), \(i) shiny::tags$details(
        class = "review-old",
        shiny::tags$summary(paste0(format(as.Date(substr(r$date[i], 1, 10)), "%d %b %Y"), " · ", r$model[i])),
        shiny::markdown(r$response_md[i])
      ))
    })
  })
}
