# 5/3/1 training app. Files in R/ are sourced automatically by Shiny.

library(shiny)
library(bslib)

ui <- page_navbar(
  title = "5/3/1",
  id = "nav",
  theme = app_theme(),
  fillable = FALSE,
  navbar_options = navbar_options(position = "fixed-bottom", collapsible = FALSE),
  header = tagList(
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
      tags$meta(name = "theme-color", content = "#fcfcfb"),
      tags$link(rel = "stylesheet", href = "app.css"),
      tags$script(src = "app.js")
    ),
    login_ui(),
    div(id = "rest-timer", hidden = NA, role = "timer", `aria-live` = "polite",
        span(class = "rest-label", "Rest"), span(class = "rest-time", "0:00"),
        span(class = "rest-hint", "tap to stop"))
  ),
  nav_panel(span(icon("dumbbell"), span(class = "nav-text", "Today")), value = "today", mod_today_ui("today")),
  nav_panel(span(icon("table-list"), span(class = "nav-text", "Plan")), value = "plan", mod_plan_ui("plan")),
  nav_panel(span(icon("chart-line"), span(class = "nav-text", "Progress")), value = "progress", mod_progress_ui("progress")),
  nav_panel(span(icon("person-running"), span(class = "nav-text", "Run")), value = "running", mod_running_ui("running")),
  nav_panel(span(icon("comments"), span(class = "nav-text", "Review")), value = "review", mod_review_ui("review")),
  nav_panel(span(icon("gear"), span(class = "nav-text", "Settings")), value = "settings", mod_settings_ui("settings"))
)

server <- function(input, output, session) {
  session$allowReconnect(TRUE)
  auth_server(input, output, session, on_login = function() {
    store <- app_store()
    mod_today_server("today", store)
    mod_plan_server("plan", store)
    mod_progress_server("progress", store)
    mod_running_server("running", store)
    mod_review_server("review", store)
    mod_settings_server("settings", store)
  })
}

shinyApp(ui, server)
