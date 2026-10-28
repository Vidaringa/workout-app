# Shared UI pieces: theme, chart styling, labels.

lift_labels <- c(deadlift = "Deadlift", bench = "Bench", ohp = "OHP", front_squat = "Front squat")

lift_label <- function(lift) dplyr::coalesce(unname(lift_labels[lift]), lift)

# Categorical slots 1-3 of the reference palette (validated all-pairs for
# colour-vision deficiency); colour follows the lift, never its rank.
lift_colors <- c(deadlift = "#2a78d6", bench = "#eb6834", ohp = "#1baf7a")

# Status colours are reserved for TM health and load flags, always shown with
# an icon and a label.
status_colors <- c(ok = "#0ca30c", warn = "#fab219", flag = "#d03b3b", unknown = "#8a8984")
status_labels <- c(ok = "OK", warn = "Watch", flag = "Too high", unknown = "No data")
status_icons <- c(ok = "circle-check", warn = "triangle-exclamation", flag = "circle-exclamation", unknown = "circle-question")

ink <- list(primary = "#0b0b0b", secondary = "#52514e", muted = "#8a8984", grid = "#e8e7e3",
            surface = "#fcfcfb")

app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#fcfcfb", fg = "#0b0b0b", primary = "#2a78d6",
    base_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"),
    "border-radius" = "0.75rem"
  )
}

theme_app <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = ink$surface, colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = ink$grid, linewidth = 0.3),
      axis.text = ggplot2::element_text(colour = ink$secondary, size = 10),
      axis.title = ggplot2::element_text(colour = ink$secondary, size = 10),
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = ink$secondary),
      strip.text = ggplot2::element_text(colour = ink$primary, hjust = 0, face = "bold"),
      plot.margin = ggplot2::margin(4, 8, 4, 4)
    )
}

status_badge <- function(status, text = status_labels[[status]]) {
  shiny::span(
    class = paste("status-badge", paste0("status-", status)),
    shiny::icon(status_icons[[status]]), text
  )
}

# A small label + big number tile.
stat_tile <- function(label, value, sub = NULL) {
  shiny::div(
    class = "stat-tile",
    shiny::div(class = "stat-label", label),
    shiny::div(class = "stat-value", value),
    if (!is.null(sub)) shiny::div(class = "stat-sub", sub)
  )
}

empty_state <- function(text) shiny::div(class = "empty-state", text)

now_iso <- function(tz) format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = tz)

today_in <- function(tz) as.Date(format(Sys.time(), "%Y-%m-%d", tz = tz))

format_pace <- function(seconds_per_km) {
  ifelse(is.na(seconds_per_km), NA_character_, paste0(format_duration(seconds_per_km), " /km"))
}

format_date_short <- function(date) format(as.Date(date), "%d %b")
