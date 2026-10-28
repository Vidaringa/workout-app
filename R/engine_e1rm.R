# Estimated 1RM and its smoothed trend.

# Epley. A single rep is the 1RM itself.
epley <- function(kg, reps) {
  dplyr::if_else(reps <= 1, kg, kg * (1 + reps / 30))
}

# One e1RM per logged main-lift set. Reps in reserve are added to the reps when
# logged; a set without RIR could have had more reps in it, so its e1RM is only
# a lower bound.
e1rm_points <- function(sets, lifts, max_reps = 10) {
  sets |>
    dplyr::filter(
      lift %in% lifts,
      set_type %in% c("work", "top", "joker", "backoff"),
      !is.na(actual_kg), !is.na(actual_reps),
      actual_reps >= 1, actual_reps <= max_reps
    ) |>
    dplyr::mutate(
      date = as.Date(date),
      reps_eff = actual_reps + dplyr::coalesce(rir, 0),
      e1rm = epley(actual_kg, reps_eff),
      lower_bound = is.na(rir)
    )
}

# Best e1RM per lift and day.
e1rm_daily <- function(points) {
  points |>
    dplyr::group_by(lift, date) |>
    dplyr::slice_max(e1rm, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(lift, date, e1rm, lower_bound, actual_kg, actual_reps, rir) |>
    dplyr::arrange(lift, date)
}

# Local linear trend (level + slope) state-space model on log e1RM, fitted
# with a Kalman filter and Rauch-Tung-Striebel smoother.
#
# Why this model: sessions are irregular and single sets are noisy (a good or
# bad day moves e1RM by a few percent). The filter treats each day's best e1RM
# as a noisy reading of a latent strength level that drifts slowly, with time
# measured in weeks so gaps between sessions widen the uncertainty. Working
# on the log scale makes the noise proportional (a 3 % day-to-day wobble
# whether you lift 60 or 200 kg) and the slope a weekly % change. The slope
# state gives the goal projection with an honest uncertainty range.
#
# Lower-bound readings (no RIR logged) are softly censored. Above the
# predicted level they count fully ("at least this strong"). Below it they
# still pull, but with `lower_bound_factor` times the measurement noise: a
# 5-rep set with unknown reps in reserve is weak evidence of being weaker.
# (Ignoring them entirely lets the trend ratchet upward forever when no RIR
# is logged, as in the imported spreadsheet history.)
e1rm_trend <- function(daily, obs_sd = 0.03, level_sd = 0.005, slope_sd = 0.002,
                       slope_prior_sd = 0.01, lower_bound_factor = 3) {
  n <- nrow(daily)
  if (n == 0) return(NULL)

  y <- log(daily$e1rm)
  t <- as.numeric(daily$date - daily$date[1]) / 7
  r <- obs_sd^2

  x_pred <- x_filt <- matrix(NA_real_, n, 2)
  p_pred <- p_filt <- vector("list", n)
  transition <- vector("list", n)

  x <- c(y[1], 0)
  p <- diag(c(obs_sd^2 * 4, slope_prior_sd^2))
  for (i in seq_len(n)) {
    dt <- if (i == 1) 0 else t[i] - t[i - 1]
    f <- matrix(c(1, 0, dt, 1), 2)
    q <- matrix(c(level_sd^2 * dt + slope_sd^2 * dt^3 / 3, slope_sd^2 * dt^2 / 2,
                  slope_sd^2 * dt^2 / 2, slope_sd^2 * dt), 2)
    x <- as.vector(f %*% x)
    p <- f %*% p %*% t(f) + q
    x_pred[i, ] <- x
    p_pred[[i]] <- p
    transition[[i]] <- f

    censored_below <- daily$lower_bound[i] && y[i] < x[1]
    s <- p[1, 1] + if (censored_below) r * lower_bound_factor^2 else r
    k <- p[, 1] / s
    x <- x + k * (y[i] - x[1])
    p <- p - k %*% t(p[1, ])
    x_filt[i, ] <- x
    p_filt[[i]] <- p
  }

  x_smooth <- x_filt
  p_smooth <- p_filt
  if (n > 1) {
    for (i in (n - 1):1) {
      gain <- p_filt[[i]] %*% t(transition[[i + 1]]) %*% solve(p_pred[[i + 1]])
      x_smooth[i, ] <- x_filt[i, ] + as.vector(gain %*% (x_smooth[i + 1, ] - x_pred[i + 1, ]))
      p_smooth[[i]] <- p_filt[[i]] + gain %*% (p_smooth[[i + 1]] - p_pred[[i + 1]]) %*% t(gain)
    }
  }

  level_sd_hat <- purrr::map_dbl(p_smooth, \(m) sqrt(m[1, 1]))
  z80 <- stats::qnorm(0.9)
  z95 <- stats::qnorm(0.975)
  trend <- tibble::tibble(
    date = daily$date,
    level = exp(x_smooth[, 1]),
    lo80 = exp(x_smooth[, 1] - z80 * level_sd_hat),
    hi80 = exp(x_smooth[, 1] + z80 * level_sd_hat),
    lo95 = exp(x_smooth[, 1] - z95 * level_sd_hat),
    hi95 = exp(x_smooth[, 1] + z95 * level_sd_hat)
  )
  attr(trend, "final") <- list(
    date = daily$date[n], log_level = x_filt[n, 1], slope = x_filt[n, 2],
    cov = p_filt[[n]]
  )
  trend
}

# All lifts at once: returns the trend rows with a lift column, and the final
# state per lift as an attribute.
e1rm_trends <- function(daily, settings) {
  by_lift <- split(daily, daily$lift)
  fits <- purrr::map(by_lift, \(d) e1rm_trend(
    d,
    obs_sd = setting_num(settings, "trend_obs_sd"),
    level_sd = setting_num(settings, "trend_level_sd"),
    slope_sd = setting_num(settings, "trend_slope_sd"),
    lower_bound_factor = setting_num(settings, "trend_lower_bound_factor")
  ))
  trends <- purrr::imap(fits, \(fit, lift) dplyr::mutate(fit, lift = lift)) |>
    purrr::list_rbind()
  attr(trends, "final") <- purrr::map(fits, \(fit) attr(fit, "final"))
  trends
}

# Current smoothed e1RM per lift (the filter's last state).
e1rm_current <- function(trends) {
  final <- attr(trends, "final")
  if (length(final) == 0) return(tibble::tibble(lift = character(), e1rm = numeric(), as_of = as.Date(character()), weekly_change = numeric()))
  purrr::imap(final, \(f, lift) tibble::tibble(
    lift = lift, e1rm = exp(f$log_level), as_of = f$date, weekly_change = exp(f$slope) - 1
  )) |>
    purrr::list_rbind()
}

# When does the trend reach `goal` kg? Uses the 80 % interval of the slope,
# so the answer is a date range. Level uncertainty is ignored, and the trend
# is assumed to continue: treat it as a rough guide, not a forecast.
goal_projection <- function(final, goal, level = 0.8) {
  empty <- list(status = "no_data", earliest = as.Date(NA), latest = as.Date(NA), current = NA_real_)
  if (is.null(final) || is.na(goal)) return(empty)
  gap <- log(goal) - final$log_level
  current <- exp(final$log_level)
  if (gap <= 0) return(list(status = "reached", earliest = final$date, latest = final$date, current = current))

  z <- stats::qnorm(1 - (1 - level) / 2)
  slope_sd <- sqrt(final$cov[2, 2])
  slope_hi <- final$slope + z * slope_sd
  slope_lo <- final$slope - z * slope_sd
  weeks_to_date <- function(weeks) final$date + round(weeks * 7)
  if (slope_hi <= 0) {
    return(list(status = "no_trend", earliest = as.Date(NA), latest = as.Date(NA), current = current))
  }
  list(
    status = if (slope_lo > 0) "range" else "open",
    earliest = weeks_to_date(gap / slope_hi),
    latest = if (slope_lo > 0) weeks_to_date(gap / slope_lo) else as.Date(NA),
    current = current
  )
}
