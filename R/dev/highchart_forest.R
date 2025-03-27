#' Generate a Highcharts-based forest plot
#'
#' @param dat A dataframe containing at least `study`, `yi` (effect size), and `vi` (variance).
#' @return A Highcharts object displaying a forest plot.
#' @importFrom highcharter highchart hc_chart hc_xAxis hc_yAxis hc_add_series list_parse
#' @importFrom dplyr mutate bind_rows
#' @importFrom metafor rma
#' @export

highchart_forest <- function(dat) {

  library(dplyr)
  library(highcharter)
  library(metafor)
  library(htmlwidgets)

  # Fit a random-effects model
  res <- rma(yi, vi, data = dat, method = "REML", digits = 3, level = 95, test = "knha")
  print(res)

  # Compute confidence and prediction intervals
  plot_df <- data.frame(
    study = dat$study,
    effect = dat$yi,
    lower_CI = dat$yi - 1.96 * sqrt(dat$vi),
    upper_CI = dat$yi + 1.96 * sqrt(dat$vi),
    lower_PI = dat$yi - 1.96 * sqrt(res$tau2 + dat$vi),
    upper_PI = dat$yi + 1.96 * sqrt(res$tau2 + dat$vi)
  )

  # Add pooled effect
  pooled_effect <- res$b[[1]]
  pooled_CI <- c(res$ci.lb, res$ci.ub)
  pooled_PI <- c(res$b - 1.96 * sqrt(res$tau2 + res$se), res$b + 1.96 * sqrt(res$tau2 + res$se))

  pooled_df <- data.frame(
    study = "Pooled Effect",
    effect = pooled_effect,
    lower_CI = pooled_CI[1],
    upper_CI = pooled_CI[2],
    lower_PI = pooled_PI[1],
    upper_PI = pooled_PI[2]
  )

  # Combine data
  plot_df <- bind_rows(plot_df, pooled_df)

  # Find correct x-axis index for "Pooled Effect"
  pooled_index <- which(plot_df$study == "Pooled Effect") - 1  # Zero-based index for Highcharts

  # Create Highcharts plot
  hc_plot <- highchart() |>
    hc_chart(inverted = TRUE) |>
    hc_xAxis(categories = plot_df$study, title = list(text = "Study")) |>
    hc_yAxis(title = list(text = "Effect Size")) |>

    # Add Prediction Interval
    hc_add_series(data = list_parse(mutate(plot_df[1:nrow(dat),], low = lower_PI, high = upper_PI)),
                  type = "errorbar", color = "grey", stemWidth = 1, whiskerLength = 1, name = "Prediction Interval") |>

    # Add Confidence Interval
    hc_add_series(data = list_parse(mutate(plot_df[1:nrow(dat),], low = lower_CI, high = upper_CI)),
                  type = "errorbar", color = "red", stemWidth = 1, whiskerLength = 1, name = "Confidence Interval") |>

    # Add Effect Sizes (Red Points)
    hc_add_series(data = plot_df$effect[1:nrow(dat)], color = "red", type = "scatter", name = "Effect Size") |>

    # Add Pooled Effect
    hc_add_series(data = list(list(x = pooled_index, y = pooled_effect)),
                  type = "scatter", color = "blue", marker = list(symbol = "diamond", radius = 8), name = "Pooled Effect") |>

    # Add Pooled Prediction Interval
    hc_add_series(data = list(list(x = pooled_index, low = pooled_PI[1], high = pooled_PI[2])),
                  type = "errorbar", color = "black", stemWidth = 2, whiskerLength = 1, name = "Pooled Prediction Interval")

  return(hc_plot)
}

# Example usage:
dat_example <- data.frame(
  study = paste("Study", 1:10),
  yi = rnorm(10, 0, 0.5),
  vi = runif(10, 0.01, 0.1)
)

hc_plot <- highchart_forest(dat_example)

# Save and open plot
saveWidget(hc_plot, "testhighcharts.html", selfcontained = TRUE)
browseURL("testhighcharts.html")
