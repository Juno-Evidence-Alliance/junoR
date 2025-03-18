#' This function is under development and is not yet exported for package users.
#'
#' @keywords internal
#' @importFrom highcharter highchart
#' @importFrom highcharter hc_chart
#' @importFrom highcharter hc_xAxis
#' @importFrom highcharter hc_yAxis
#' @importFrom highcharter hc_add_series
#' @importFrom highcharter list_parse
#'
#
# highchart_forest<-function(){
#
# ## This is starting to work!!!!
#
# library(dplyr)
# library(highcharter)
# library(metafor)
# library(htmlwidgets)
#
# # Generate random example data
# dat <- data.frame(
#   study = paste("Study", 1:10),  # Study names
#   yi = rnorm(10, 0, 0.5),        # Effect sizes
#   vi = runif(10, 0.01, 0.1)      # Variance
# )
#
# # Fit a random-effects model
# res <- rma(yi, vi, data = dat, method = "REML", digits = 3, level = 95, test = "knha")
# print(res)
#
# # Add results to plot_df
# # Compute study-specific prediction intervals
# plot_df <- data.frame(
#   study = dat$study,
#   effect = dat$yi,
#   lower_CI = dat$yi - 1.96 * sqrt(dat$vi),  # 95% Confidence Interval
#   upper_CI = dat$yi + 1.96 * sqrt(dat$vi),
#   lower_PI = dat$yi - 1.96 * sqrt(res$tau2 + dat$vi),  # 95% Prediction Interval
#   upper_PI = dat$yi + 1.96 * sqrt(res$tau2 + dat$vi)
# )
#
# # Add pooled effect and its intervals
# pooled_effect <- res$b[[1]]
# pooled_CI <- c(res$ci.lb, res$ci.ub)
# pooled_PI <- c(res$b- 1.96 * sqrt(res$tau2 + res$se), res$b+ 1.96 * sqrt(res$tau2 + res$se))
#
# pooled_df<-data.frame(study = "Pooled Effect",effect = pooled_effect,lower_CI = pooled_CI[1],upper_CI = pooled_CI[2],
#                       lower_PI =pooled_PI[1],
#                       upper_PI =pooled_PI[2])
#
# plot_df <- bind_rows(plot_df,pooled_df)
#
#
# # Flip the axes: Use studies as x-axis categories, effect sizes on y-axis
# hc_plot <- highchart() |>
#   hc_chart(inverted = TRUE) |>
#   hc_xAxis(categories = plot_df$study, title = list(text = "Study")) |>  #rev(plot_df$study)
#   hc_yAxis(title = list(text = "Effect Size")) |>
#
#   # Prediction Intervals (gray error bars)
#   hc_add_series(data = list_parse(mutate(plot_df, low = lower_PI, high = upper_PI)),
#                 type = "errorbar", color = "grey", stemWidth = 1, whiskerLength = 1, name = "Prediction Interval") |>
#
#   # Confidence Intervals (red error bars)
#   hc_add_series(data = list_parse(mutate(plot_df, low = lower_CI, high = upper_CI)),
#                 type = "errorbar", color = "red", stemWidth = 1, whiskerLength = 1, name = "Confidence Interval") |>
#
#   # Effect Sizes (red points)
#   hc_add_series(data = plot_df$effect, color = "red", type = "scatter", name = "Effect Size") |>
#
#   # Pooled Effect (blue diamond)
#   hc_add_series(data = list(list(x = 11, y = pooled_effect)),
#                 type = "scatter", color = "blue", marker = list(symbol = "diamond", radius = 8), name = "Pooled Effect") |>
#
#   # Pooled Prediction Interval (black error bars)
#   hc_add_series(data = list(list(x = 11, low = pooled_PI[1], high = pooled_PI[2])),
#                 type = "errorbar", color = "black", stemWidth = 2, whiskerLength = 1, name = "Pooled Prediction Interval")
#
# # Save and open plot
# #saveWidget(hc_plot, "testhighcharts.html", selfcontained = TRUE)
# #browseURL("testhighcharts.html")
# }
