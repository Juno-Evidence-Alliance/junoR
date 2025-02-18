#' Juno colour palette
#'
#' This function returns a custom coluor palette for plots.
#'
#' @return A vector of hex color codes
#' @examples
#' data <- data.frame(
#' x = rep(1:5, each = 3),
#' y = rnorm(15),
#' group = rep(letters[1:5], times = 3)
#' )
#' library(tidyverse)
#' ggplot(data, aes(x = x, y = y, color = group)) +
#' geom_point(size = 4) +
#' scale_color_manual(values = juno_palette()) +
#' theme_minimal()

#' @export
juno_palette <- function() {
  c("#7BC143", "#418045", "#FBA12A", "#F07912", "#5B5C5E")
}

