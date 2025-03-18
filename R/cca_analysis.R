#' Calculate Corrected Covered Area (CCA) and Generate a Heatmap
#'
#' This function calculates the Corrected Covered Area (CCA) for a set of studies
#' across systematic reviews and generates a heatmap of study inclusion.
#'
#' @param data A tibble or dataframe with columns:
#'   - `study` (character): Study identifiers.
#'   - `review` (character): Review identifiers.
#'   - `status` (character): Study status, one of `c("included", "excluded", "ineligible", NA)`.
#' @param show_plot Logical; if `TRUE`, returns a heatmap plot, otherwise only the CCA value.
#' @return A list with:
#'   - `CCA` (numeric): The Corrected Covered Area value.
#'   - `plot` (ggplot object): The heatmap (if `show_plot = TRUE`).
#' @references
#' Pieper, D., Antoine, S. L., Mathes, T., Neugebauer, E. A., & Eikermann, M. (2014).
#' Systematic review finds overlapping reviews were not mentioned in every other overview.
#' \emph{Journal of Clinical Epidemiology}, 67(4), 368–375. \doi{10.1016/j.jclinepi.2013.11.007}
#'
#' Hedges, L. V., & Olkin, I. (1988). \emph{Statistical Methods for Meta-Analysis}. Academic Press.
#'
#' @examples
#' library(tidyverse)
#' library(junoR)
#' # Example dataset
#' data <- tibble(
#'   study = rep(paste0("Study_", 1:5), times = 5),
#'   review = rep(paste0("Review_", 1:5), each = 5),
#'   status = c("included", "included", "ineligible", NA, NA,
#'              NA, "included", NA, "excluded", NA,
#'              "included", NA, "included", "excluded", "ineligible",
#'              "included", "excluded", "included", "excluded", NA,
#'              NA, "included", "ineligible", NA, "excluded")
#' )
#'
#' # Run function
#' result <- cca_heatmap(data)
#' print(result$CCA)
#' print(result$plot)
#'
#' @export
cca_heatmap <- function(data, show_plot = TRUE) {

  library(tidyverse)

  # **Calculate CCA**
  N <- data %>% filter(status == "included") %>% nrow()  # Count only included occurrences
  r <- data %>% filter(status == "included") %>% distinct(study) %>% nrow()  # Unique included studies
  c <- data %>% distinct(review) %>% nrow()  # Number of reviews

  # **Prevent division by zero**
  if (r == 0 | c <= 1) {
    stop("CCA calculation error: Ensure there are at least 1 included study and 2 reviews.")
  }

  CCA <- ((N - r) / (r * (c - 1))) * 100  # CCA formula

  # **Define colour mapping**
  status_colors <- c(
    "included" = juno_palette()[1],       # Included in review
    "excluded" = juno_palette()[5],        # Excluded from review
    "ineligible" = juno_palette()[4],# Ineligible due to publication dates
    "NA" = "gray90"            # Not assessed (NA values)
  )

  # **Generate heatmap**
  heatmap_plot <- data %>%
    mutate(status = factor(status, levels = c("included", "excluded", "ineligible", NA))) %>%
    ggplot(aes(x = review, y = study, fill = status)) +
    geom_tile(colour = "white") +
    scale_fill_manual(values = status_colors, na.value = "gray90") +
    theme_minimal() +
    labs(
      title = "Study Inclusion Across Reviews",
      fill = "Study Status",
      x = "Systematic Reviews",
      y = "Primary Studies"
    ) +
    theme(legend.position = "bottom") +
    annotate("text", x = 3, y = 6, label = paste0("CCA: ", round(CCA, 2), "%"),
             size = 6, fontface = "bold", color = "black") +
    coord_cartesian(ylim = c(0.5, max(length(unique(data$study))) + 1))  # Adjust space for CCA text

  # **Return results**
  return(list(CCA = CCA, plot = if (show_plot) heatmap_plot else NULL))
}
