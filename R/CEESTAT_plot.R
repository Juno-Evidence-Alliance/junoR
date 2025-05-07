#' Plot a horizontal stacked bar chart for review assessments
#'
#' This function creates a horizontal stacked bar chart (like the one used in systematic review. It is designed to visualize categorical
#' appraisal CEESTAT results (e.g., Gold, Green, Amber, Red, N/A) across multiple review criteria.
#'
#' @param df A data frame with one column `Question` (character) and additional columns for each
#'   appraisal category (e.g., `Gold`, `Green`, `Amber`, `Red`, `NA_`), all numeric.
#' @param palette A named vector of colors for each appraisal category. Defaults to a standard
#'   color set used in systematic review plots.
#'
#' @return A ggplot object representing the horizontal stacked bar chart.
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual theme_minimal labs theme element_text
#' @importFrom tidyr pivot_longer
#' @examples
#' review_data <- data.frame(
#'   Question = c(
#'     "Is the review question clear?",
#'     "Is there a protocol?",
#'     "Is the search clearly defined?",
#'     "Is the search comprehensive?",
#'     "Are eligibility criteria clearly defined?",
#'     "Is eligibility criteria consistently applied?",
#'     "Are eligibility decisions transparently reported?",
#'     "Was each study critically appraised?",
#'     "During critical appraisal was effort made to minimize subjectivity?",
#'     "Is the method of data extraction fully documented?",
#'     "Are the extracted data reported for each study?",
#'     "Were extracted data cross checked by more than one reviewer?",
#'     "Is the choice of synthesis approach appropriate?",
#'     "Is a statistical estimate of pooled effect (or similar) provided together with a measure of precision?",
#'     "Is variability in the study findings investigated and discussed?",
#'     "Have limitations of the synthesis been considered?"
#'   ),
#'   Gold = c(50, 30, 35, 34, 42, 40, 39, 32, 33, 37, 33, 32, 34, 20, 25, 28),
#'   Green = c(8, 10, 8, 9, 9, 6, 6, 5, 1, 10, 13, 10, 12, 5, 12, 10),
#'   Amber = c(0, 10, 9, 9, 3, 4, 5, 5, 1, 2, 3, 1, 6, 3, 5, 8),
#'   Red = c(0, 5, 5, 4, 1, 4, 4, 7, 14, 3, 1, 10, 3, 1, 2, 5),
#'   NA_ = c(0, 0, 0, 2, 0, 1, 2, 1, 9, 1, 2, 5, 0, 10, 10, 4)
#' )
#'
#' plot_CEESTAT(review_data)
#' @export
plot_CEESTAT <- function(df,
                                palette = c(
                                  Gold = "#b28800",
                                  Green = "#00a651",
                                  Amber = "#f7931e",
                                  Red = "#c1272d",
                                  NA_ = "#fbe8b5"
                                )) {

  # Check required structure
  if (!"Question" %in% colnames(df)) stop("The data frame must contain a 'Question' column.")

  # Pivot to long format
  df_long <- tidyr::pivot_longer(df, cols = -Question, names_to = "Rating", values_to = "Count")

  # Ensure factor levels for plot order
  df_long$Rating <- factor(df_long$Rating, levels = names(palette))
  df_long$Question <- factor(df_long$Question, levels = rev(df$Question))

  # Plot
  ggplot2::ggplot(df_long, ggplot2::aes(x = Count, y = Question, fill = Rating)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = palette, name = "") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(size = 10),
      legend.position = "bottom"
    )
}
