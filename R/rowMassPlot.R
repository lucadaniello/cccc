#' Plot Row Masses by Frequency Zone
#'
#' This function creates a horizontal bar plot showing the total frequency of each keyword,
#' ordered from lowest to highest frequency. Each bar is colored according to its assigned
#' frequency zone (e.g., VH, H, L, VL for statistical zones or high, medium, low for linguistic zones).
#' The legend displays both the zone label and its corresponding frequency interval.
#'
#' @param data A list object returned by \code{importData()}, which must contain:
#'   \itemize{
#'     \item \code{tdm}: a tibble with columns \code{keyword}, \code{tot_freq}, \code{zone}, and \code{int_freq}
#'     \item \code{colors_light}: a character vector of colors for the frequency zones
#'   }
#'
#' @return A \code{ggplot2} object displaying keyword frequencies as a bar plot.
#'   The x-axis represents keywords (unlabeled for clarity), and the y-axis shows total frequency.
#'   Bars are colored by frequency zone, with a horizontal legend at the bottom.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tdm <- system.file("extdata", "tdm.csv", package = "cccc")
#' corpus <- system.file("extdata", "corpus.csv", package = "cccc")
#' data <- importData(tdm_file = tdm, corpus_file = corpus,
#'   sep_tdm = ";", sep_corpus_info = ";", zone = "stat")
#'
#' # Generate the plot
#' rowMassPlot(data)
#' }

rowMassPlot <- function(data) {

  tdm <- data$tdm %>%
    mutate(
      zone_label = paste0(zone, " ", int_freq),
      zone_label = factor(zone_label, levels = unique(zone_label[order(zone)]))
    )

  g <- ggplot2::ggplot(tdm, ggplot2::aes(x = reorder(keyword, tot_freq), y = tot_freq, fill = zone_label)) +
    ggplot2::geom_bar(stat = "identity", show.legend = TRUE) +
    ggplot2::scale_fill_manual(
      values = setNames(data$colors_light, levels(tdm$zone_label))
    ) +
    ggplot2::labs(
      x = "Keyword",
      y = "Total Frequency",
      fill = "Frequency zone - frequency interval"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.direction = "horizontal",
      legend.title.position = "top",
      legend.title = ggplot2::element_text(size = rel(0.85), hjust = 0.5, face = "bold"),
      legend.text = ggplot2::element_text(size = rel(0.75), margin = margin(l = 2, r = 2)),
      legend.key.size = unit(0.4, "cm"),
      legend.spacing.x = unit(0.2, "cm"),
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "lines")
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      nrow = 1,
      reverse = TRUE,
      title.position = "top",
      title.hjust = 0.5
    ))

  return(g)
}

