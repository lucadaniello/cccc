#' Plot Row Masses by Frequency Zone
#'
#' This function creates a bar plot of keywords ordered by their total frequency
#' and colored by their assigned frequency zone, combining both the zone
#' and its frequency interval label.
#'
#' @param data A list contained the output of `importData()`
#'
#' @return A `ggplot2` object showing the distribution of total frequencies by zone.
#' The bars are colored by zone and frequency interval.
#'
#' @export
#'
#' @examples
#' \donttest{
#' data <- importData("JPSP_TITfreq.csv", "JPSP_TITinfo.csv", sep_tdm = ";", sep_corpus_info = ";")
#' rowMassPlot(tdm = data$tdm)
#' }
#'

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
      x = "Keywords",
      y = "Total Frequency",
      fill = "Keyword Zone [Frequency Interval]"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))

  return(g)
}


