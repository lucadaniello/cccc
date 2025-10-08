#' Plot Temporal Dimensions of a Corpus
#'
#' This function creates a multi-line plot of four corpus metrics over time:
#' - number of documents (\code{nDoc})
#' - number of tokens (\code{dimCorpus})
#' - sum of keyword frequencies (\code{Csum})
#' - maximum keyword frequency (\code{Mcf}).
#'
#' All measures are rescaled by user-defined factors to improve visual comparability.
#'
#' @param data A list containing the output of `importData()`
#' @param sc Numeric vector of length 4. Scaling factors for:
#'   \code{nDoc}, \code{dimCorpus}, \code{Csum}, \code{Mcf}. Default: \code{c(1, 10, 10, 1)}.
#' @param r Integer. Interval for thinning x-axis labels (default = 1 = show all).
#' @param textty Character. Unit of analysis for legend (e.g. "text", "place", "paper").
#' @param themety Character. Either \code{"light"} (default) or \code{"dark"}.
#' @param size_b Numeric. Base size for bar lines (default = 2.5).
#' @param x_lab Character. X-axis label (default = "year").
#'
#' @return A \code{ggplot2} object representing temporal trends in corpus structure.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tdm <- system.file("extdata", "tdm.csv", package = "cccc")
#' corpus <- system.file("extdata", "corpus.csv", package = "cccc")
#' data <- importData(tdm_file = tdm, corpus_file = corpus,
#' sep_tdm = ";",sep_corpus_info = ";",zone="stat")
#'
#' colMassPlot(data,
#' sc = c(1, 10, 10, 1),
#' r = 1,
#' textty = "text",
#' themety = "light",
#' size_b = 2.5,
#' x_lab = "year"
#' )
#'
#' }
colMassPlot <- function(data,
                        sc = c(1, 10, 10, 1),
                        r = 1,
                        textty = "text",
                        themety = "light",
                        size_b = 2.5,
                        x_lab = "year") {

  corpus <- data$corpus_info
  tdm <- data$tdm

  col_leg <- ifelse(themety == "light", "black", "white")

  year_cols <- grep("\\d", names(tdm), value = TRUE)

  # Csum and Mcf
  Csum <- tdm %>%
    select(all_of(year_cols)) %>%
    summarise(across(everything(), sum), .groups = "drop") %>% as.numeric()

  Mcf <- tdm %>%
    select(all_of(year_cols)) %>%
    summarise(across(everything(), max), .groups = "drop") %>% as.numeric()

  corpus <- bind_cols(corpus, data.frame(Csum = Csum, Mcf = Mcf))

  # x-axis labels
  x_labels <- corpus$years
  x_labels[-seq(1, length(x_labels), by = r)] <- ""

  colors <- if (themety == "light") {
    c("nDoc" = "black", "dimCorpus" = "lightgoldenrod3", "Csum" = "aquamarine3", "Mcf" = "indianred4")
  } else {
    c("nDoc" = "white", "dimCorpus" = "lightgoldenrod3", "Csum" = "aquamarine3", "Mcf" = "indianred4")
  }

  base_theme <- if (themety == "light") theme_classic() else theme_dark()

  plot_theme <- base_theme +
    theme(
      plot.margin = unit(c(.5,0.1,.25,.1), "line"),
      axis.text = element_text(size = rel(0.875)),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = rel(0.85)),
      axis.title.x = element_text(margin = margin(t = 4)),
      axis.text.y = element_text(hjust = 0.5, vjust = 0.25),
      axis.ticks.length = unit(0.1, "cm"),
      legend.text = element_text(size = rel(0.95), face = "bold",
                                 color = col_leg, margin = margin(l = 0.25)),
      legend.background = element_rect(fill = NA),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.key.width = unit(0.001, "in"),
      legend.key.spacing.x = unit(0.25, "in"),
      legend.key.spacing.y = unit(0.01, "in"),
      legend.margin = margin(t = 0.0, b = 0.0),
      legend.title = element_blank(),
      panel.grid = element_blank()
    )

  g <- ggplot(corpus, aes(x = years, y = nDoc)) +
    geom_linerange(aes(ymin = 0, ymax = dimCorpus / sc[2], colour = "dimCorpus"), linewidth = size_b * 1.5) +
    geom_linerange(aes(ymin = 0, ymax = Csum / sc[3],    colour = "Csum"), linewidth = size_b * 1.25) +
    geom_linerange(aes(ymin = 0, ymax = Mcf / sc[4],     colour = "Mcf"), linewidth = size_b) +
    geom_linerange(aes(ymin = 0, ymax = nDoc / sc[1],    colour = "nDoc"), linewidth = size_b / 5) +
    geom_point(aes(y = nDoc / sc[1], colour = "nDoc"), size = size_b / 5) +
    scale_colour_manual(
      "",
      values = colors,
      breaks = c("nDoc", "dimCorpus", "Csum", "Mcf"),
      labels = c(
        paste0("Number of ", textty, "s"),
        paste0("Number of word-tokens (/", sc[2], ") in ", textty, "s"),
        paste0("Sum of keyword frequencies (/", sc[3], ")"),
        paste0("Max keyword frequency (/", sc[4], ")"))
      ) +
    scale_y_continuous(expand = c(0.01, 0)) +
    scale_x_continuous(expand = c(0, 0.5), breaks = corpus$years, labels = x_labels) +
    labs(x = x_lab, y = "Corpus Size (rescaled)") +
    guides(colour=guide_legend(nrow=2, byrow=FALSE)) +
    plot_theme

  return(g)
}

