#' Plot Temporal Dimensions of a Corpus
#'
#' This function creates a multi-layered visualization showing four corpus metrics over time,
#' displayed as vertical bars (using \code{geom_linerange}). The four metrics are:
#' \itemize{
#'   \item Number of documents (\code{nDoc}): shown as a thin line with points
#'   \item Number of word-tokens (\code{dimCorpus}): widest bar
#'   \item Sum of keyword frequencies in the TDM (\code{Csum}): medium-width bar
#'   \item Maximum keyword frequency per year (\code{Mcf}): narrow bar
#' }
#'
#' All metrics are rescaled by user-defined factors (\code{sc}) to ensure visual comparability.
#' For example, if \code{sc = c(1, 10, 10, 1)}, then \code{dimCorpus} and \code{Csum} are
#' divided by 10 before plotting.
#'
#' @param data A list object returned by \code{importData()}, containing:
#'   \itemize{
#'     \item \code{corpus_info}: a tibble with columns \code{years}, \code{nDoc}, and \code{dimCorpus}
#'     \item \code{tdm}: a term-document matrix used to compute \code{Csum} and \code{Mcf}
#'   }
#' @param sc Numeric vector of length 4. Scaling factors for \code{nDoc}, \code{dimCorpus},
#'   \code{Csum}, and \code{Mcf}, respectively. Default: \code{c(1, 10, 10, 1)}.
#' @param r Integer. Interval for thinning x-axis year labels. If \code{r = 3}, only every
#'   third year is labeled. Default: \code{1} (show all years).
#' @param textty Character. Unit of analysis used in legend labels (e.g., "text", "article",
#'   "abstract"). Default: \code{"text"}.
#' @param themety Character. Plot theme: \code{"light"} (default) or \code{"dark"}.
#' @param size_b Numeric. Base line width multiplier for the bars. Default: \code{2.5}.
#' @param x_lab Character. Label for the x-axis. Default: \code{"year"}.
#'
#' @return A \code{ggplot2} object showing the temporal evolution of corpus dimensions.
#'   The y-axis represents rescaled corpus size, and the x-axis shows time points (years).
#'   A horizontal legend at the bottom identifies each metric.
#'
#' @details
#' The function computes \code{Csum} (column-wise sum of keyword frequencies) and
#' \code{Mcf} (column-wise maximum frequency) directly from the TDM. The bars are layered
#' from widest to narrowest for visual clarity, with \code{nDoc} shown as a dot-and-line
#' overlay.
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
#' # Basic usage
#' colMassPlot(data)
#'
#' # Custom scaling and labeling
#' colMassPlot(data,
#'   sc = c(1, 50, 50, 1),
#'   r = 2,
#'   textty = "article",
#'   themety = "dark",
#'   size_b = 2,
#'   x_lab = "Year"
#' )
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

  # Calculate Csum and Mcf (column-wise sum and max)
  Csum <- tdm %>%
    select(all_of(year_cols)) %>%
    summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) %>%
    as.numeric()

  Mcf <- tdm %>%
    select(all_of(year_cols)) %>%
    summarise(across(everything(), \(x) max(x, na.rm = TRUE))) %>%
    as.numeric()

  corpus <- bind_cols(corpus, data.frame(Csum = Csum, Mcf = Mcf))

  # x-axis labels (thinned by r)
  x_labels <- corpus$years
  x_labels[-seq(1, length(x_labels), by = r)] <- ""

  # Color scheme
  colors <- if (themety == "light") {
    c("nDoc" = "black", "dimCorpus" = "lightgoldenrod3",
      "Csum" = "aquamarine3", "Mcf" = "indianred4")
  } else {
    c("nDoc" = "white", "dimCorpus" = "lightgoldenrod3",
      "Csum" = "aquamarine3", "Mcf" = "indianred4")
  }

  base_theme <- if (themety == "light") theme_classic() else theme_dark()

  plot_theme <- base_theme +
    theme(
      plot.margin = unit(c(0.5, 0.1, 0.5, 0.1), "lines"),
      axis.text = element_text(size = rel(0.875)),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = rel(0.85)),
      axis.title.x = element_text(margin = margin(t = 4)),
      axis.text.y = element_text(hjust = 0.5, vjust = 0.25),
      axis.ticks.length = unit(0.1, "cm"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = rel(0.85), face = "bold", color = col_leg),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.key.width = unit(0.8, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.spacing.x = unit(0.3, "cm"),
      legend.margin = margin(t = 5, b = 0),
      legend.background = element_rect(fill = NA),
      panel.grid = element_blank()
    )

  g <- ggplot(corpus, aes(x = years, y = nDoc)) +
    geom_linerange(aes(ymin = 0, ymax = dimCorpus / sc[2], colour = "dimCorpus"),
                   linewidth = size_b * 1.5) +
    geom_linerange(aes(ymin = 0, ymax = Csum / sc[3], colour = "Csum"),
                   linewidth = size_b * 1.25) +
    geom_linerange(aes(ymin = 0, ymax = Mcf / sc[4], colour = "Mcf"),
                   linewidth = size_b) +
    geom_linerange(aes(ymin = 0, ymax = nDoc / sc[1], colour = "nDoc"),
                   linewidth = size_b / 5) +
    geom_point(aes(y = nDoc / sc[1], colour = "nDoc"), size = size_b / 5) +
    scale_colour_manual(
      "",
      values = colors,
      breaks = c("nDoc", "dimCorpus", "Csum", "Mcf"),
      labels = c(
        paste0("Number of ", textty, "s"),
        paste0("Number of word-tokens (/", sc[2], ") in ", textty, "s"),
        paste0("Sum of keyword frequencies (/", sc[3], ")"),
        paste0("Max keyword frequency (/", sc[4], ")")
      )
    ) +
    scale_y_continuous(expand = c(0.01, 0)) +
    scale_x_continuous(expand = c(0, 0.5), breaks = corpus$years, labels = x_labels) +
    labs(x = x_lab, y = "Corpus Size (rescaled)") +
    guides(colour = guide_legend(
      nrow = 2,
      byrow = FALSE,
      override.aes = list(linewidth = c(1, 2.5, 2, 1.5))
    )) +
    plot_theme

  return(g)
}
