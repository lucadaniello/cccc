#' Plot Temporal Curves of Keyword Frequencies
#'
#' This function generates a temporal plot of keyword frequencies (normalized or raw)
#' from a TDM in long format. It allows visualizing curves grouped by frequency zone,
#' with thematic customization options.
#'
#' @param data A list containing the output of `importData()`
#' @param r Scalar. Interval for thinning x-axis year labels (default = 1).
#' @param themety Character. Theme type: "light" (default) or "dark".
#' @param size_class Numeric vector for line sizes by class (default is context-dependent).
#' @param x_leg Numeric. x-position of legend (default = 0.85).
#' @param x_lab Character. Label for x-axis (default = "year").
#'
#' @examples
#' \donttest{
#' data <- importData("tdm.csv", "corpus.csv", sep_tdm = ";", sep_corpus_info = ",")
#' curvePlot(data)
#' }
#'
#' @export
curvePlot <- function(data,
                      r = 1,
                      themety = "light",
                      size_class = NULL,
                      x_leg = 0.85,
                      x_lab = "years") {

  norm <- data$norm
  y_lab <- ifelse(norm, "keyword (normalized) frequency", "keyword frequency")
  col_leg <- ifelse(themety == "light", "black", "white")

  dat_l <- data$tdm_long
  year_cols <- data$year_cols

  dat_l <- dat_l %>%
    mutate(zone = factor(zone, levels = unique(zone)))

  if (themety == "light") {
    base_theme <- theme_classic()
    col_class <- setNames(data$colors_light, levels(dat_l$zone))
    if (is.null(size_class)) size_class <- c(0.25, 0.35, 0.45, 0.55)
  } else {
    base_theme <- theme_dark()
    col_class <- setNames(data$colors_dark, levels(dat_l$zone))
    if (is.null(size_class)) size_class <- c(0.4, 0.5, 0.6, 0.7)
  }


    year_vec <- data$corpus_info$years
  n_y <- length(year_vec)
  xaxlab <- year_vec
  xaxlab[-seq(1, n_y, by = r)] <- ""


  opts <- base_theme +
    theme(
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "lines"),
      axis.text = element_text(angle = 90, size = rel(0.9)),
      axis.text.x = element_text(vjust = 0.5),
      axis.text.y = element_text(hjust = 0.5),
      axis.ticks.length = unit(0.1, "cm"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.background = element_rect(fill = NA),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.text = element_text(color = col_leg, face = "bold", size = rel(1)),
      legend.title = element_text(color = col_leg, face = "bold", size = rel(1)),
      panel.grid = element_blank()
    )


  p <- ggplot(dat_l, aes(x = chrono, y = freq, group = keyword)) +
    geom_line(aes(colour = zone, size = zone)) +
    scale_x_continuous(expand = c(0.0065, 0), breaks = 1:n_y, labels = xaxlab) +
    scale_y_continuous(expand = c(0.01, 0)) +
    xlab(x_lab) +
    ylab(y_lab) +
    scale_colour_manual(
      name = "Frequency Zone",
      values = col_class,
      guide = guide_legend(reverse = TRUE, keywidth = 2)
    ) +
    scale_size_manual(
      values = setNames(size_class, levels(dat_l$zone)),
      guide = "none"
    ) +
    opts

  return(p)
}


