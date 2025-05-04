#' Plot of Temporal Curves for Frequency Zones and Example Keywords
#'
#' This function generates a line plot of keyword frequency over time,
#' distinguishing between predefined frequency zones (high, medium, low)
#' and highlighting three example keywords with custom colors.
#' It supports both normalized and raw frequencies, and includes
#' customizable themes, axis labeling, and adaptive thinning of the x-axis labels.
#'
#' The plot uses two separate color scales: one for
#' the frequency zones and one for the example keywords. Each zone is
#' labeled with its frequency interval (e.g., "High [201–500]"),
#' and the example keywords are displayed with a distinct legend.
#'
#' @param data A list containing the output of `importData()`
#' @param ctu_noun Character vector of length 3. Specifies exactly three example keywords to highlight in the plot. Each keyword must be present in a different frequency zone (high, medium, low).
#' @param r Integer. Thinning rate for the x-axis labels; only one label every \code{r} years will be shown.
#' @param themety Character. Theme type, either \code{"light"} (default) or \code{"dark"}. This affects both plot aesthetics and color palettes.
#' @param size_class Optional numeric vector of length 3. Defines the line widths for the three zones (high, medium, low), respectively. If \code{NULL}, defaults are used based on the theme.
#' @param x_lab Character. Label for the x-axis. Defaults to \code{"year"}.
#'
#' @return A \code{ggplot} object displaying keyword frequency trajectories over time,
#' with frequency zones visually distinguished by color and line thickness, and
#' the three specified keywords highlighted using a separate color legend.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- importData("tdm.csv", "corpus.csv", sep_tdm = ";", sep_corpus_info = ",", zone="stat")
#' curveCtuPlot(data,
#'                 ctu_noun = c("person", "object", "instruct", "incident"),
#'                 r = 1,
#'                 themety = "light",
#'                 size_class = NULL,
#'                 x_lab = "year")
#' }

curveCtuPlot <- function(data,
                         ctu_noun = NULL,
                         r = 1,
                         themety = "light",
                         size_class = NULL,
                         x_lab = "year") {

  norm <- data$norm
  col_leg <- ifelse(themety == "light", "black", "white")
  y_lab <- ifelse(norm, "keyword (normalized) frequency", "keyword frequency")

  tdm <- data$tdm
  dat_l <- data$tdm_long
  year_cols <- data$year_cols

  zone_levels <- levels(data$zone)
  n_zones <- length(zone_levels)

  if (length(ctu_noun) != n_zones) {
    stop(paste("Please provide exactly", n_zones, "keywords: one for each zone."))
  }

  if (themety=="light"){
    col_class <- setNames(data$colors_light, zone_levels)
    base_theme <- theme_classic()
  } else {
    col_class <- setNames(data$colors_dark, zone_levels)
    base_theme <- theme_dark()
  }
  col_class <- paste0(col_class, "70") # alpha
  col_kw <- colorlist(type=themety)[n_zones + seq(1, n_zones)]
  names(col_kw) <- ctu_noun

  if (is.null(size_class)) {
    size_class <- if (themety == "light") rep(0.3, n_zones) else rep(0.5, n_zones)
  }
  names(size_class) <- zone_levels

  year_vec <- sort(unique(dat_l$year))
  n_y <- length(year_vec)
  xaxlab <- year_vec
  xaxlab[-seq(1, n_y, by = r)] <- ""

  zone_labels <- dat_l %>%
    distinct(zone, int_freq) %>%
    arrange(factor(zone, levels = zone_levels)) %>%
    transmute(label = paste(zone, int_freq)) %>%
    pull(label)

  example_df <- purrr::map_dfr(ctu_noun, function(k) {
    df <- dat_l %>% filter(keyword == k)
    df$example <- k
    df
  }) %>%
    mutate(example = factor(example, levels = ctu_noun))

  opts <- base_theme +
    theme(
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "lines"),
      axis.text = element_text(angle = 90, size = rel(0.9)),
      axis.text.x = element_text(vjust = 0.5),
      axis.text.y = element_text(hjust = 0.5),
      axis.ticks.length = unit(0.1, "cm"),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.box.spacing = unit(0.3, "lines"),
      legend.background = element_rect(fill = NA),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.text = element_text(face = "bold", size = rel(0.85), color = col_leg),
      legend.title = element_text(face = "bold", size = rel(0.9), color = col_leg),
      panel.grid = element_blank(),
      plot.caption = element_text(hjust = 0.5, face = "italic", size = rel(0.9), color = col_leg),
      plot.caption.position = "plot"
    )

  p <- ggplot(dat_l, aes(x = chrono, y = freq, group = keyword)) +
    geom_line(aes(colour = zone, linewidth = zone)) +
    scale_colour_manual(
      name = "Frequency Zone",
      values = col_class,
      breaks = zone_levels,
      labels = zone_labels,
      guide = guide_legend(order = 1, override.aes = list(size = 2))
    ) +
    scale_linewidth_manual(
      values = size_class,
      guide = "none"
    ) +

    ggnewscale::new_scale_colour() +
    geom_line(data = example_df,
              aes(x = chrono, y = freq, group = example, colour = example),
              linewidth = 1) +
    scale_colour_manual(
      name = "Example Keywords",
      values = col_kw,
      guide = guide_legend(order = 2, override.aes = list(size = 1))
    ) +

    scale_x_continuous(expand = c(0.0065, 0), breaks = 1:n_y, labels = xaxlab) +
    scale_y_continuous(expand = c(0.01, 0)) +
    labs(x = x_lab, y = y_lab) +
    opts

  return(p)
}
