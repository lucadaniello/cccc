#' Plot of Temporal Curves for Frequency Zones and Example Keywords
#'
#' This function generates a line plot of keyword frequencies over time,
#' distinguishing between predefined frequency zones
#' and highlighting, for each zone, an example keyword with custom colors.
#' It supports both normalized and raw frequencies, and includes
#' customizable themes, axis labeling, and adaptive thinning of the x-axis labels.
#'
#' The plot uses two separate color scales: one for
#' the frequency zones and one for the example keywords. Each zone is
#' labeled with its frequency interval,
#' and the example keywords are displayed with a distinct legend.
#'
#' @param data A list containing the output of `importData()`
#' @param ctu_noun Character vector of length three or four. Specifies exactly three example keywords to highlight in the plot. Each keyword must be present in a different frequency zone (high, medium, low).
#' @param r Integer. Thinning rate for the x-axis labels; only one label every \code{r} years will be shown.
#' @param themety Character. Theme type, either \code{"light"} (default) or \code{"dark"}. This affects both plot aesthetics and color palettes.
#' @param size_class Numeric vector specifying line widths for each frequency zone. If \code{NULL}, defaults are used based on the theme.
#'   Must have length equal to the number of zones. If \code{NULL}, defaults to:
#'   \itemize{
#'     \item light theme: c(0.2, 0.3, 0.35, 0.35)
#'     \item dark theme: c(0.35, 0.5, 0.5, 0.5)
#'   }
#' @param size_example Numeric vector specifying line widths for each example keyword.
#'    Must have length equal to the number of zones. If \code{NULL}, defaults to c(1.2, 1.05, 0.9, 0.7), creating a visual hierarchy.
#' @param size_example Optional numeric vector of length three or four. Defines the line widths for the three or four keyword examples, respectively. If \code{NULL}, defaults are used.
#' @param x_lab Character. Label for the x-axis. Defaults to \code{"year"}.
#' @param y_lab Character. Label for the y-axis. Default: automatically set based on
#'   \code{data$norm} (\code{"keyword (normalized) frequency"} if normalized,
#'   \code{"keyword frequency"} otherwise). Can be customized by the user.
#'
#' @return A \code{ggplot} object with two legends:
#'   \itemize{
#'     \item Top legend: "Frequency Zone" showing zone colors and intervals
#'     \item Bottom legend: "Example Keywords" showing highlighted keywords
#'   }
#'
#' @details
#'
#' Zone lines are displayed with 80\% opacity (alpha = "80") to create a subtle
#' background layer, while example keywords are drawn with full opacity and variable
#' line widths (controlled by \code{size_example}) for emphasis.
#'
#' The function validates that:
#' \itemize{
#'   \item The number of keywords in \code{ctu_noun} matches the number of zones
#'   \item All specified keywords exist in the dataset
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tdm <- system.file("extdata", "tdm.csv", package = "cccc")
#' corpus <- system.file("extdata", "corpus.csv", package = "cccc")
#' data <- importData(tdm_file = tdm, corpus_file = corpus,
#' sep_tdm = ";",sep_corpus_info = ";",zone="stat")
#
#' # With statistical zones (4 zones)
#' curveCtuPlot(data,
#'   ctu_noun = c("person", "object", "instruct", "incident"),
#'   r = 2,
#'   themety = "light"
#' )
#'
#' # With custom line widths for examples
#' curveCtuPlot(data,
#'   ctu_noun = c("person", "object", "instruct", "incident"),
#'   size_example = c(1.5, 1.2, 1.0, 0.8)
#' )
#'
#' # With linguistic zones (3 zones)
#' data_ling <- importData(tdm_file = tdm, corpus_file = corpus, zone = "ling")
#' curveCtuPlot(data_ling,
#'   ctu_noun = c("frequent_word", "medium_word", "rare_word"),
#'   r = 1
#' )
#'
#' # Custom y-axis label
#' curveCtuPlot(data,
#'   ctu_noun = c("person", "object", "instruct", "incident"),
#'   y_lab = "Normalized frequency (per 1000 tokens)"
#' )
#' }
#'

curveCtuPlot <- function(data,
                         ctu_noun = NULL,
                         r = 1,
                         themety = "light",
                         size_class = NULL,
                         size_example = NULL,
                         x_lab = "year",
                         y_lab = NULL) {

  # Input validation
  if (is.null(ctu_noun)) {
    stop("Argument 'ctu_noun' is required. Provide one keyword per frequency zone.")
  }

  # Set default y_lab if not provided
  if (is.null(y_lab)) {
    y_lab <- ifelse(data$norm, "keyword (normalized) frequency", "keyword frequency")
  }


  col_leg <- ifelse(themety == "light", "black", "white")

  tdm <- data$tdm
  dat_l <- data$tdm_long
  year_cols <- data$year_cols

  zone_levels <- levels(data$zone)
  n_zones <- length(zone_levels)


  if (themety=="light"){
    col_class <- setNames(data$colors_light, zone_levels)
    base_theme <- theme_classic()
  } else {
    col_class <- setNames(data$colors_dark, zone_levels)
    base_theme <- theme_dark()
  }
  col_class <- paste0(col_class, "80") # Add transparency for background zones

  # Colors for example keywords (distinct from zone colors)
  # Skip some colors to avoid overlap with zone colors
  col_kw <- colorlist(type = themety)[(n_zones + 1):(2 * n_zones)]
  names(col_kw) <- ctu_noun

  # Set default line widths for zones
  if (is.null(size_class)) {
    size_class <- if (themety == "light") {
      c(0.2, 0.3, 0.35, 0.35)[1:n_zones]
    } else {
      c(0.35, 0.5, 0.5, 0.5)[1:n_zones]
    }
  }

  # Validate size_class length
  if (length(size_class) != n_zones) {
    stop(paste0("'size_class' must have length ", n_zones, " (one per zone)."))
    warning(paste0("'size_class' has length ", length(size_class),
                   " but there are ", n_zones, " zones. Recycling values."))
    size_class <- rep_len(size_class, n_zones)
  }
  names(size_class) <- zone_levels

  # Set default line widths for example keywords (decreasing hierarchy)
  if (is.null(size_example)) {
    size_example <- c(1.2, 1.05, 0.9, 0.7)[1:n_zones]
  }

  # Validate size_example length
  if (length(size_example) != n_zones) {
    warning(paste0("'size_example' has length ", length(size_example),
                   " but there are ", n_zones, " keywords. Recycling values."))
    size_example <- rep_len(size_example, n_zones)
  }
  names(size_example) <- ctu_noun

  # Prepare x-axis labels
  year <- dat_l$year %>% unique %>% as.numeric
  n_y <- diff(range(year))+1
  xaxlab <- year[1]+0:(n_y-1)
  xaxlab[-seq(1, n_y, by=r)] <- ""

  # Create zone labels with intervals
  zone_labels <- dat_l %>%
    distinct(zone, int_freq) %>%
    arrange(factor(zone, levels = zone_levels)) %>%
    transmute(label = paste(zone, int_freq)) %>%
    pull(label)

  # Extract example keyword data
  example_df <- do.call(rbind, lapply(ctu_noun, function(k) {
    df <- dat_l %>% filter(keyword == k)
    df$example <- k
    df
  }))
  example_df$example <- factor(example_df$example, levels = ctu_noun)

  # Theme customization (fixed negative margins)
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
      legend.margin = margin(t = -5, b = -2),
      #legend.margin = margin(t = 2, b = 0),  # Fixed: non-negative margins
      legend.background = element_rect(fill = NA),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.text = element_text(face = "bold", size = rel(0.85), color = col_leg),
      legend.title = element_text(face = "bold", size = rel(0.9), color = col_leg),
      panel.grid = element_blank(),
      plot.caption = element_text(hjust = 0.5, face = "italic", size = rel(0.9), color = col_leg),
      plot.caption.position = "plot"
    )

  # Create plot with two color scales
  p <- ggplot(dat_l, aes(x = chrono, y = freq, group = keyword)) +
    # First layer: all keywords by zone (transparent)
    geom_line(aes(colour = zone, linewidth = zone)) +
    scale_colour_manual(
      name = "Frequency Zone",
      values = col_class,
      breaks = zone_levels,
      labels = zone_labels,
      guide = guide_legend(order = 1, override.aes = list(alpha = 1, linewidth = 1.5))  # Solid in legend
    ) +

    ggnewscale::new_scale_colour() +
    # Second layer: example keywords (solid, variable width)
    geom_line(data = example_df,
              aes(x = chrono, y = freq, group = example, colour = example,
                  linewidth = example)) +
    scale_colour_manual(
      name = "Example Keywords",
      values = col_kw,
      guide = guide_legend(order = 2, override.aes = list(size = 1))
    ) +
      scale_linewidth_manual(
        values = c(size_class, size_example),
        guide = "none"
      ) +

    scale_x_continuous(expand = c(0.0065, 0), breaks = 1:n_y, labels = xaxlab) +
    scale_y_continuous(expand = c(0.01, 0)) +
    labs(x = x_lab, y = y_lab) +
    opts

  return(p)
}
