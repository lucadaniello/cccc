#' Plot of Temporal Curves for Frequency Zones and Example Keywords
#'
#' This function generates a line plot of keyword frequency over time,
#' with two distinct visual layers:
#' \enumerate{
#'   \item Background layer showing all keywords grouped by frequency zone (with transparency)
#'   \item Foreground layer highlighting specific example keywords (one per zone) with solid colors
#' }
#'
#' The plot uses \code{ggnewscale::new_scale_colour()} to apply two separate color scales,
#' allowing both zone colors and example keyword colors to coexist in the same plot.
#'
#' @param data A list object returned by \code{importData()}, containing:
#'   \itemize{
#'     \item \code{tdm_long}: long-format frequency data
#'     \item \code{tdm}: original term-document matrix
#'     \item \code{zone}: factor levels for frequency zones
#'     \item \code{colors_light} and \code{colors_dark}: zone color palettes
#'   }
#' @param ctu_noun Character vector with exactly one keyword per frequency zone.
#'   The number of keywords must match the number of zones in the data
#'   (e.g., 4 for statistical zones: VH, H, L, VL; or 3 for linguistic zones: high, medium, low).
#'   Each keyword must exist in the dataset and belong to a different zone.
#' @param r Integer. Interval for thinning x-axis labels. If \code{r = 2},
#'   only every second year is labeled. Default: \code{1} (show all years).
#' @param themety Character. Plot theme: \code{"light"} (default) or \code{"dark"}.
#' @param size_class Numeric vector specifying line widths for each frequency zone.
#'   Must have length equal to the number of zones. If \code{NULL}, defaults to:
#'   \itemize{
#'     \item light theme: c(0.2, 0.3, 0.35, 0.35)
#'     \item dark theme: c(0.35, 0.5, 0.5, 0.5)
#'   }
#' @param size_example Numeric vector specifying line widths for each example keyword.
#'   Must have length equal to the number of zones. If \code{NULL}, defaults to
#'   c(1.15, 1.05, 0.95, 0.75), creating a visual hierarchy.
#' @param x_lab Character. Label for the x-axis. Default: \code{"year"}.
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
#'   sep_tdm = ";", sep_corpus_info = ";", zone = "stat")
#'
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

  # Validate number of keywords
  if (length(ctu_noun) != n_zones) {
    stop(paste0("Please provide exactly ", n_zones, " keywords (one for each zone: ",
                paste(zone_levels, collapse = ", "), ")."))
  }

  # Validate keyword existence
  missing_kw <- setdiff(ctu_noun, dat_l$keyword)
  if (length(missing_kw) > 0) {
    stop(paste0("Keywords not found in data: ", paste(missing_kw, collapse = ", ")))
  }

  # Set color palettes
  if (themety == "light") {
    col_class <- setNames(data$colors_light, zone_levels)
    base_theme <- theme_classic()
  } else {
    col_class <- setNames(data$colors_dark, zone_levels)
    base_theme <- theme_dark()
  }
  col_class <- paste0(col_class, "80")  # Add transparency for background zones

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
    warning(paste0("'size_class' has length ", length(size_class),
                   " but there are ", n_zones, " zones. Recycling values."))
    size_class <- rep_len(size_class, n_zones)
  }
  names(size_class) <- zone_levels

  # Set default line widths for example keywords (decreasing hierarchy)
  if (is.null(size_example)) {
    size_example <- c(1.15, 1.05, 0.95, 0.75)[1:n_zones]
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
  n_y <- diff(range(year)) + 1
  xaxlab <- year[1] + 0:(n_y - 1)
  xaxlab[-seq(1, n_y, by = r)] <- ""

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
      plot.margin = unit(c(0.1, 0.1, 0.4, 0.1), "lines"),
      axis.text = element_text(angle = 90, size = rel(0.9)),
      axis.text.x = element_text(vjust = 0.5),
      axis.text.y = element_text(hjust = 0.5),
      axis.ticks.length = unit(0.1, "cm"),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.box.spacing = unit(0.3, "lines"),
      legend.margin = margin(t = 2, b = 0),  # Fixed: non-negative margins
      legend.background = element_rect(fill = NA),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.text = element_text(face = "bold", size = rel(0.85), color = col_leg),
      legend.title = element_text(face = "bold", size = rel(0.9), color = col_leg),
      panel.grid = element_blank()
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

    # Reset color scale for example keywords
    ggnewscale::new_scale_colour() +

    # Second layer: example keywords (solid, variable width)
    geom_line(
      data = example_df,
      aes(x = chrono, y = freq, group = example, colour = example, linewidth = example)
    ) +
    scale_colour_manual(
      name = "Example Keywords",
      values = col_kw,
      guide = guide_legend(order = 2, override.aes = list(linewidth = 1.5))
    ) +

    # Single linewidth scale for both zones and examples
    scale_linewidth_manual(
      values = c(size_class, size_example),
      guide = "none"
    ) +

    # Axes and labels
    scale_x_continuous(expand = c(0.0065, 0), breaks = 1:n_y, labels = xaxlab) +
    scale_y_continuous(expand = c(0.01, 0)) +
    labs(x = x_lab, y = y_lab) +
    opts

  return(p)
}
