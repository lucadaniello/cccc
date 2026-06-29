#' Plot Temporal Trajectories/Curves of Keyword Frequencies
#'
#' This function generates a temporal plot of keyword frequencies (normalized or raw)
#' from a TDM in long format. Each keyword's trajectory over time is displayed as a line,
#' with color and line width mapped to its frequency zone. The plot supports both
#' light and dark themes and allows customization of axis labels and line sizes.
#'
#' @param data A list object returned by \code{importData()}, containing:
#'   \itemize{
#'     \item \code{tdm_long}: a tibble in long format with columns \code{keyword},
#'           \code{year}, \code{chrono}, \code{freq}, \code{zone}, and \code{int_freq}
#'     \item \code{year_cols}: numeric indices of year columns in the original TDM
#'     \item \code{colors_light} and \code{colors_dark}: color palettes for zones
#'     \item \code{norm}: logical indicating if data is normalized
#'   }
#' @param r Integer. Interval for thinning x-axis year labels. If \code{r = 3},
#'   only every third year is labeled. Default: \code{1} (show all years).
#' @param themety Character. Plot theme: \code{"light"} (default) or \code{"dark"}.
#' @param size_class Numeric vector specifying line widths for each frequency zone.
#'   If \code{NULL}, defaults are set based on theme and number of zones:
#'   \itemize{
#'     \item light theme: c(0.1, 0.2, 0.35, 0.35)
#'     \item dark theme: c(0.35, 0.5, 0.5, 0.5)
#'   }
#'   The vector is automatically truncated to match the number of zones.
#' @param x_lab Character. Label for the x-axis. Default: \code{"year"}.
#' @param y_lab Character. Label for the y-axis. Default: automatically set based on
#'   \code{data$norm} (\code{"keyword (normalized) frequency"} if normalized,
#'   \code{"keyword frequency"} otherwise). Can be customized by the user.
#'
#' @return A \code{ggplot2} object showing keyword frequency trajectories over time,
#'   colored and sized by frequency zone. A horizontal legend at the bottom displays
#'   zone labels with their frequency intervals.
#'
#' @details
#' The function uses the \code{chrono} variable (a continuous time index starting from 1)
#' for the x-axis, while displaying actual year labels. This ensures correct spacing
#' even if years are not consecutive in the data.
#'
#' Line widths are mapped to frequency zones using \code{scale_linewidth_manual()},
#' with higher-frequency zones typically shown with thicker lines (but this can be
#' customized with the \code{size_class} parameter).
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
#' curvePlot(data)
#'
#' # Custom theme and label thinning
#' curvePlot(data, r = 2, themety = "dark")
#'
#' # Custom y-axis label
#' curvePlot(data, y_lab = "Normalized frequency (per 1000 tokens)")
#'
#' # Manual line width specification
#' curvePlot(data, size_class = c(0.3, 0.4, 0.5, 0.6))
#' }
#'
curvePlot <- function(data,
                      r = 1,
                      themety = "light",
                      size_class = NULL,
                      x_lab = "year",
                      y_lab = NULL) {

  # Input validation
  if (!"tdm_long" %in% names(data)) {
    stop("Input data must contain 'tdm_long'. Run importData() first.")
  }

  # Set default y_lab if not provided
  if (is.null(y_lab)) {
    y_lab <- ifelse(data$norm, "keyword (normalized) frequency", "keyword frequency")
  }

  col_leg <- ifelse(themety == "light", "black", "white")

  dat_l <- data$tdm_long
  year_cols <- data$year_cols

  # Get zone information from data object (more efficient)
  zone_levels <- levels(data$zone)
  n_zones <- length(zone_levels)

  # Ensure zone is a factor with correct levels
  dat_l <- dat_l %>%
    mutate(zone = factor(zone, levels = zone_levels))

  # Set theme-specific defaults
  if (themety == "light") {
    base_theme <- theme_classic()
    col_class <- setNames(data$colors_light, zone_levels)
    if (is.null(size_class)) {
      size_class <- c(0.1, 0.2, 0.35, 0.35)[1:n_zones]
    }
  } else {
    base_theme <- theme_dark()
    col_class <- setNames(data$colors_dark, zone_levels)
    if (is.null(size_class)) {
      size_class <- c(0.35, 0.5, 0.5, 0.5)[1:n_zones]
    }
  }

  # Validate size_class length
  if (length(size_class) != n_zones) {
    warning(paste0("'size_class' has length ", length(size_class),
                   " but there are ", n_zones, " zones. Using first ",
                   n_zones, " values or recycling."))
    size_class <- rep_len(size_class, n_zones)
  }

  # Prepare x-axis labels
  year <- dat_l$year %>% unique %>% as.numeric
  n_y <- diff(range(year)) + 1
  xaxlab <- year[1] + 0:(n_y - 1)
  xaxlab[-seq(1, n_y, by = r)] <- ""

  # Create zone labels with frequency intervals
  zone_labels <- dat_l %>%
    distinct(zone, int_freq) %>%
    arrange(factor(zone, levels = zone_levels)) %>%
    transmute(label = paste(zone, int_freq)) %>%
    pull(label)

  # Theme customization (fixed negative margins)
  opts <- base_theme +
    theme(
      plot.margin = unit(c(0.1, 0.1, 0.3, 0.1), "lines"),
      axis.text = element_text(angle = 90, size = rel(0.9)),
      axis.text.x = element_text(vjust = 0.5),
      axis.text.y = element_text(hjust = 0.5),
      axis.ticks.length = unit(0.1, "cm"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = margin(t = 2, b = 0),  # Fixed: non-negative margins
      legend.background = element_rect(fill = NA),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.text = element_text(color = col_leg, face = "bold", size = rel(0.95)),
      legend.title = element_text(color = col_leg, face = "bold", size = rel(1)),
      panel.grid = element_blank()
    )

  # Create plot
  p <- ggplot(dat_l, aes(x = chrono, y = freq, group = keyword)) +
    geom_line(aes(colour = zone, linewidth = zone)) +
    scale_x_continuous(expand = c(0.0065, 0), breaks = 1:n_y, labels = xaxlab) +
    scale_y_continuous(expand = c(0.01, 0)) +
    scale_colour_manual(
      name = "Frequency Zone",
      values = col_class,
      breaks = zone_levels,
      labels = zone_labels,
      guide = guide_legend(order = 1, override.aes = list(linewidth = 2))  # Fixed: linewidth instead of size
    ) +
    scale_linewidth_manual(  # Fixed: use scale_linewidth_manual instead of scale_discrete_manual
      values = setNames(size_class, zone_levels),
      guide = "none"
    ) +
    labs(x = x_lab, y = y_lab) +  # More concise way to set labels
    opts

  return(p)
}

