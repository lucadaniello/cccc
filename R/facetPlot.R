#' Faceted Plot of Keyword Frequency Curves
#'
#' This function creates a faceted \code{ggplot2} visualization of keyword frequency curves
#' across time, grouped by frequency zone. Each panel (facet) corresponds to one frequency zone
#' and displays:
#' \enumerate{
#'   \item All keywords in that zone as thin, semi-transparent lines (background)
#'   \item A subset of selected keywords as highlighted, colored lines (foreground)
#' }
#'
#' Keywords can be selected by total frequency (top N), randomly (random N), or from a custom list.
#'
#' @param data A list object returned by \code{importData()}, containing:
#'   \itemize{
#'     \item \code{tdm_long}: long-format frequency data with columns \code{keyword},
#'           \code{zone}, \code{year}, \code{chrono}, \code{freq}, and \code{tot_freq}
#'     \item \code{zone}: factor levels for frequency zones
#'     \item \code{colors_light} and \code{colors_dark}: zone color palettes
#'   }
#' @param keyword_selection A list specifying how to select keywords to highlight:
#' \describe{
#'   \item{\code{type}}{Character. One of:
#'     \itemize{
#'       \item \code{"frequency"}: select top N keywords by total frequency per zone
#'       \item \code{"random"}: randomly sample N keywords per zone
#'       \item \code{"list"}: use custom keyword list from \code{kw.list}
#'     }
#'   }
#'   \item{\code{n}}{Integer. Number of keywords to select per zone
#'     (used for \code{type = "frequency"} or \code{"random"}). Default: 3.}
#'   \item{\code{kw.list}}{Character vector. Custom list of keywords to highlight
#'     (used for \code{type = "list"}). Must exist in the data.}
#' }
#' @param r Integer. Interval for thinning x-axis labels. If \code{r = 4},
#'   only every fourth year is labeled. Default: 4.
#' @param scales Character. Y-axis scaling across facets:
#'   \itemize{
#'     \item \code{"fixed"} (default): same y-axis scale for all zones
#'     \item \code{"free"}: independent y-axis scale per zone (useful for comparing patterns)
#'   }
#' @param leg Logical. Whether to display the legend. Default: \code{TRUE}.
#' @param themety Character. Plot theme: \code{"light"} (default) or \code{"dark"}.
#' @param size_class Numeric vector specifying line widths for background zone lines.
#'   Length must match number of zones. If \code{NULL}, defaults to 0.1 for all zones.
#' @param size_kw Numeric. Line width for highlighted keywords. Default: 0.7.
#' @param x_lab Character. Label for x-axis. Default: \code{"year"}.
#' @param y_lab Character. Label for y-axis. If \code{NULL} (default), uses
#'   "keyword frequency" or "keyword (normalized) frequency" based on \code{data$norm}.
#'
#' @return A \code{ggplot} object with faceted panels (one per zone) showing keyword
#'   frequency trajectories. Background lines show all keywords in each zone with
#'   transparency, while selected keywords are highlighted with distinct colors.
#'
#' @details
#' The function uses two color layers:
#' \itemize{
#'   \item Zone colors (with 70\% transparency) for background lines
#'   \item Distinct keyword colors for highlighted trajectories
#' }
#'
#' When \code{type = "list"}, keywords that don't exist in the data will be silently ignored.
#' If a zone has fewer than \code{n} keywords, all available keywords will be selected.
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
#' # Select top 3 keywords by frequency per zone
#' facetPlot(data,
#'   keyword_selection = list(type = "frequency", n = 3),
#'   r = 4,
#'   scales = "free"
#' )
#'
#' # Random selection with dark theme
#' facetPlot(data,
#'   keyword_selection = list(type = "random", n = 5),
#'   themety = "dark",
#'   scales = "fixed"
#' )
#'
#' # Custom keyword list
#' facetPlot(data,
#'   keyword_selection = list(
#'     type = "list",
#'     kw.list = c("person", "object", "instruct")
#'   ),
#'   leg = TRUE
#' )
#' }
#'
#' @export


facetPlot <- function(data,
                      keyword_selection = list(type = "frequency", n = 3, kw.list = NULL),
                      r = 4,
                      scales = "fixed",
                      leg = TRUE,
                      themety = "light",
                      size_class = NULL,
                      size_kw = NULL,
                      x_lab = "year",
                      y_lab = NULL) {

  # Input validation
  valid_types <- c("frequency", "random", "list")
  if (!keyword_selection$type %in% valid_types) {
    stop(paste0("Invalid selection type. Must be one of: ",
                paste(valid_types, collapse = ", ")))
  }

  if (keyword_selection$type %in% c("frequency", "random") && is.null(keyword_selection$n)) {
    stop("Argument 'n' is required for type 'frequency' or 'random'.")
  }

  if (keyword_selection$type == "list" && is.null(keyword_selection$kw.list)) {
    stop("Argument 'kw.list' is required for type 'list'.")
  }

  # Extract data
  if (is.null(y_lab)) {
    y_lab <- ifelse(data$norm, "keyword (normalized) frequency", "keyword frequency")
  }

  zone_levels <- data$zone
  n_zones <- length(zone_levels)
  m <- keyword_selection$n

  # Set theme-specific defaults
  if (themety == "light") {
    col_class <- setNames(data$colors_light, zone_levels)
    if (is.null(size_class)) size_class <- rep(0.1, n_zones)
  } else {
    col_class <- setNames(data$colors_dark, zone_levels)
    if (is.null(size_class)) size_class <- rep(0.2, n_zones)
  }

  # Validate size_class length
  if (length(size_class) != n_zones) {
    warning(paste0("'size_class' length (", length(size_class),
                   ") doesn't match number of zones (", n_zones, "). ",
                   "Recycling or truncating values."))
    size_class <- rep_len(size_class, n_zones)
  }

  # Set size_kw default
  if (is.null(size_kw)) size_kw <- 0.7

  col_class <- paste0(col_class, "70")  # Add transparency for background zones

  # Generate distinct colors for highlighted keywords (avoid zone colors)
  col_kw <- colorlist(type = themety)[(n_zones + 1):(n_zones + 20)]

  d <- data$tdm_long
  type <- keyword_selection$type

  switch(
    type,
    "random" = {
      # Count keywords per zone and sample appropriately
      kw_by_zone <- d %>%
        select(keyword, zone) %>%
        distinct() %>%
        group_by(zone) %>%
        summarise(available = list(keyword), .groups = "drop")

      kw <- kw_by_zone %>%
        mutate(
          selected = map(available, ~ {
            n_take <- min(m, length(.x))
            sample(.x, n_take)
          })
        ) %>%
        select(zone, selected) %>%
        unnest(selected) %>%
        rename(keyword = selected) %>%
        mutate(id = row_number())
    },
    "frequency" = {
      # Select top m keywords by frequency per zone
      kw <- d %>%
        select(keyword, zone, tot_freq) %>%
        distinct() %>%
        group_by(zone) %>%
        arrange(desc(tot_freq)) %>%
        mutate(rank = row_number()) %>%
        filter(rank <= m) %>%
        ungroup() %>%
        mutate(id = row_number()) %>%
        select(zone, keyword, id)
    },
    "list" = {
      # Validate keyword existence
      available_kw <- intersect(keyword_selection$kw.list, d$keyword)
      if (length(available_kw) == 0) {
        stop("None of the keywords in 'kw.list' exist in the data.")
      }
      missing_kw <- setdiff(keyword_selection$kw.list, d$keyword)
      if (length(missing_kw) > 0) {
        warning(paste0("Keywords not found (ignored): ",
                       paste(missing_kw, collapse = ", ")))
      }

      kw <- tibble(keyword = available_kw) %>%
        left_join(
          d %>% select(keyword, zone) %>% distinct(),
          by = "keyword"
        ) %>%
        mutate(id = row_number()) %>%
        select(zone, keyword, id)
    }
  )

  # Assign colors to selected keywords
  kw <- kw %>%
    mutate(kw_color = col_kw[id]) %>%
    select(-id)

  # Filter data for selected keywords
  kw_df <- d %>%
    filter(keyword %in% kw$keyword) %>%
    left_join(kw %>% select(keyword, kw_color), by = "keyword")

  # Set keyword factor levels (for consistent ordering)
  kw_df$keyword <- factor(kw_df$keyword, levels = kw$keyword)

  # Prepare x-axis labels
  year <- d$year %>% unique() %>% as.numeric()
  n_y <- diff(range(year)) + 1
  xaxlab <- year[1] + 0:(n_y - 1)
  xaxlab[-seq(1, n_y, by = r)] <- ""

  # Define color scales
  zone_colors <- setNames(col_class, zone_levels)
  kw_colors <- setNames(unique(kw_df$kw_color), unique(kw_df$keyword))
  all_colors <- c(zone_colors, kw_colors)

  # Set base theme
  base_theme <- if (themety == "light") theme_classic() else theme_dark()

  # Theme options
  opts <- base_theme +
    theme(
      plot.margin = unit(c(0.1, 0.1, 0.5, 0.1), "lines"),  # More bottom margin for 2 legends
      axis.text = element_text(angle = 90, size = rel(0.9)),
      axis.text.x = element_text(vjust = 0.5),
      axis.text.y = element_text(hjust = 0.5),
      legend.position = if (leg) "bottom" else "none",
      legend.box = "vertical",  # Stack legends vertically
      legend.box.spacing = unit(0.2, "cm"),  # Space between the two legends
      legend.key = element_rect(colour = NA, fill = NA),
      legend.key.width = unit(1.5, "lines"),
      legend.key.height = unit(0.4, "lines"),
      legend.text = element_text(size = rel(0.80)),
      legend.title = element_text(size = rel(0.85), face = "bold"),
      panel.grid = element_blank()
    )

  # Create base plot
  p <- ggplot(d, aes(x = chrono, y = freq, group = keyword)) +
    labs(x = x_lab, y = y_lab) +
    scale_x_continuous(expand = c(0.0065, 0), breaks = 1:n_y, labels = xaxlab) +
    scale_y_continuous(expand = c(0.01, 0)) +
    opts

  # Add faceting and layers based on scale type
  if (scales == "fixed") {
    p <- p +
      facet_wrap(~zone, nrow = 2) +
      # Layer 1: Background zone lines (WITH legend, order=1)
      geom_line(aes(colour = zone, linewidth = zone)) +
      scale_colour_manual(
        name = "Frequency Zone",
        values = zone_colors,
        guide = guide_legend(order = 1, override.aes = list(linewidth = 1.5, alpha = 1))
      ) +
      scale_linewidth_manual(values = setNames(size_class, zone_levels), guide = "none") +
      # Reset color scale for keywords
      ggnewscale::new_scale_colour() +
      # Layer 2: Foreground keyword lines (with legend, order=2)
      geom_line(data = kw_df, aes(colour = keyword), linewidth = size_kw) +
      scale_colour_manual(
        name = "Selected Keywords",
        values = kw_colors,
        guide = guide_legend(order = 2, override.aes = list(linewidth = 1.2))
      )
  } else {
    p <- p +
      facet_wrap(~zone, nrow = 2, scales = "free_y") +
      # Layer 1: Background zone lines (WITH legend, order=1)
      geom_line(aes(colour = zone, linewidth = zone)) +
      scale_colour_manual(
        name = "Frequency Zone",
        values = zone_colors,
        guide = guide_legend(order = 1, override.aes = list(linewidth = 1.5, alpha = 1))
      ) +
      scale_linewidth_manual(values = setNames(size_class, zone_levels), guide = "none") +
      # Reset color scale for keywords
      ggnewscale::new_scale_colour() +
      # Layer 2: Foreground keyword lines (with legend, order=2)
      geom_line(data = kw_df, aes(colour = keyword), linewidth = size_kw) +
      scale_colour_manual(
        name = "Selected Keywords",
        values = kw_colors,
        guide = guide_legend(order = 2, override.aes = list(linewidth = 1.2))
      )
  }

  return(p)
}
