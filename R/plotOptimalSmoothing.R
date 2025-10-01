#' Plot Optimally Smoothed Keyword Frequency Curves
#'
#' This function visualizes the result of the optimal smoothing procedure applied
#' to keyword frequency curves over time. It overlays the raw frequencies (dashed gray lines)
#' and the smoothed curves (solid colored lines) using optimal smoothing parameters
#' previously selected by \code{optimalSmoothing()}.
#'
#' Smoothing is performed using B-spline basis functions with roughness penalties. The smoothed
#' functions are evaluated over a dense grid of points (default: 10 × number of years) to provide
#' a high-resolution visualization.
#'
#' @param data A list as returned by \code{importData()}, containing the term-document matrix and metadata.
#' @param opt_result A list as returned by \code{optimalSmoothing()}, including optimal degree and penalty type.
#' @param m_opt Optional. Integer indicating the spline degree to override the optimal one.
#' @param penalty_opt Optional. Character string to override the selected penalization type (e.g., \code{"m-2"}, \code{"2"}, \code{"1"}, or \code{"0"}).
#' @param ylab Character. Y-axis label. Defaults to \code{"Frequency"}.
#' @param xlim Numeric vector of length 2 specifying the x-axis range. If \code{NULL}, it is determined automatically.
#' @param main Optional. Plot title. If \code{NULL}, a dynamic expression is used showing the optimal degree, penalty, and GCV value.
#' @param lwd Numeric. Line width scaling factor. Default is \code{1}.
#' @param nx Integer. Number of evaluation points for the smoothed curves. Default is \code{10 × number of years}.
#'
#' @return A \code{ggplot2} object displaying raw (dashed gray) and smoothed (colored) frequency curves for all keywords over time.
#'
#' @examples
#' \dontrun{
#' tdm <- system.file("extdata", "tdm.csv", package = "cccc")
#' corpus <- system.file("extdata", "corpus.csv", package = "cccc")
#' data <- importData(tdm_file = tdm, corpus_file = corpus,
#' sep_tdm = ";",sep_corpus_info = ";",zone="stat")
#'
#' data_nchi <- normalization(data, normty = "nchi", sc = 1000)
#'
#' res_list <- list(
#'   "m-2" = smoothingSelection(data_nchi, penalty_type = "m-2", plot = FALSE),
#'   "2"   = smoothingSelection(data_nchi, penalty_type = "2", plot = FALSE),
#'   "1"   = smoothingSelection(data_nchi, penalty_type = "1", plot = FALSE),
#'   "0"   = smoothingSelection(data_nchi, penalty_type = "0", plot = FALSE)
#' )
#'
#' opt <- optimalSmoothing(res_list)
#' plotOptimalSmoothing(data_nchi, opt_result = opt)
#' }
#'
#' @export


plotOptimalSmoothing <- function(data,
                                 opt_result,
                                 m_opt = NULL,
                                 penalty_opt = NULL,
                                 ylab = "Frequency",
                                 xlim = NULL,
                                 main = NULL,
                                 lwd = 1,
                                 nx = NULL) {

  # Extract term-document matrix (TDM) and convert to matrix
  mat <- data$tdm %>%
    dplyr::select(dplyr::any_of(data$year_cols)) %>%
    as.matrix()

  # Assign row names (keywords) and remove rows with all zeros
  keywords <- data$tdm$keyword
  rownames(mat) <- keywords
  mat <- mat[rowSums(mat) > 0, , drop = FALSE]

  # Define time points (columns = years), number of years, and axis labels
  fdtime <- seq_len(ncol(mat))
  ny <- ncol(mat)
  xaxlab <- as.character(data$corpus_info$years)

  # Retrieve optimal parameters from previous smoothing selection
  opt_df <- opt_result$optSmoothing$summary_optimal
  if (is.null(m_opt)) m_opt <- opt_result$m_opt
  if (is.null(penalty_opt)) penalty_opt <- opt_result$penalty_opt
  lambda <- 10^opt_df$log_lambda_gcv[opt_df$degree == m_opt]
  lambda_gcv <- opt_df$gcv_min[opt_df$degree == m_opt]

  # Build B-spline basis and set the penalty order
  basis <- fda::create.bspline.basis(breaks = fdtime, norder = m_opt)
  lfd <- switch(penalty_opt,
                "m-2" = max(0, m_opt - 2),
                "2" = if (m_opt > 3) 2 else if (m_opt == 3) 1 else 0,
                "1" = if (m_opt > 2) 1 else 0,
                "0" = 0)

  # Apply smoothing using functional data analysis (FDA)
  fdpar <- fda::fdPar(basis, Lfdobj = lfd, lambda = lambda)
  smoothed <- fda::smooth.basis(argvals = fdtime, y = t(mat), fdParobj = fdpar)

  # Evaluate smoothed curves at a fine grid (default: 10 × number of years)
  if (is.null(nx)) nx <- ny * 10
  eval_time <- seq(min(fdtime), max(fdtime), length.out = nx)
  smooth_vals <- fda::eval.fd(eval_time, smoothed$fd)

  # Reshape smoothed values into long format for plotting
  smooth_df <- as.data.frame(smooth_vals)
  smooth_df$time <- eval_time
  long_smooth <- tidyr::pivot_longer(smooth_df, -time, names_to = "keyword", values_to = "freq")

  # Reshape raw (unsmoothed) values into long format for plotting
  raw_df <- as.data.frame(t(mat))
  raw_df$time <- fdtime
  long_raw <- tidyr::pivot_longer(raw_df, -time, names_to = "keyword", values_to = "freq")

  # Define y-axis limits with a small padding
  all_y <- c(long_raw$freq, long_smooth$freq)
  if (is.null(xlim)) xlim <- range(fdtime)
  ylim <- range(all_y, na.rm = TRUE) + c(-1, 1) * diff(range(all_y)) / 20

  # Set color palette for the smoothed lines (one per keyword)
  n_keywords <- length(unique(long_smooth$keyword))
  cols <- rep(colorlist("light")[9:100], length.out = n_keywords)

  # Customize x-axis tick labels (every 2 years)
  ticks_pos <- fdtime[seq(1, length(fdtime), by = 2)]
  ticks_years <- xaxlab[seq(1, length(xaxlab), by = 2)]

  # Plot raw (dashed) and smoothed (solid) keyword frequency curves
  ggplot2::ggplot() +
    ggplot2::geom_line(data = long_raw,
                       ggplot2::aes(x = time, y = freq, group = keyword),
                       color = "grey70", linetype = "dashed", linewidth = lwd * 0.3) +
    ggplot2::geom_line(data = long_smooth,
                       ggplot2::aes(x = time, y = freq, group = keyword, color = keyword),
                       linewidth = lwd * 0.7) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::scale_x_continuous(breaks = ticks_pos, labels = ticks_years) +
    ggplot2::labs(
      title = main %||%
        bquote("optimal smoothing: " ~ m == .(m_opt) ~ " with " ~ PEN[.(penalty_opt)] ~
                 "  (" ~ gcv == .(round(lambda_gcv, 4)) ~ ")"),
      x = NULL,
      y = ylab
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(size = 11, angle = 90, vjust = 0.3, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 11),
      axis.title.x = ggplot2::element_text(size = 12, margin = ggplot2::margin(t = 8)),
      axis.title.y = ggplot2::element_text(size = 12, margin = ggplot2::margin(r = 8))
    ) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
}





