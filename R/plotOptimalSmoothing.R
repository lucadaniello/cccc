#' Plot the Optimally Smoothed Keyword Curves
#'
#' This function visualizes the result of the optimal smoothing procedure applied
#' to keyword frequency curves over time. It overlays raw curves (dashed gray lines)
#' and smoothed curves (solid colored lines) for all selected keywords.
#' The optimal spline degree and penalization type are used by default, unless
#' explicitly overridden via `m_opt` and `penalty_opt`.
#'
#' The smoothing is performed using B-splines with roughness penalties, and the smoothed
#' functions are evaluated at a high-resolution grid (`nx` points) for smooth visualization.
#'
#' @param data A list returned by [importData()], containing at least:
#'   \code{tdm}, \code{corpus_info}, and \code{year_cols}.
#'   The element \code{optimal_results} must also be set, typically from
#'   `data$optimal_results <- results_list[[opt$penalty_opt]]`.
#' @param opt_result A list returned by [optimalSmoothing()], containing the selected
#'   optimal degree and penalty.
#' @param m_opt Optional integer. If specified, overrides the spline degree in `opt_result`.
#' @param penalty_opt Optional character. If specified, overrides the penalty in `opt_result`.
#'   Must be one of `"m-2"`, `"2"`, `"1"`, `"0"`.
#' @param ylab Character. Label for the y-axis. Default is `"Frequency"`.
#' @param xlim Numeric vector of length 2 specifying the x-axis limits. Default is automatic.
#' @param main Optional plot title. If `NULL`, an automatic expression with GCV info is used.
#' @param lwd Numeric. Line width multiplier. Default is `1`.
#' @param nx Integer. Number of evaluation points for the smoothed curves.
#'   Default is `10 × number of years`.
#'
#' @return A `ggplot2` object showing raw (dashed) and smoothed (solid) keyword curves over time.
#'
#' @examples
#' \dontrun{
#' data <- importData("tdm.csv", "corpus.csv")
#' res_list <- list(
#'   "m-2" = smoothingSelection(data, penalty_type = "m-2", plot = FALSE),
#'   "2"   = smoothingSelection(data, penalty_type = "2", plot = FALSE),
#'   "1"   = smoothingSelection(data, penalty_type = "1", plot = FALSE),
#'   "0"   = smoothingSelection(data, penalty_type = "0", plot = FALSE)
#' )
#'
#' opt <- optimalSmoothing(res_list)
#' data$optimal_results <- res_list[[opt$penalty_opt]]
#' plotOptimalSmoothing(data, opt_result = opt)
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
  opt_df <- data$optimal_results$summary_optimal
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





