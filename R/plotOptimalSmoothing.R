#' Plot Optimally Smoothed Keyword Frequency Curves
#'
#' This function visualizes the result of the optimal smoothing procedure applied
#' to keyword frequency curves over time. It overlays the raw frequencies (dashed gray lines)
#' and the smoothed curves (solid colored lines) using optimal smoothing parameters
#' previously selected by \code{optimalSmoothing()}.
#'
#' Smoothing is performed using B-spline basis functions with roughness penalties.
#' The smoothed functions are evaluated over the sampled points (by default) or over
#' a dense grid of points (input \code{nx} × total number of years) to provide a
#' high-resolution visualization.
#' The function returns, besides a ggplot object, both the smoothed functional data object
#' and a matrix of evaluated smoothed curves over a chosen grid.
#'
#' @param data A list as returned by \code{importData()}, containing the term-document matrix and metadata.
#' @param opt_result A list as returned by \code{optimalSmoothing()}, including optimal order and penalty type (as well as the optimal smoothing parameter lLambda), or a list containing the smoothing selection results for the chosen penalty type.
#' @param penalty Optional. Character string to override the selected penalization type (e.g., \code{"m-2"}, \code{"2"}, \code{"1"}, or \code{"0"}).
#' @param m Optional. If \code{penalty_opt} is not \code{NULL}, integer indicating the spline order to override the optimal one.
#' @param ylab Character. Y-axis label. Defaults to \code{"normalized frequency"}.
#' @param xlim Numeric vector of length 2 specifying the x-axis range. If \code{NULL}, it is determined automatically.
#' @param nug Numeric vector of length 2 setting x-axis and y-axis padding factors to avoid clipping of curves at the plot borders. Default is \code{(100,20)}.
#' @param r Integer. Thinning factor for x-axis year labels (e.g., every \code{r}-th year is labeled). Default is \code{2}.
#' @param main Optional. Plot title. If \code{NULL}, a dynamic expression is used showing the optimal degree, penalty, and GCV value.
#' @param lwd Numeric. Line width scaling factor. Default is \code{1}.
#' @param nx Integer. Number of evaluation points for the smoothed curves. Default is \code{number of sampled time points (e.g. years)}.
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
                                 m = NULL,
                                 penalty = NULL,
                                 ylab = "normalized frequency",
                                 xlim = NULL,
                                 nug = c(20, 100),
                                 r = 2,
                                 main = NULL,
                                 lwd = 1,
                                 nx = FALSE) {

  # Extract term-document matrix (TDM) and convert to matrix
  mat <- data$tdm %>%
    dplyr::select(dplyr::any_of(data$year_cols)) %>%
    as.matrix()

  # Assign row names (keywords) and remove rows with all zeros
  keywords <- data$tdm$keyword
  rownames(mat) <- keywords
  mat <- mat[rowSums(mat) > 0, , drop = FALSE]

  # Define time points (columns = years), number of years, and axis labels
  year <- names(data$tdm)[data$year_cols]
  samplpo <- year %>% as.numeric
  ny <- diff(range(samplpo))+1
  xaxlab <- (samplpo)[1]+0:(ny-1)
  xaxlab[-seq(1, ny, by=r)] <- ""

  fdtime <- 1+samplpo-samplpo[1]
  br <- fdtime

  # Retrieve optimal parameters from previous smoothing selection
  if (is.null(penalty) & is.null(m)) {
    m_opt <- opt_result$m_opt
    penalty_opt <- opt_result$penalty_opt
    llambda_opt <- opt_result$llambda_opt
    gcv_opt <- opt_result$gcv_opt
  }
  else {
    if (is.null(penalty) | !is.null(m)) {
      stop("'penalty_opt' must be provided if 'm_opt' is specified.")
    }
    if (!is.null(penalty) | is.null(m_opt)) {
      opt_summary <- opt_result$opt_summary[which(opt_result$opt_summary$penalty_type == penalty),]
      m_opt <- opt_summary$degree
      penalty_opt <- penalty
      llambda_opt <- opt_summary$log_lambda
      gcv_opt <- opt_summary$gcv
    } else {
      penalty_opt <- penalty
      m_opt <- m
      llambda_opt <- opt_result$optimal_gcv$log_lambda[opt_result$optimal_gcv$degree == m_opt]
      gcv_opt <- opt_result$optimal_gcv$gcv[opt_result$optimal_gcv$degree == m_opt]
    }
  }

  lambda <- 10^llambda_opt


  # Build B-spline basis and set the linear differential operator (defined by the order of the derivative)
  basis <- fda::create.bspline.basis(breaks = br, norder = m_opt)
  lfd <- switch(penalty_opt,
                "m-2" = max(0, m_opt - 2),
                "2" = if (m_opt > 3) 2 else if (m_opt == 3) 1 else 0,
                "1" = if (m_opt > 2) 1 else 0,
                "0" = 0)
  # m = 0 penalizes the squared difference from 0 of the function
  # 1 penalize the square of the slope or velocity
  # 2 penalize the squared acceleration
  # 3 = penalize the squared rate of change of acceleration, 4 = penalize the squared curvature of acceleration?

  # Apply smoothing using the roughness penalty approach:
  # smoothing is achieved by penalizing the integral of the square of the derivative defined by 'lfd'
  fdpar <- fda::fdPar(basis, Lfdobj = lfd, lambda = lambda)
  smoothed <- fda::smooth.basis(argvals = fdtime, y = t(mat), fdParobj = fdpar)

  # Attach degree and penalty info to the output object
  smoothed$penalty <- penalty_opt
  smoothed$order <- m_opt

  # Evaluate smoothed curves at a fine grid? (default: number of sampled points, fdtime).
  # The user can fix, alternatively, the period length (nx=1) if the sampled points are unequally distant, or a multiple of it.
  if (!nx) {
    eval_time <- fdtime
    smooth_vals <- fda::eval.fd(eval_time, smoothed$fd)
  } else {
    nx <- ny * as.numeric(nx)
    eval_time <- seq(min(fdtime), max(fdtime), length.out = nx)
    smooth_vals <- fda::eval.fd(eval_time, smoothed$fd)
  }

  # Reshape smoothed values into long format for plotting
  smooth_df <- as.data.frame(smooth_vals)
  smooth_df$eval_time <- eval_time
  long_smooth <- tidyr::pivot_longer(smooth_df, -eval_time, names_to = "keyword", values_to = "eval")

  # Reshape raw (unsmoothed) values into long format for plotting
  raw_df <- as.data.frame(t(mat))
  raw_df$fdtime <- fdtime
  long_raw <- tidyr::pivot_longer(raw_df, -fdtime, names_to = "keyword", values_to = "freq")

  # Define y-axis limits with a small padding
  all_y <- c(long_raw$freq, long_smooth$eval)
  if (is.null(xlim)) xlim <- range(fdtime) + c(-1, 1) * diff(range(fdtime)) / nug[1]
  ylim <- range(all_y, na.rm = TRUE) + c(-1, 1) * diff(range(all_y)) / nug[2]

  # Set color palette for the smoothed lines (one per keyword)
  n_keywords <- length(unique(long_smooth$keyword))
  cols <- rep(colorlist("light")[-(1:8)], length.out = n_keywords)

  # Customize x-axis tick labels (every 1 years)
  ticks_pos <- seq(1, length(xaxlab), by = 1)
  ticks_years <- xaxlab[seq(1, length(xaxlab), by = 1)]

  # Plot raw (dashed) and smoothed (solid) keyword frequency curves
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(data = long_raw,
                       ggplot2::aes(x = fdtime, y = freq, group = keyword),
                       color = "grey70", linetype = "dotdash", linewidth = lwd * 0.3) +
    ggplot2::geom_line(data = long_smooth,
                       ggplot2::aes(x = eval_time, y = eval, group = keyword, color = keyword),
                       linewidth = lwd * 0.5) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::scale_x_continuous(breaks = ticks_pos, labels = ticks_years) +
    ggplot2::labs(
      title = main %||%
        bquote("optimal smoothing: " ~ m == .(m_opt) ~ " with " ~ PEN[.(penalty_opt)] ~
                 "  (" ~ gcv == .(round(gcv_opt, 4)) ~ ")"),
      y = ylab
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(size = rel(.9), angle = 90, vjust = .5, hjust = .5),
      axis.text.y = ggplot2::element_text(size = rel(.9), angle = 90, vjust = .3, hjust = .5),
      axis.title.x=element_blank(),
      axis.title.y = ggplot2::element_text(size = rel(.925)),
      plot.title = element_text(size = rel(.95), face = "bold"),
      plot.margin = unit(c(4, 0, 2, 1.5), "pt"),
    ) +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = F)

  # Return optimal results and optional plots
  res <- list(
    smoothed = smoothed,
    smooth_vals = smooth_vals,
    plot = p
  )
  return(res)
}





