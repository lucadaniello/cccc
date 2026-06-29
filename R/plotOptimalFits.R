#' Plot Optimal Smoothed Curves for Selected Keywords
#'
#' This function visualizes raw and smoothed time trends of keyword frequencies based on smoothing spline fits.
#' A subset of keywords is selected based on the residual Root Mean Square (RMS) error of the smoothed fit.
#' Both individual and combined plots are returned.
#'
#' @param data A list containing the outputs returned by [importData()]
#' @param smoothed The functional data object, which is the smoothed spline fit based on the optimal parameters, returned by [plotOptimalSmoothing()]. If it is \code{NULL} argument \code{opt_res} must be provided.
#' @param opt_res A list with the optimal smoothing configuration returned by [optimalSmoothing()]. If it is \code{NULL} argument \code{smoothed} must be provided.
#' @param n_curves Integer. Number of keywords to display, chosen across the RMS distribution (default is 9).
#' @param show_zone Logical. If \code{TRUE}, include the frequency zone (if available in \code{tdm}) in the plot labels.
#' @param graph Logical. If \code{FALSE} (default), print the sole combined plot to the active device. If \code{TRUE}, print all individual plots as well.
#'
#' @return A named list with the following elements (returned invisibly):
#' \itemize{
#'   \item \code{singleKeywordPlot}: A list of ggplot objects, one for each selected keyword (full title included).
#'   \item \code{combinedKeywordPlot}: A combined \code{patchwork} plot showing all selected fits.
#' }
#'
#' @details
#' The function requires the smoothed spline fit based on the optimal parameters, which is either provided directly via the \code{smoothed} argument or computed internally using the \code{opt_res} argument.
#' It then computes the residual RMS for each keyword and selects a subset using quantiles.
#' Each keyword's frequency curve (raw and smoothed) is plotted, either individually or in a combined layout using the \code{patchwork} package.
#' The individual plots include detailed titles with keyword, RMS value, RMS-based position, and optionally the frequency zone.
#'
#' @examples
#' \dontrun{
#' tdm <- system.file("extdata", "tdm.csv", package = "cccc")
#' corpus <- system.file("extdata", "corpus.csv", package = "cccc")
#' data <- importData(tdm_file = tdm, corpus_file = corpus,
#' sep_tdm = ";",sep_corpus_info = ";",zone="stat")
#'
#' data_nchi <- normalization(data, normty = "nchi", sc = 1)
#'
#' res_list <- list(
#'   "m-2" = smoothingSelection(data_nchi, penalty_type = "m-2", plot = FALSE),
#'   "2"   = smoothingSelection(data_nchi, penalty_type = "2", plot = FALSE),
#'   "1"   = smoothingSelection(data_nchi, penalty_type = "1", plot = FALSE),
#'   "0"   = smoothingSelection(data_nchi, penalty_type = "0", plot = FALSE)
#' )
#'
#' # One valid way to obtain the result is to provide the optimal smoothing result:
#'
#' opt <- optimalSmoothing(res_list)
#' result <- plotOptimalFits(data = data_nchi, opt_res = opt, graph = TRUE)
#'
#' # The other valid way is to provide the smoothed object directly:
#'
#' plot_opt <- plotOptimalSmoothing(data = data_nchi, opt_result = opt)
#' result <- plotOptimalFits(data = data_nchi, smoothed = plot_opt$smoothed, graph = TRUE)
#' }
#'
#' @export


plotOptimalFits <- function(data,
                            smoothed = NULL,
                            opt_res = NULL,
                            n_curves = 9,
                            show_zone = FALSE,
                            graph = FALSE) {

  # --- 1. Input validation ---
  if (is.null(smoothed) & is.null(opt_res)) {
    message("Either argument 'smoothed' or argument 'opt_res' must be provided.")
  }
  else if (is.null(smoothed)) {
    if (missing(data) || is.null(data$tdm) || is.null(data$year_cols)) {
    message("Argument 'data' is missing or incomplete. It must contain 'tdm' and 'year_cols'.")
  }
  if (is.null(opt_res$m_opt) || is.null(opt_res$penalty_opt)) {
    message("Argument 'opt_res' is incomplete. It must include 'm_opt' and 'penalty_opt'.")
  }}


  # --- 2. Compute smoothed functional object using optimal settings (if not provided as input) ---
  wsmooth <- if(!is.null(smoothed)) smoothed else getWsmooth(data, opt_res)
  penalty <- wsmooth$penalty
  order <- wsmooth$order

  # --- 3. Prepare the matrix of keyword frequencies ---
  mat <- data$tdm %>%
    dplyr::select(dplyr::any_of(data$year_cols)) %>%
    as.matrix()
  rownames(mat) <- data$tdm$keyword
  mat <- mat[rowSums(mat) > 0, , drop = FALSE]  # Remove keywords with zero frequency across all years

  # --- 4. Define time information and matrix size ---
  samplpo <- names(data$tdm)[data$year_cols] %>% as.numeric
  fdtime <- 1+samplpo-samplpo[1]
  n <- nrow(mat)
  nsamplpo <- ncol(mat)

  # --- 5. Compute RMS residuals from smoothing results ---
  df <- wsmooth$df
  gcv <- wsmooth$gcv
  errs <- ((length(fdtime) - df)^2 * gcv) / length(fdtime)
  werrs <- sqrt(errs / nsamplpo)

  # --- 6. Select 'n_curves' keywords based on RMS quantiles ---
  quants <- round(quantile(1:n, probs = seq(0, 1, length.out = n_curves)))
  sel_indices <- order(werrs)[quants]

  # --- 7. Evaluate smoothed functional curves over time ---
  fitted_curves <- fda::eval.fd(fdtime, wsmooth$fd)
  keywords <- colnames(wsmooth$fd$coefs)[sel_indices]
  fitted_df <- as.data.frame(fitted_curves)[, sel_indices, drop = FALSE]
  colnames(fitted_df) <- keywords
  raw_df <- mat[keywords, , drop = FALSE]

  # --- 8. Create x-axis tick marks every 3 time points ---
  r <- 3
  ny <- diff(range(samplpo))+1
  breaks_idx <- seq(1, ny, by=r)
  breaks_years <- (samplpo[1]+0:(ny-1))[breaks_idx]
  breaks_years_combined <- breaks_years
  breaks_years_combined[-seq(1, by=2, le=length(breaks_idx))] <- ""

  # --- 9. Initialize lists to store individual and clean plots ---
  plot_list <- list()
  plot_list_clean <- list()

  # --- 10. Generate individual plots for each selected keyword ---
  for (i in seq_along(keywords)) {
    k <- keywords[i]
    smooth_vals <- fitted_df[[k]]
    raw_vals <- as.numeric(raw_df[k, ])

    # Extract zone label if requested
    zone_label <- if (show_zone && "zone" %in% colnames(data$tdm)) {
      data$tdm$zone[match(k, data$tdm$keyword)]
    } else {
      NA
    }

    # Compose label to display in plot
    label_core <- paste0(
      "keyword: ", k, if (show_zone) paste0(", zone: ", zone_label) else "",
      "\nRMS: ", format(round(werrs[sel_indices[i]],3), nsmall = 3),
      ", pos: ", quants[i]
    )

    # Create the dataframe for plotting
    df_plot <- tibble::tibble(
      year = fdtime,
      year_label = samplpo,
      raw = raw_vals,
      smooth = smooth_vals
    )

    # Full plot for individual visualization
    base_size <- 11

    p <- ggplot(df_plot, aes(x = year)) +
      geom_line(aes(y = raw), color = "snow4", linetype = "solid", linewidth = .25) +
      geom_line(aes(y = smooth), color = "indianred2", linewidth = .8) +
      scale_x_continuous(breaks = breaks_idx, labels = breaks_years) +
      labs(
        title = paste0("Optimal Smoothing Fit - Spline Penalty ", penalty, " | Order ", order,
                       "\n", label_core),
        x = "year",
        y = "normalized frequency"
      ) +
      theme_minimal(base_size = base_size) +
      theme(
        plot.title = element_text(face = "plain", size = rel(.825)),
        axis.title.x = element_text(size = rel(.85)),
        axis.title.y = element_text(size = rel(.85)),
        axis.text.x = element_text(angle = 45, hjust = 1, size = rel(.8)),
        axis.text.y = element_text(size = rel(.8))
      )


    # Clean version for the combined plot (only keyword and metrics)

    half_line <- base_size/2

    label_core_combined <- paste0(
      "kw: ", k, if (show_zone) paste0(", zone ", zone_label) else "", "\n",
      "Pen", penalty, " m=", order,
      " RMS ", format(round(werrs[sel_indices[i]],3), nsmall = 3), " ", quants[i], "th"
    )

    # Remove existing x scale to avoid conflicts
    p_clean <- p
    # i <- which(sapply(p_clean$scales$scales, function(x) 'x' %in% x$aesthetics))
    # p_clean$scales$scales[[i]] <- NULL

    p_clean <- p_clean +
      labs(title = label_core_combined) +
      scale_x_continuous(breaks = breaks_idx, labels = breaks_years_combined) +
      theme(
        plot.title = element_text(size = rel(.6)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = rel(.675), angle = 45, hjust = 1),
        axis.text.y = element_text(size = rel(.55), angle = 90, margin = margin(r = 0 * half_line/2, l=0, t=0, b=0)),
        plot.margin = unit(c(0.0, 0.0, 0.0, 0.0), "lines")
      )

    # Store plots
    plot_list[[k]] <- p
    plot_list_clean[[k]] <- p_clean

    if (graph) print(p)
  }

  combined_plot <- patchwork::wrap_plots(plot_list_clean, ncol = 3) +
   # patchwork::plot_annotation(
   #    #title = "Optimal Smoothing Fits",
   #    subtitle = paste0("Penalty ", penalty, " | Order ", order)
   #  ) +
    theme(
      plot.margin = unit(c(0., 0., 0., 0.), "lines"))

  if (graph) print(combined_plot)

  # --- 12. Return invisibly the list of plots and the combined layout ---
  invisible(list(
    singleKeywordPlot = plot_list,
    combinedKeywordPlot = combined_plot
  ))
}
