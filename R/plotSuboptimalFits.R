#' Plot Suboptimal Smoothed Curves for Selected Keywords
#'
#' This function visualizes raw and smoothed time trends of keyword frequencies based on smoothing spline fits.
#' A subset of keywords is selected based on the residual Root Mean Square (RMS) error of the smoothed fit.
#' Both individual and combined plots are returned.
#'
#' @param data A list containing the outputs returned by [importData()]
#' @param opt_res A list with the optimal smoothing configuration returned by [optimalSmoothing()]:
#' @param n_curves Integer. Number of keywords to display, chosen across the RMS distribution (default is 9).
#' @param show_zone Logical. If \code{TRUE}, include the frequency zone (if available in \code{tdm}) in the plot labels.
#' @param graph Logical. If \code{TRUE}, print all individual and combined plots to the active device (default is \code{FALSE}).
#'
#' @return A named list with the following elements (returned invisibly):
#' \itemize{
#'   \item \code{singleKeywordPlot}: A list of ggplot objects, one for each selected keyword (full title included).
#'   \item \code{combinedKeywordPlot}: A combined \code{patchwork} plot showing all selected fits.
#' }
#'
#' @details
#' The function first extracts the smoothed spline fit based on the optimal parameters.
#' It then computes the residual RMS for each keyword and selects a subset using quantiles.
#' Each keyword's frequency curve (raw and smoothed) is plotted, either individually or in a combined layout using the \code{patchwork} package.
#'
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
#'
#'   result <- plotSuboptimalFits(data = data_nchi, opt_res = opt, graph = TRUE)
#' }
#'
#' @export


plotSuboptimalFits <- function(data,
                               opt_res,
                               n_curves = 9,
                               show_zone = FALSE,
                               graph = FALSE) {

  # --- 1. Input validation ---
  if (missing(data) || is.null(data$tdm) || is.null(data$year_cols)) {
    message("Argument 'data' is missing or incomplete. It must contain 'tdm' and 'year_cols'.")
  }
  if (missing(opt_res) || is.null(opt_res$m_opt) || is.null(opt_res$penalty_opt)) {
    message("Argument 'opt_res' is missing or incomplete. It must include 'm_opt' and 'penalty_opt'.")
  }

  # --- 2. Compute smoothed functional object using optimal settings ---
  wsmooth <- getWsmooth(data, opt_res)
  penalty <- wsmooth$penalty
  degree <- wsmooth$degree

  # --- 3. Prepare the matrix of keyword frequencies ---
  mat <- data$tdm %>%
    dplyr::select(dplyr::any_of(data$year_cols)) %>%
    as.matrix()
  rownames(mat) <- data$tdm$keyword
  mat <- mat[rowSums(mat) > 0, , drop = FALSE]  # Remove keywords with zero frequency across all years

  # --- 4. Define time information and matrix size ---
  years_real <- names(data$tdm[data$year_cols])
  fdtime <- seq_along(data$year_cols)
  n <- nrow(mat)
  nfine <- ncol(mat)

  # --- 5. Compute RMS residuals from smoothing results ---
  df <- wsmooth$df
  gcv <- wsmooth$gcv
  errs <- ((length(fdtime) - df)^2 * gcv) / length(fdtime)
  werrs <- round(sqrt(errs / nfine), 3)

  # --- 6. Select 'n_curves' keywords based on RMS quantiles ---
  quants <- round(quantile(1:n, probs = seq(0, 1, length.out = n_curves + 1)))
  sel_indices <- order(werrs)[quants]

  # --- 7. Evaluate smoothed functional curves over time ---
  fitted_curves <- fda::eval.fd(fdtime, wsmooth$fd)
  keywords <- colnames(wsmooth$fd$coefs)[sel_indices]
  fitted_df <- as.data.frame(fitted_curves)[, sel_indices, drop = FALSE]
  colnames(fitted_df) <- keywords
  raw_df <- mat[keywords, , drop = FALSE]

  # --- 8. Create x-axis tick marks every 3 time points ---
  step <- 3
  breaks_idx <- seq(1, length(years_real), by = step)
  breaks_years <- years_real[breaks_idx]

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
      "Keyword: ", k,
      "\nRMS: ", format(werrs[sel_indices[i]], nsmall = 3),
      ", Pos: ", sel_indices[i],
      if (show_zone) paste0(", Zone: ", zone_label) else ""
    )

    # Create the dataframe for plotting
    df_plot <- tibble::tibble(
      year = fdtime,
      year_label = years_real,
      raw = raw_vals,
      smooth = smooth_vals
    )

    # Full plot for individual visualization
    p <- ggplot(df_plot, aes(x = year)) +
      geom_line(aes(y = raw), color = "grey70", linetype = "dashed") +
      geom_line(aes(y = smooth), color = "indianred2", linewidth = 1) +
      scale_x_continuous(breaks = breaks_idx, labels = breaks_years) +
      labs(
        title = paste0("Suboptimal Smoothing Fits by RMS Residual",
                       "\nPenalty: ", penalty, ", Degree: ", degree,
                       "\n", label_core),
        x = "Year",
        y = "Frequency"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title = element_text(face = "plain", size = 9),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 9)
      )

    # Clean version for the combined plot (only keyword and metrics)
    p_clean <- p +
      labs(title = label_core) +
      theme(
        plot.title = element_text(size = 8, face = "italic"),
        axis.title = element_text(size = 9),
        axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8)
      )

    # Store plots
    plot_list[[k]] <- p
    plot_list_clean[[k]] <- p_clean

    if (graph) print(p)
  }

  combined_plot <- patchwork::wrap_plots(plot_list_clean, ncol = 3) +
    patchwork::plot_annotation(
      title = "Suboptimal Smoothing Fits by RMS Residual",
      subtitle = paste0("Penalty: ", penalty, " | Degree: ", degree)
    )

  if (graph) print(combined_plot)

  # --- 12. Return invisibly the list of plots and the combined layout ---
  invisible(list(
    singleKeywordPlot = plot_list,
    combinedKeywordPlot = combined_plot
  ))
}
