#' Plot Smoothed Keyword Trajectories by Cluster
#'
#' This function visualizes smoothed frequency trajectories for each cluster
#' over time, based on a functional data object produced by `computeClusterFd()`.
#' For each cluster, the plot includes:Transparent lines for each keyword's
#' smoothed frequency curve, and a bold line representing
#' the Rand-index-weighted mean trajectory.
#'
#' @param fd_obj A list object returned by \code{computeClusterFd()}, containing the `fd` object and attributes.
#' @param nx Integer. Number of interpolation points along the time axis (default = 100).
#'
#' @return A named list of `ggplot` objects, one for each cluster.
#'
#' @examples
#' \dontrun{
#' tdm <- system.file("extdata", "tdm.csv", package = "cccc")
#' corpus <- system.file("extdata", "corpus.csv", package = "cccc")
#' data <- importData(tdm_file = tdm, corpus_file = corpus,
#' sep_tdm = ";",sep_corpus_info = ";",zone="stat")
#'
#' data_nchi <- normalization(data, normty = "nchi", sc = 1000)
#' g <- runClusteringRange(data_nchi,
#' k_range = 2:26,
#' n_repeats = 20,
#' seed = 123,
#' dist_method = "euclidean",
#' verbose = TRUE)
#'
#' results_m2 <- smoothingSelection(data_nchi,
#' penalty_type = "m-2", plot = FALSE)
#' results_2  <- smoothingSelection(data_nchi,
#' penalty_type = "2", plot = FALSE)
#' results_1  <- smoothingSelection(data_nchi,
#' penalty_type = "1", plot = FALSE)
#' results_0  <- smoothingSelection(data_nchi,
#' penalty_type = "0", plot = FALSE)
#'
#' Compare and select best smoothing strategy
#' opt_res <- optimalSmoothing(list("m-2" = results_m2,
#' "2" = results_2,
#' "1" = results_1, "0" = results_0))
#'
#' g2 <- buildCIVf(clustering_set=g,
#' criteria_set = NULL,
#' min_valid_values = 0.75,
#' verbose = TRUE)
#'
#' g3 <- selectBestPartitions(g2, g, graph=T)
#' rand_result <- computeRandMatrix(g3)
#'
#' best_k <- selectBestKfromRand(rand_result)
#'
#' final_part <- extractBestPartition(g3, best_k$best_k,
#' rand_matrix=rand_result)
#'
#' #' Compute Smoothed Cluster Trajectories and Summary Information
#' fd_obj <- computeClusterFd(data = data_nchi, final_part = final_part,
#' opt = opt_res)
#'
#' Plot Cluster Curves from Functional Data Object
#' plots <- plotClusterCurvesList(fd_obj,
#' nx = 100)
#'}
#'
#' @export
plotClusterCurvesList <- function(fd_obj,
                                  nx = 100) {

  # Extract fd object and attributes
  fd <- fd_obj$fd_obj
  clusters <- attr(fd, "partition")
  rand_vec <- attr(fd, "rand_vec")
  traj_mean_rand <- attr(fd, "traj_mean_rand")
  term_names <- fd$fdnames$reps
  years <- fd$fdnames$time
  rand_index <- attr(fd, "rand_index")

  # Sanity check
  stopifnot(length(clusters) == length(term_names))

  # Interpolation points for evaluation
  eval_time <- seq(1, length(years), length.out = nx)

  # Evaluate functional data at interpolated time points
  smooth_vals <- fda::eval.fd(eval_time, fd)
  smooth_df <- as.data.frame(smooth_vals)
  colnames(smooth_df) <- term_names
  smooth_df$time <- eval_time

  # Reshape to long format and assign clusters
  long_df <- tidyr::pivot_longer(smooth_df, cols = -time, names_to = "id", values_to = "freq") %>%
    dplyr::mutate(cluster = factor(clusters[match(id, term_names)]))

  # Set light theme and color palette
  base_theme <- ggplot2::theme_minimal(base_size = 13)
  palette <- RColorBrewer::brewer.pal(max(3, nlevels(long_df$cluster)), "Set1")

  # Initialize list of plots
  cluster_levels <- levels(long_df$cluster)
  results <- vector("list", length(cluster_levels))
  names(results) <- cluster_levels

  for (i in seq_along(cluster_levels)) {
    cl <- cluster_levels[i]
    dat_cl <- dplyr::filter(long_df, cluster == cl)

    # Extract mean Rand-weighted curve
    rand_curve <- data.frame(time = eval_time, freq = traj_mean_rand[i, ])

    # Cluster info for title
    n_words <- length(unique(dat_cl$id))
    perc <- round(n_words / length(term_names) * 100, 1)
    rand_val <- round(rand_index[i], 2)

    # Build the ggplot
    p <- ggplot2::ggplot(dat_cl, ggplot2::aes(x = time, y = freq, group = id)) +
      ggplot2::geom_line(color = palette[i], alpha = 0.1, linewidth = 0.4) +
      ggplot2::geom_line(data = rand_curve, ggplot2::aes(x = time, y = freq),
                         color = palette[i], linewidth = 1.2, inherit.aes = FALSE) +
      ggplot2::scale_x_continuous(breaks = seq_along(years), labels = years) +
      ggplot2::labs(
        title = paste0("Cluster ", cl, " (", n_words, " terms, ", perc, "%)"),
        subtitle = paste0("Rand Index: ", rand_val),
        x = "Year", y = "Frequency"
      ) +
      base_theme +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )

    results[[cl]] <- p
  }

  return(results)
}

