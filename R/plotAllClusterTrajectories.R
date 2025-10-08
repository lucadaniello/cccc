#' Plot All Cluster Trajectories with Colored Legend
#'
#' This function generates a combined visualization of all smoothed keyword trajectories
#' grouped by cluster, along with a bottom legend showing the number and percentage of
#' terms in each cluster. Each cluster is represented with a distinct color and a letter
#' identifier (A, B, C, ...).
#'
#' @param fd_obj A functional data object returned by \code{computeClusterFd()}.
#' @param partition_id Integer (optional). The ID of the selected partition to display in the title.
#' @param nx Integer. Number of interpolation points used for smoothing (default = 100).
#'
#' @return A patchwork \code{ggplot} object combining the cluster trajectories and bottom legend.
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
#' Plot Cluster Trajectories
#' plots2 <- plotAllClusterTrajectories(fd_obj,
#' partition_id = NULL,
#' nx = 100)
#'}
#'
#'
#' @export
plotAllClusterTrajectories <- function(fd_obj,
                                       partition_id = NULL,
                                       nx = 100) {

  # Extract the fd object from input
  fd_obj <- fd_obj$fd_obj
  clusters <- factor(attr(fd_obj, "partition"))
  traj_mean_rand <- attr(fd_obj, "traj_mean_rand")
  years <- fd_obj$fdnames$time
  terms <- fd_obj$fdnames$reps

  # If not provided, get the partition ID from attributes
  if (is.null(partition_id)) {
    partition_id <- attr(fd_obj, "partition_id")
  }

  # Evaluate the functional data on a fine time grid
  eval_time <- seq(1, length(years), length.out = nx)
  smooth_vals <- fda::eval.fd(eval_time, fd_obj)
  smooth_df <- as.data.frame(smooth_vals)
  colnames(smooth_df) <- terms
  smooth_df$time <- eval_time

  # Convert to long format and match each term to its cluster and cluster letter
  long_df <- tidyr::pivot_longer(smooth_df, cols = -time, names_to = "id", values_to = "freq") %>%
    dplyr::mutate(
      cluster_num = clusters[match(id, terms)],
      letter = LETTERS[as.integer(cluster_num)]
    )

  # Define cluster IDs and assign colors
  cluster_ids <- levels(clusters)
  palette <- RColorBrewer::brewer.pal(max(3, length(cluster_ids)), "Set1")
  names(palette) <- LETTERS[seq_along(cluster_ids)]

  # Build data frame for mean trajectories
  mean_list <- lapply(seq_along(cluster_ids), function(i) {
    tibble::tibble(
      cluster = cluster_ids[i],
      letter = LETTERS[i],
      time = eval_time,
      freq = traj_mean_rand[i, ]
    )
  }) %>% dplyr::bind_rows()

  # Build data for legend labels
  cluster_stats <- as.data.frame(table(clusters)) %>%
    dplyr::mutate(
      letter = LETTERS[seq_len(n())],
      color = palette[letter],
      perc = round(100 * Freq / sum(Freq), 1),
      label = paste0(
        "<span style='color:", color, ";'><b>", letter, "</b></span> ",
        Freq, " (", perc, "%)"
      )
    )

  # Title
  title_main <- paste0(length(cluster_ids), " clusters - partition ", partition_id)

  # Plot: all term curves (transparent) and cluster mean (bold)
  main_plot <- ggplot2::ggplot() +
    ggplot2::geom_line(data = long_df,
                       aes(x = time, y = freq, group = id, color = letter),
                       alpha = 0.1, linewidth = 0.3) +
    ggplot2::geom_line(data = mean_list,
                       aes(x = time, y = freq, group = letter, color = letter),
                       linewidth = 1.2) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_x_continuous(breaks = seq_along(years), labels = years) +
    ggplot2::labs(x = NULL, y = NULL, title = title_main) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0)
    )

  legend_text <- paste(cluster_stats$letter,
                       paste0(cluster_stats$Freq, " (", cluster_stats$perc, "%)"),
                       sep = " ", collapse = "   -   ")


  legend_plot <- ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = legend_text,
                      size = 4.2, hjust = 0.5, vjust = 0.5, family = "sans") +
    ggplot2::theme_void()

  # Combine main plot and legend using patchwork
  combined <- main_plot / legend_plot +
    patchwork::plot_layout(heights = c(0.94, 0.06))

  return(combined)
}
