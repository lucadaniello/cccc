#' Select Optimal Number of Clusters Based on Rand Index Stability
#'
#' This function identifies the most stable number of clusters (\code{k}) by computing the
#' mean Adjusted Rand Index (ARI) across all clustering partitions associated with each \code{k}.
#' The configuration with the highest average ARI is selected as the most stable solution.
#'
#' Optionally, a bar chart is displayed to visualize the mean ARI values for each \code{k}.
#'
#' @param rand_matrix An object of class \code{ccccRandMatrix}, returned by \code{computeRandMatrix()}.
#'
#' @return A list with:
#' \describe{
#'   \item{best_k}{The number of clusters with the highest average ARI.}
#'   \item{ari_stats}{A tibble with mean ARI values computed for each \code{k}.}
#' }
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
#' g2 <- buildCIVf(clustering_set=g,
#' criteria_set = NULL,
#' min_valid_values = 0.75,
#' verbose = TRUE)
#'
#' g3 <- selectBestPartitions(g2, g, graph=T)
#' rand_result <- computeRandMatrix(g3)
#'
#' best_k <- selectBestKfromRand(rand_result)
#' }
#'
#' @export
selectBestKfromRand <- function(rand_matrix) {
  stopifnot(inherits(rand_matrix, "ccccRandMatrix"))

  partition_labels <- rand_matrix$partition_labels
  ari_mat <- rand_matrix$rand_matrix

  # Extract the k part from partition labels (e.g., "k4" from "k4_Silhouette")
  k_labels <- substr(partition_labels, 1, regexpr("_", partition_labels) - 1)
  k_unique <- unique(k_labels)

  # Compute mean ARI for each k (only if at least 2 partitions exist)
  ari_stats <- lapply(k_unique, function(k) {
    idx <- which(k_labels == k)
    if (length(idx) < 2) return(NULL)  # At least 2 partitions required for ARI
    submat <- ari_mat[idx, idx]
    mean_ari <- mean(submat[upper.tri(submat)], na.rm = TRUE)
    tibble::tibble(k = as.integer(sub("k", "", k)), mean_ari = mean_ari)
  }) |>
    purrr::compact() |>
    dplyr::bind_rows()

  # Optional plot of ARI by number of clusters
  # if (plot && nrow(ari_stats) > 0) {
  #   ggplot2::ggplot(ari_stats, ggplot2::aes(x = factor(k), y = mean_ari)) +
  #     ggplot2::geom_col(fill = "steelblue") +
  #     ggplot2::labs(title = "Mean Adjusted Rand Index by Number of Clusters",
  #                   x = "k (number of clusters)", y = "Mean ARI") +
  #     ggplot2::theme_minimal()
  # }

  # Select the k with the maximum average ARI
  best_k <- ari_stats$k[which.max(ari_stats$mean_ari)]

  return(list(
    best_k = best_k,
    ari_stats = ari_stats
  ))
}
