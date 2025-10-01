#' Extract the Best Partition for a Given Number of Clusters
#'
#' This function selects the best partition from a set of candidate solutions
#' corresponding to a given number of clusters (\code{k}). If multiple partitions
#' are available for the same \code{k}, and a Rand matrix is provided, the partition
#' with the highest average Adjusted Rand Index (ARI) is selected as the most stable.
#'
#' @param best_partitions An object of class \code{ccccBestPartitions}, returned by \code{selectBestPartitions()}.
#' @param best_k Integer. The selected number of clusters (\code{k}).
#' @param rand_matrix Optional. An object of class \code{ccccRandMatrix} returned by \code{computeRandMatrix()}.
#'                    If provided, the function will select the partition with the highest mean ARI.
#'
#' @return A list containing:
#' \describe{
#'   \item{partition}{Integer vector of cluster assignments.}
#'   \item{criterion}{The internal criterion used for selecting this partition.}
#'   \item{index}{Index of the selected partition among repetitions.}
#'   \item{k}{The number of clusters.}
#'   \item{label}{Unique label for the partition (e.g., "k4_Silhouette").}
#'   \item{mean_ari}{Mean Adjusted Rand Index with other partitions for the same \code{k} (if available).}
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
#'
#' final_part <- extractBestPartition(g3, best_k$best_k, rand_matrix=rand_result)
#' }
#'
#' @export
extractBestPartition <- function(best_partitions, best_k, rand_matrix = NULL) {
  stopifnot(inherits(best_partitions, "ccccBestPartitions"))

  # Filter partitions that match the selected number of clusters (k)
  matches <- Filter(function(x) as.integer(x$k) == as.integer(best_k), best_partitions)

  if (length(matches) == 0) {
    stop("No partition found for the selected k.")
  }

  # If Rand matrix is provided, use ARI to select the most stable partition
  if (!is.null(rand_matrix)) {
    stopifnot(inherits(rand_matrix, "ccccRandMatrix"))

    partition_labels <- rand_matrix$partition_labels
    ari_mat <- rand_matrix$rand_matrix

    # Extract labels and calculate mean ARI for each candidate
    match_labels <- names(matches)
    ari_values <- sapply(match_labels, function(label) {
      idx <- which(partition_labels == label)
      if (length(idx) == 0) return(NA)
      submat <- ari_mat[idx, match_labels, drop = FALSE]
      mean(submat[1, -idx], na.rm = TRUE)
    })

    # Select the partition with the highest mean ARI
    best_idx <- which.max(ari_values)
    best <- matches[[best_idx]]
    mean_ari <- ari_values[best_idx]
    label <- names(matches)[best_idx]
  } else {
    # Default to the first match if no ARI matrix is provided
    best <- matches[[1]]
    mean_ari <- NA
    label <- names(matches)[1]
  }

  # Clean partition names
  names(best$partition) <- NULL

  return(list(
    partition = best$partition,
    criterion = best$criterion,
    index = best$index,
    k = best$k,
    label = label,
    mean_ari = mean_ari
  ))
}
