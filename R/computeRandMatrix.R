#' Compute Adjusted Rand Index Matrix for Selected Partitions
#'
#' This function computes the Adjusted Rand Index (ARI) between all pairs of cluster partitions
#' selected in the \code{ccccBestPartitions} object. The ARI measures the similarity between two
#' partitions, correcting for chance.
#'
#' @param best_partitions An object of class \code{ccccBestPartitions}, as returned by \code{selectBestPartitions()}.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{rand_matrix}{A symmetric matrix of pairwise Adjusted Rand Index values.}
#'   \item{partition_labels}{A character vector of partition names used as row/column names.}
#'   \item{partitions}{A named list of all cluster partitions.}
#'   \item{partition_k}{A named vector indicating the number of clusters (\code{k}) for each partition.}
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
#' }
#'
#' @export
computeRandMatrix <- function(best_partitions) {
  stopifnot(inherits(best_partitions, "ccccBestPartitions"))

  part_list <- list()        # stores each selected partition
  part_labels <- c()         # names like "k5_C_index"
  part_k <- c()              # stores number of clusters for each partition

  # Extract partition, label and k for each best partition
  for (entry in best_partitions) {
    k <- entry$k
    label <- paste0("k", k, "_", entry$criterion)
    part_list[[label]] <- entry$partition
    part_labels <- c(part_labels, label)
    part_k <- c(part_k, k)
  }

  names(part_k) <- part_labels

  # Initialize ARI matrix
  n <- length(part_list)
  rand_matrix <- matrix(NA, n, n, dimnames = list(part_labels, part_labels))

  # Compute pairwise Adjusted Rand Index
  for (i in seq_len(n)) {
    for (j in seq(i, n)) {
      ri <- aricode::ARI(part_list[[i]], part_list[[j]])
      rand_matrix[i, j] <- ri
      rand_matrix[j, i] <- ri
    }
  }

  # Return results as a structured object
  out <- list(
    rand_matrix = rand_matrix,
    partition_labels = part_labels,
    partitions = part_list,
    partition_k = part_k
  )
  class(out$rand_matrix) <- "ccccRandMatrix"
  class(out) <- "ccccRandMatrix"
  return(out)
}

