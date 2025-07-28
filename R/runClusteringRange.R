#' Perform Repeated K-Means Clustering Across a Range of Cluster Numbers
#'
#' This function applies k-means clustering multiple times to the temporal profiles
#' (typically rows of a term-document matrix) over a specified range of cluster numbers.
#' It is designed to evaluate clustering stability and prepare results for subsequent
#' internal validation and comparison.
#'
#' @param data A list object produced by \code{importData()}, containing at least the elements \code{tdm} (term-document matrix) and \code{year_cols} (column indices of time points).
#' @param k_range Integer vector specifying the number of clusters to evaluate (default: 2:26).
#' @param n_repeats Integer. Number of k-means repetitions per value of \code{k} (default: 20).
#' @param seed Integer. Random seed for reproducibility (default: 123).
#' @param dist_method Character. Currently unused, included for future compatibility (default: "euclidean").
#' @param verbose Logical. If TRUE, displays progress messages for each clustering iteration (default: TRUE).
#'
#' @return An object of class \code{"ccccClusteringSet"} containing:
#' \describe{
#'   \item{clusters}{A list of lists, where each sublist contains \code{n_repeats} clustering results for a given \code{k}.}
#'   \item{matrix}{The numeric matrix used for clustering (rows = terms, columns = years).}
#'   \item{k_range}{The vector of evaluated cluster numbers.}
#'   \item{n_repeats}{Number of repetitions per cluster number.}
#'   \item{dist_method}{Distance method (for potential future use).}
#'   \item{keywords}{Character vector of row names (terms) from the TDM.}
#'   \item{tdm_info}{Data frame with metadata about the terms (e.g., frequency and zone).}
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
#' runClusteringRange(data_nchi,
#' k_range = 2:26,
#' n_repeats = 20,
#' seed = 123,
#' dist_method = "euclidean",
#' verbose = TRUE)
#' }
#'
#' @export
runClusteringRange <- function(data,
                               k_range = 2:26,
                               n_repeats = 20,
                               seed = 123,
                               dist_method = "euclidean",
                               verbose = TRUE) {
  set.seed(seed)  # Ensure reproducibility

  # Extract the matrix to cluster (terms × years)
  mat <- data$tdm[, data$year_cols] |> as.matrix()
  rownames(mat) <- data$tdm$keyword

  clustering_results <- list()

  # Loop through each number of clusters
  for (k in k_range) {
    if (verbose) message("Running clustering for k = ", k)

    # Repeat k-means clustering n_repeats times for stability
    k_list <- vector("list", n_repeats)
    for (r in seq_len(n_repeats)) {
      km <- suppressWarnings(stats::kmeans(mat, centers = k, nstart = 10))
      k_list[[r]] <- km$cluster
    }

    clustering_results[[as.character(k)]] <- k_list
  }

  # Prepare the output object
  out <- list(
    clusters = clustering_results,                        # Cluster assignments
    matrix = mat,                                         # Matrix used for clustering
    k_range = k_range,                                    # Range of cluster numbers
    n_repeats = n_repeats,                                # Number of repetitions
    dist_method = dist_method,                            # Distance method (not yet used)
    keywords = data$tdm$keyword,                          # Term names
    tdm_info = data$tdm[, c("keyword", "tot_freq", "int_freq", "zone")]  # Term metadata
  )

  class(out) <- "ccccClusteringSet"
  return(out)
}
