#' Extract Top Keywords per Cluster by Rand Index
#'
#' This function selects, for each cluster, the top keywords based on their individual
#' Rand Index scores, which reflect similarity to the cluster center trajectory.
#'
#' @param fd_obj A list object returned by \code{computeClusterFd()}, containing the functional data object,
#'        partition, keyword labels, and Rand Index scores.
#' @param percent_words Numeric. The percentage of keywords to retain for each cluster (default = 50).
#'
#' @return A named list of data frames, one per cluster, each containing:
#' \describe{
#'   \item{term}{Keyword label.}
#'   \item{zone}{Frequency zone of the keyword.}
#'   \item{cluster}{Cluster number.}
#'   \item{rand_index}{Rand Index value (rounded).}
#' }
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
#' "1" = results_1,
#' "0" = results_0))
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
#' fd_obj <- computeClusterFd(data = data_nchi, final_part =
#' final_part, opt = opt_res)
#'
#' Extract Top Keywords per Cluster by Rand Index
#' top_keywords <- extractTopClusterWords(fd_obj, percent_words = 50)
#' }
#'
#' @export
extractTopClusterWords <- function(fd_obj, percent_words = 50) {
  # Extract metadata
  partition <- fd_obj$partition
  rand_vec <- fd_obj$rand_vec
  keywords <- fd_obj$keywords
  zones <- attr(fd_obj$fd_obj, "zone")
  clusters <- sort(unique(partition))

  result <- list()

  for (cl in clusters) {
    # Get indices for current cluster
    idx <- which(partition == cl)
    if (length(idx) == 0) next

    # Determine how many top keywords to retain
    top_n <- ceiling(length(idx) * percent_words / 100)

    # Select top keywords by descending Rand Index
    selected <- idx[order(rand_vec[idx], decreasing = TRUE)][1:top_n]

    # Store as data frame
    result[[as.character(cl)]] <- data.frame(
      term = keywords[selected],
      zone = zones[selected],
      cluster = cl,
      rand_index = round(rand_vec[selected], 3),
      stringsAsFactors = FALSE
    )
  }

  return(result)
}
