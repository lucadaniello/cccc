#' Select Best Partitions Based on Top Ranking Criteria
#'
#' This function selects the best clustering partitions that ranked first (Top-1)
#' in at least one internal validation criterion for each value of \code{k}.
#' For each selected partition, it returns the optimal repetition that maximizes
#' (or minimizes) the associated criterion value, depending on the optimization rule.
#'
#' Optionally, it displays a bar plot showing how frequently each number of clusters
#' appears among the top-ranked solutions.
#'
#' @param civf_list A list returned by \code{buildCIVf()}, containing validation matrices and optimization rules.
#' @param clustering_set A list returned by \code{runClusteringRange()}, containing repeated clustering results.
#' @param graph Logical. If \code{TRUE}, shows a bar chart of top-k frequencies (default: \code{FALSE}).
#'
#' @return A named list of selected partitions, each containing:
#' \describe{
#'   \item{k}{The number of clusters.}
#'   \item{criterion}{The criterion by which the partition was selected.}
#'   \item{index}{The index of the selected repetition.}
#'   \item{partition}{An integer vector representing the cluster assignment.}
#' }
#' The list has class \code{"ccccBestPartitions"}.
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
#' }
#'
#' @export
selectBestPartitions <- function(civf_list, clustering_set, graph = FALSE) {
  CIVf <- civf_list$CIVf
  clusters <- clustering_set$clusters
  k_range <- clustering_set$k_range
  n_repeats <- clustering_set$n_repeats

  selected <- list()

  # Loop over each value of k
  for (k in k_range) {
    k_label <- paste0("k", k)

    # Identify criteria for which this k is ranked 1
    top_criteria <- names(which(CIVf[k_label, ] == 1))

    if (length(top_criteria) > 0) {
      for (crit in top_criteria) {
        # Compute the criterion values for all repetitions
        val_list <- sapply(clusters[[as.character(k)]], function(cl) {
          tryCatch(
            computeCriterion(clustering_set$matrix, cl, criterion = crit),
            error = function(e) NA
          )
        })

        if (all(is.na(val_list))) next  # Skip if no valid values

        opt_rule <- civf_list$opt_civ[[crit]]

        # Determine optimal repetition (min or max)
        best_idx <- if (opt_rule == "min") {
          which.min(val_list)
        } else {
          which.max(val_list)
        }

        # Store selected partition with metadata
        if (!is.na(best_idx) && length(best_idx) > 0) {
          selected[[paste0("k", k, "_", crit)]] <- list(
            k = k,
            criterion = crit,
            index = best_idx,
            partition = clusters[[as.character(k)]][[best_idx]]
          )
        }
      }
    }
  }

  class(selected) <- "ccccBestPartitions"

  # Optional visualization of top-k frequencies
  if (graph) {
    rank_mat <- CIVf
    top_n <- min(4, ncol(rank_mat))  # Max top-N ranks to include

    # Count how often each k appears in Top-1, Top-2, ..., Top-n
    top_counts <- matrix(0, nrow = nrow(rank_mat), ncol = top_n)
    rownames(top_counts) <- rownames(rank_mat)
    colnames(top_counts) <- paste0("Top-", 1:top_n)

    for (i in seq_len(top_n)) {
      top_counts[, i] <- rowSums(rank_mat == i, na.rm = TRUE)
    }

    # Convert to long format for plotting
    top_counts_df <- as.data.frame(top_counts)
    top_counts_df$k <- as.numeric(gsub("k", "", rownames(top_counts)))
    top_counts_long <- tidyr::pivot_longer(top_counts_df, cols = starts_with("Top-"),
                                           names_to = "rank", values_to = "count")

    # Plot the bar chart
    print(
      ggplot(top_counts_long, aes(x = k, y = count, fill = rank)) +
        geom_bar(stat = "identity") +
        labs(
          title = "Frequencies of cluster numbers in top rankings",
          x = "Number of clusters (k)", y = "Frequency among top criteria"
        ) +
        scale_fill_brewer(palette = "Greens") +
        theme_minimal()
    )
  }

  return(selected)
}
