#' Compute and Rank Clustering Internal Validation Criteria (CIVf)
#'
#' This function evaluates a wide set of internal validation indices across multiple
#' clustering solutions (for different numbers of clusters), transforms and standardizes
#' them based on their optimization rules, and computes rankings to support the selection
#' of the optimal number of clusters.
#'
#' @param clustering_set An object of class \code{ccccClusteringSet}, as produced by \code{runClusteringRange()}.
#' @param criteria_set Optional character vector specifying which internal validation criteria to compute.
#'        If \code{NULL} (default), all supported criteria defined in \code{get_opt_civ()} are used.
#' @param min_valid_values Proportion of non-missing values required per criterion (default: 0.75).
#'        Criteria with fewer valid values will be excluded from analysis.
#' @param verbose Logical. If \code{TRUE}, progress messages are printed (default: \code{TRUE}).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{CIV}{Matrix of raw internal validation criterion values (rows = k, columns = criteria).}
#'   \item{CIVtr}{Transformed values, based on optimization rules.}
#'   \item{CIVstd}{Standardized values in the 0-1 range.}
#'   \item{CIVf}{Final ranking matrix (higher rank = better).}
#'   \item{opt_civ}{Named vector indicating the optimization rule for each retained criterion.}
#' }
#'
#'@examples
#'\dontrun{
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
#' }
#' @export
buildCIVf <- function(clustering_set,
                      criteria_set = NULL,
                      min_valid_values = 0.75,
                      verbose = TRUE) {

  # Check input class
  stopifnot(inherits(clustering_set, "ccccClusteringSet"))

  # Extract information from clustering object
  traj <- clustering_set$matrix
  clusters <- clustering_set$clusters
  k_range <- clustering_set$k_range
  n_repeats <- clustering_set$n_repeats

  # Load optimization rules (from internal function)
  opt_civ <- get_opt_civ()

  # Define the set of criteria to compute
  if (is.null(criteria_set)) {
    criteria_set <- names(opt_civ)
  }

  # Initialize matrix to store raw criterion values
  CIV <- matrix(NA, nrow = length(k_range), ncol = length(criteria_set),
                dimnames = list(paste0("k", k_range), criteria_set))

  # Loop through criteria
  for (crit in criteria_set) {
    if (verbose) message("Computing criterion: ", crit)

    values <- numeric(length(k_range))

    # Loop through each k (number of clusters)
    for (i in seq_along(k_range)) {
      k <- k_range[i]
      clusts_k <- clusters[[as.character(k)]]

      # Compute criterion across all repeated clusterings for k
      val_k <- suppressWarnings(
        sapply(clusts_k, function(cl) {
          tryCatch({
            val <- computeCriterion(traj, cl, criterion = crit)
            if (is.infinite(val) || is.nan(val)) NA else val
          }, error = function(e) NA)
        })
      )

      # Aggregate values based on optimization rule
      values[i] <- if (crit %in% c("KL", "Gap")) {
        quantile(val_k, probs = 0.95, na.rm = TRUE)
      } else {
        if (opt_civ[crit] == "max") max(val_k, na.rm = TRUE) else
          if (opt_civ[crit] == "min") min(val_k, na.rm = TRUE) else
            mean(val_k, na.rm = TRUE)  # fallback
      }
    }

    # Store aggregated values for criterion
    CIV[, crit] <- values
  }

  # Filter criteria with sufficient valid values
  valid_counts <- colSums(!is.na(CIV))
  min_valid <- ceiling(min_valid_values * length(k_range))
  criteria_ok <- names(valid_counts[valid_counts >= min_valid])

  CIV <- CIV[, criteria_ok, drop = FALSE]
  opt_civ <- opt_civ[criteria_ok]

  # Transform raw values according to optimization rules
  CIVtr <- CIV
  for (crit in criteria_ok) {
    if (opt_civ[crit] == "min")    CIVtr[, crit] <- -CIV[, crit]
    if (opt_civ[crit] == "maxd2")  CIVtr[, crit] <- c(NA, diff(diff(CIV[, crit])), NA)
    if (opt_civ[crit] == "mind1")  CIVtr[, crit] <- -c(NA, diff(CIV[, crit]))
    if (opt_civ[crit] == "maxd1")  CIVtr[, crit] <-  c(NA, diff(CIV[, crit]))
    if (opt_civ[crit] == "mind2")  CIVtr[, crit] <- -c(NA, diff(diff(CIV[, crit])), NA)
  }

  # Standardize to 0-1 range
  CIVstd <- apply(CIVtr, 2, function(x) {
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
  })

  # Compute rankings (higher = better)
  CIVf <- apply(CIVstd, 2, function(x) rank(-x, ties.method = "min"))

  return(list(
    CIV = CIV,         # Raw values
    CIVtr = CIVtr,     # Transformed values
    CIVstd = CIVstd,   # Standardized values
    CIVf = CIVf,       # Final ranking matrix
    opt_civ = opt_civ  # Optimization rules used
  ))
}
