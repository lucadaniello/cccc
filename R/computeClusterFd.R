#' Compute Smoothed Cluster Trajectories and Summary Information
#'
#' This function computes a smoothed functional data object (`fd`) from the temporal keyword matrix,
#' using a B-spline basis and optimal smoothing parameters.
#' It also calculates within-cluster similarity measures (Rand vector), cluster-wise mean trajectories,
#' and returns all required information for downstream visualization and analysis.
#'
#' @param data A normalized data object, output of the \code{normalization()} function.
#' @param final_part A list returned by \code{extractBestPartition()}, containing the best partition and its index.
#' @param opt A list returned by \code{optimalSmoothing()}, containing optimal parameters: degree, penalty type, and lambda.
#' @param nx Integer. Number of interpolation points for evaluating smooth curves (default = 100).
#'
#' @return A list containing:
#' \describe{
#'   \item{fd_obj}{Functional data object (`fd`) with relevant cluster-based attributes.}
#'   \item{cluster_distribution}{Data frame with cluster sizes and relative percentages.}
#'   \item{partition}{Integer vector with cluster assignment for each keyword.}
#'   \item{rand_vec}{Numeric vector with similarity-to-center for each keyword curve.}
#'   \item{keywords}{Character vector with keyword names.}
#'   \item{traj_mean}{Matrix of unweighted cluster mean curves (rows = clusters, cols = time points).}
#'   \item{traj_mean_rand}{Matrix of weighted cluster mean curves using the Rand vector.}
#'   \item{rand_index}{Vector of average Rand Index for each cluster.}
#'   \item{years}{Character vector of time points (years).}
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
#' results_m2 <- smoothingSelection(data_nchi, penalty_type = "m-2", plot = FALSE)
#' results_2  <- smoothingSelection(data_nchi, penalty_type = "2", plot = FALSE)
#' results_1  <- smoothingSelection(data_nchi, penalty_type = "1", plot = FALSE)
#' results_0  <- smoothingSelection(data_nchi, penalty_type = "0", plot = FALSE)
#'
#' Compare and select best smoothing strategy
#' opt_res <- optimalSmoothing(list("m-2" = results_m2, "2" = results_2,
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
#' final_part <- extractBestPartition(g3, best_k$best_k, rand_matrix=rand_result)
#'
#' #' Compute Smoothed Cluster Trajectories and Summary Information
#' fd_obj <- computeClusterFd(data = data_nchi, final_part = final_part, opt = opt_res)
#' }
#'
#'
#' @export
computeClusterFd <- function(data, final_part, opt, nx = 100) {
  # Extract partition and its ID
  partition <- final_part$partition
  partition_id <- final_part$index

  # Extract optimal smoothing parameters
  m_opt <- opt$m_opt
  penalty_opt <- opt$penalty_opt
  lambda_opt <- opt$lambda_opt

  # Build data matrix and ensure rows with non-zero frequency only
  mat <- data$tdm %>%
    dplyr::select(dplyr::any_of(data$year_cols)) %>%
    as.matrix()
  rownames(mat) <- data$tdm$keyword
  mat <- mat[rowSums(mat) > 0, , drop = FALSE]

  # Create B-spline basis over time
  fdtime <- seq_len(ncol(mat))
  basis <- fda::create.bspline.basis(breaks = fdtime, norder = m_opt)

  # Define penalty order based on selected type
  lfd <- switch(penalty_opt,
                "m-2" = max(0, m_opt - 2),
                "2"   = if (m_opt > 3) 2 else if (m_opt == 3) 1 else 0,
                "1"   = if (m_opt > 2) 1 else 0,
                "0"   = 0,
                stop("Invalid penalty_opt: must be one of 'm-2', '2', '1', '0'"))

  # Smooth data using functional data smoothing
  fdpar <- fda::fdPar(basis, Lfdobj = lfd, lambda = 10^lambda_opt)
  smoothed <- fda::smooth.basis(argvals = fdtime, y = t(mat), fdParobj = fdpar)
  fd_obj <- smoothed$fd

  # Evaluate smooth curves and compute similarity-to-center (Rand vector)
  smooth_vals <- t(fda::eval.fd(fdtime, fd_obj))
  cluster_levels <- sort(unique(partition))
  rand_vec <- numeric(length(partition))
  for (cl in cluster_levels) {
    idx <- which(partition == cl)
    center <- colMeans(smooth_vals[idx, , drop = FALSE])
    rand_vec[idx] <- apply(smooth_vals[idx, , drop = FALSE], 1, function(x) cor(x, center, use = "complete.obs"))
  }

  # Interpolate smooth curves over a finer grid
  eval_time <- seq(fd_obj$basis$rangeval[1], fd_obj$basis$rangeval[2], length.out = nx)
  smooth_interp <- t(fda::eval.fd(eval_time, fd_obj))

  # Compute cluster-wise mean curves (weighted and unweighted)
  traj_mean <- traj_mean_rand <- matrix(NA, nrow = length(cluster_levels), ncol = nx)
  for (i in seq_along(cluster_levels)) {
    idx <- which(partition == cluster_levels[i])
    traj_mean[i, ] <- colMeans(smooth_interp[idx, , drop = FALSE])
    weights <- rand_vec[idx]
    if (sum(weights, na.rm = TRUE) > 0) {
      weights <- weights / sum(weights, na.rm = TRUE)
      traj_mean_rand[i, ] <- colSums(sweep(smooth_interp[idx, , drop = FALSE], 1, weights, `*`))
    } else {
      traj_mean_rand[i, ] <- traj_mean[i, ]
    }
  }

  # Attach key attributes to fd object
  attr(fd_obj, "partition") <- partition
  attr(fd_obj, "partition_id") <- partition_id

  # Create cluster distribution summary
  cluster_distribution <- data.frame(
    cluster = sort(unique(partition)),
    count = as.numeric(table(partition)),
    percent = round(100 * as.numeric(table(partition)) / length(partition), 1)
  )
  attr(fd_obj, "cluster_distribution") <- cluster_distribution

  # Attach keyword and zone metadata
  attr(fd_obj, "keywords") <- rownames(mat)
  attr(fd_obj, "zone") <- data$tdm$zone[match(rownames(mat), data$tdm$keyword)]

  # Attach cluster-specific metrics
  attr(fd_obj, "rand_vec") <- rand_vec
  attr(fd_obj, "traj_mean") <- traj_mean
  attr(fd_obj, "traj_mean_rand") <- traj_mean_rand
  attr(fd_obj, "rand_index") <- sapply(cluster_levels, function(cl) {
    idx <- which(partition == cl)
    mean(rand_vec[idx], na.rm = TRUE)
  })

  # Return final list object
  return(list(
    fd_obj = fd_obj,
    cluster_distribution = cluster_distribution,
    partition = partition,
    rand_vec = rand_vec,
    keywords = rownames(mat),
    traj_mean = traj_mean,
    traj_mean_rand = traj_mean_rand,
    rand_index = sapply(cluster_levels, function(cl) {
      idx <- which(partition == cl)
      mean(rand_vec[idx], na.rm = TRUE)
    }),
    years = colnames(data$tdm[, data$year_cols, drop = FALSE])
  ))
}
