#' Selection of Optimal Smoothing Parameters for Chronological Keyword Curves
#'
#' Computes optimal smoothing parameters for term frequency curves over time using B-spline basis
#' functions and penalized smoothing. The function evaluates combinations of spline degrees (`m`)
#' and smoothing parameters (`lambda`) under different penalty types, computing diagnostic criteria
#' such as degrees of freedom (`df`), sum of squared errors (`sse`), generalized cross-validation
#' (`gcv`), and ordinary cross-validation (`ocv`).
#'
#' The function returns a complete set of results, including a table of diagnostics, optimal
#' parameters for each degree, and optional visualizations for interpretation.
#'
#' @param data A list returned by [importData()], containing the term-document matrix, corpus
#'   information, and year columns.
#' @param lambda_seq Numeric vector of log10(lambda) values to evaluate. Defaults to `seq(-6, 9, 0.25)`.
#' @param degrees Integer vector of B-spline degrees (`m`) to test. Defaults to `1:8`.
#' @param penalty_type Character string indicating the type of penalization. Accepted values are:
#'   \itemize{
#'     \item `"m-2"`: penalize the (m-2)-th derivative (default);
#'     \item `"2"`: second derivative if `m > 3`, else reduced;
#'     \item `"1"`: first derivative if `m > 2`, else none;
#'     \item `"0"`: no penalization.
#'   }
#' @param normty Optional character string indicating the normalization type applied to the TDM
#'   (used only for labeling results).
#' @param plot Logical. If `TRUE` (default), returns plots for each diagnostic criterion.
#' @param verbose Logical. If `TRUE`, prints progress messages during the selection process.
#'
#' @return A named list with the following components:
#' \describe{
#'   \item{results}{A data frame containing the full grid of (m, lambda) combinations with associated diagnostics.}
#'   \item{summary_optimal}{A summary table of optimal GCV and OCV results per degree.}
#'   \item{optimal_gcv}{Subset of `results` containing the lambda value that minimizes GCV for each degree.}
#'   \item{optimal_ocv}{Subset of `results` containing the lambda value that minimizes OCV for each degree.}
#'   \item{plots}{A list of `ggplot` objects for `df`, `sse`, `gcv`, and `ocv` (only if `plot = TRUE`).}
#'   \item{summary_panel}{A graphical summary (grob object) of optimal smoothing for visual reporting.}
#'   \item{penalty_type}{Character string of the penalization type used.}
#'   \item{call}{The matched call for reproducibility.}
#' }
#'
#' @seealso [optimalSmoothing()], [plotOptimalSmoothing()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load term-document matrix and corpus metadata
#' data <- importData("tdm.csv", "corpus.csv")
#'
#' # Run smoothing selection using default settings
#' result <- smoothingSelection(data, penalty_type = "m-2")
#'
#' # View optimal summary
#' result$summary_optimal
#'
#' # Plot GCV curves
#' result$plots$gcv
#' }


smoothingSelection <- function(data,
                               lambda_seq = NULL,
                               degrees = NULL,
                               penalty_type = "m-2",
                               normty = NULL,
                               plot = TRUE,
                               verbose = TRUE) {

  # Determine normalization type to display in output (optional)
  if (is.null(normty)){
    normty <- data$normty
  }else{
    normty <- ""
  }

  # Set default range for log(lambda) and spline degrees
  if (is.null(lambda_seq)) lambda_seq <- seq(-6, 9, by = 0.25)
  if (is.null(degrees)) degrees <- 1:8

  log_lambda <- lambda_seq
  lambdas <- 10^log_lambda

  # Extract time (years) and term-document matrix (TDM)
  year_vec <- data$corpus_info %>% dplyr::pull(years)
  year_ind <- data$year_cols
  fdtime <- seq_along(year_vec)

  tdm_wide <- data$tdm %>%
    dplyr::select(keyword, tidyselect::any_of(year_ind)) %>%
    tibble::column_to_rownames("keyword") %>%
    as.matrix()

  # Transpose matrix for use in smoothing function
  tdm_wideTranspost <- t(tdm_wide)
  results <- list()

  # Loop over spline degrees to compute smoothing statistics
  for (m in degrees) {
    if (verbose) message("Processing degree m = ", m)

    # Create B-spline basis for current degree
    basis <- fda::create.bspline.basis(breaks = fdtime, norder = m)
    bvals <- fda::eval.basis(fdtime, basis)

    # Select penalty order based on penalty_type
    lfd <- switch(penalty_type,
                  "m-2" = max(0, m - 2),
                  "2"   = if (m > 3) 2 else if (m == 3) 1 else 0,
                  "1"   = if (m > 2) 1 else 0,
                  "0"   = 0,
                  stop("Unknown penalty_type. Accepted values are 'm-2', '2', '1', '0'.")
    )

    # Loop over lambda values and compute smoothing statistics
    stats_list <- purrr::map_dfr(seq_along(lambdas), function(i) {
      lambda <- lambdas[i]
      loglam <- log_lambda[i]
      fdpar <- fda::fdPar(basis, lfd, lambda)
      sm <- fda::smooth.basis(argvals = fdtime, y = tdm_wideTranspost, fdParobj = fdpar)

      # Compute hat matrix and OCV (Ordinary Cross-Validation)
      hatmat <- bvals %*% sm$y2cMap
      ocv_vals <- apply((tdm_wideTranspost - fda::eval.fd(fdtime, sm$fd))^2 / (1 - diag(hatmat))^2, 2, mean)

      # Store results in tidy tibble
      tibble::tibble(
        degree = m,
        log_lambda = loglam,
        lambda = lambda,
        df = sm$df,
        sse = sm$SSE,
        gcv = mean(sm$gcv),
        ocv = mean(ocv_vals),
        normty = normty
      )
    })

    # Save results per degree
    results[[as.character(m)]] <- stats_list
  }

  # Combine all results into a single tibble
  res_tbl <- dplyr::bind_rows(results) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, 6))

  # Identify optimal GCV values for each degree (first minimum found)
  opt_gcv <- res_tbl %>%
    arrange(degree, lambda) %>%
    group_by(degree) %>%
    filter(gcv == min(gcv, na.rm = TRUE)) %>%
    slice(1) %>%
    ungroup()

  # Identify optimal OCV values for each degree (first minimum found)
  opt_ocv <- res_tbl %>%
    dplyr::group_by(degree) %>%
    dplyr::filter(ocv == min(ocv, na.rm = TRUE)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # Create summary table with selected GCV and OCV results
  summary_opt <- opt_gcv %>%
    dplyr::select(degree, log_lambda_gcv = log_lambda, gcv_min = gcv, df_gcv = df, sse = sse) %>%
    dplyr::left_join(
      opt_ocv %>% dplyr::select(degree, log_lambda_ocv = log_lambda, ocv_min = ocv, df_ocv = df),
      by = "degree"
    )

  # Optional: generate diagnostic plots for each criterion (df, sse, gcv, ocv)
  plot_list <- NULL
  if (plot) {
    plot_list <- purrr::map(
      c("df", "sse", "gcv", "ocv"),
      function(stat) {
        ggplot2::ggplot(res_tbl, ggplot2::aes(x = log_lambda, y = .data[[stat]], color = factor(degree))) +
          ggplot2::geom_line(linewidth = 0.5) +
          ggplot2::labs(
            title = paste("Smoothing Selection:", toupper(stat)),
            x = expression(log[10](lambda)),
            y = stat,
            color = "Degree (m)"
          ) +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(legend.position = "bottom")
      }
    )
    names(plot_list) <- c("df", "sse", "gcv", "ocv")
  }

  # Create compact summary panel for reporting
  summary_panel <- make_summary_panel(summary_opt, penalty_type, normty)

  # Return all relevant results in a structured list
  return(res = list(
    results = res_tbl,
    summary_optimal = summary_opt,
    optimal_gcv = opt_gcv,
    optimal_ocv = opt_ocv,
    plots = plot_list,
    summary_panel = summary_panel,
    penalty_type = penalty_type,
    call = match.call()
  ))
}
