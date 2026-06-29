#' Selection of Optimal Spline Degree and Penalization Strategy
#'
#' This function compares multiple smoothing strategies—each defined by a specific penalization type—
#' and selects the combination of spline degree (`m`) and penalty type that minimizes the Generalized
#' Cross-Validation (GCV) criterion. It uses the output of multiple calls to [smoothingSelection()],
#' one for each penalty type, and constructs a GCV comparison matrix.
#'
#' Optionally, the function generates diagnostic plots comparing `gcv`, `ocv`, `df`, and `sse` across
#' degrees and penalty types.
#'
#' @param smoothing_results A named list of results from [smoothingSelection()], with each entry corresponding
#'   to a penalty type (`"m-2"`, `"2"`, `"1"`, `"0"`).
#' @param plot Logical. If `TRUE` (default), returns `ggplot2`-based visualizations comparing the smoothing
#'   statistics across penalty types and orders
#'
#' @return A list containing:
#' \describe{
#'   \item{opt_summary}{A list with all data regarding the optimal penalty type resulted by [smoothingSelection()]}
#'   \item{m_opt}{Integer. Optimal spline degree minimizing GCV across all penalties.}
#'   \item{penalty_opt}{Character. The optimal penalty type (one of `"m-2"`, `"2"`, `"1"`, `"0"`).}
#'   \item{llambda_opt}{Numeric. The optimal log10(lambda) value corresponding to the selected spline order and penalty.}
#'   \item{gcv_opt}{Numeric matrix of GCV values. Rows correspond to penalty types, columns to spline orders.}
#'   \item{top4_opt}{Numeric matrix of GCV values. Rows correspond to penalty types, columns to spline orders.}
#'   \item{plots}{(Optional) A named list of `ggplot` objects for `log10(lambda)`, `df`, `sse`, and `gcv` if `plot = TRUE`.}
#'   \item{call}{The matched function call, useful for reproducibility.}
#' }
#'
#' @seealso [smoothingSelection()], [plotOptimalSmoothing()]
#'
#' @examples
#' \dontrun{
#' tdm <- system.file("extdata", "tdm.csv", package = "cccc")
#' corpus <- system.file("extdata", "corpus.csv", package = "cccc")
#' data <- importData(tdm_file = tdm, corpus_file = corpus,
#' sep_tdm = ";",sep_corpus_info = ";",zone="stat")
#'
#' data_nchi <- normalization(data, normty = "nchi", sc = 1000)
#'
#' # Assume multiple smoothing selections have been computed:
#' results_m2 <- smoothingSelection(data_nchi, penalty_type = "m-2", plot = FALSE)
#' results_2  <- smoothingSelection(data_nchi, penalty_type = "2", plot = FALSE)
#' results_1  <- smoothingSelection(data_nchi, penalty_type = "1", plot = FALSE)
#' results_0  <- smoothingSelection(data_nchi, penalty_type = "0", plot = FALSE)
#'
#' # Compare and select best smoothing strategy
#' opt <- optimalSmoothing(list("m-2" = results_m2, "2" = results_2, "1" = results_1, "0" = results_0))
#' }
#'
#' @export

optimalSmoothing <- function(smoothing_results, plot = TRUE) {

  # Extract penalty labels (must be among the supported types)
  penalties <- names(smoothing_results)
  stopifnot(all(penalties %in% c("m-2", "2", "1", "0")))

  # Extract optimal order information from each smoothing result
  opt_summary <- purrr::map(smoothing_results, ~ .x$opt_order) %>% purrr::list_rbind()

  # Identify the combination of penalty and spline order with the lowest GCV, as well as the associated logLambda
  min_pos <- opt_summary %>% pull(gcv) %>% which.min()
  penalty_opt <- opt_summary$penalty_type[min_pos[1]]
  m_opt <-  opt_summary$degree[min_pos[1]]
  llambda_opt <- opt_summary$log_lambda[min_pos[1]]
  gcv_opt <- opt_summary$gcv[min_pos[1]]
  top4_opt <- opt_summary %>%
    arrange(gcv) %>%
    pull(penalty_type)

  # Convert each smoothing result into a 3D array (temparray), one per penalty type
  temparray_list <- purrr::map(smoothing_results, ~ make_temparray(.x$summary_optimal))

  # # Collect all unique spline degrees across all penalties
  # all_degrees <- sort(unique(unlist(lapply(temparray_list, function(x) as.integer(dimnames(x)[[2]])))))
  #
  # # Initialize GCV matrix: rows = penalties, columns = spline orders
  # gcv_matrix <- matrix(NA, nrow = length(penalties), ncol = length(all_degrees),
  #                      dimnames = list(penalties, as.character(all_degrees)))
  #
  # # Populate GCV matrix with values extracted from each temparray
  # for (pen in penalties) {
  #   temp <- temparray_list[[pen]]
  #   degrees <- dimnames(temp)[[2]]
  #   gcv_vals <- temp["gcv", , 1]
  #   gcv_matrix[pen, degrees] <- gcv_vals
  # }

  # # Identify the combination of penalty and spline order with the lowest GCV
  # min_pos <- which(gcv_matrix == min(gcv_matrix, na.rm = TRUE), arr.ind = TRUE)
  # penalty_opt <- rownames(gcv_matrix)[min_pos[1]]
  # m_opt <- as.integer(colnames(gcv_matrix)[min_pos[2]])

  # optSmoothing <- smoothing_results[[penalty_opt]] #data relative to the penalty type smoothing

  # lambda_opt <- optSmoothing$summary_optimal$log_lambda_gcv[optSmoothing$summary_optimal$degree == m_opt]

  # Optionally create comparative plots of selection criteria across penalty types
  plot_list <- NULL
  if (plot) {
    plot_list <- purrr::map(
      c("lLambda", "df", "sse", "gcv"),
      function(stat) {
        # Create a tidy data frame with statistics across all penalties and degrees
        df_plot <- purrr::map2_dfr(temparray_list, names(temparray_list), function(arr, penalty) {
          tibble::tibble(
            penalty = factor(penalty, levels = top4_opt[4:1]),
            degree = as.integer(dimnames(arr)[[2]]),
            value = arr[stat, ,1]
          )
        })

        # Generate ggplot for each criterion
        p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = degree, y = value, color = penalty)) +
          ggplot2::geom_line(linewidth = 0.7) +
          ggplot2::geom_vline(xintercept = m_opt, linetype = "dashed", color = "black", linewidth = 0.5) +
          #ggplot2::geom_segment(aes(x = m_opt, y = value, xend = m_opt, yend = value, color = (penalty==penalty_opt)), linetype = "dashed",  linewidth = 0.5) +
          ggplot2::scale_colour_manual(values=c("0"="#00FF00", "1"="#9933CC", "2"="#FF3399", "m-2"="#FF9900"), limits = c("m-2", "2", "1", "0")) +
          ggplot2::labs(
            title = paste("Comparison of", toupper(stat), "across penalties"),
            x = "Spline order (m)",
            y = stat,
            color = "Penalty"
          ) +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(legend.position = "bottom")
      }
    )
    names(plot_list) <- c("lLambda", "df", "sse", "gcv")
  }


  # Return optimal results and optional plots
  res <- list(
    #optSmoothing = optSmoothing,
    opt_summary = opt_summary,
    m_opt = m_opt,
    penalty_opt = penalty_opt,
    llambda_opt = llambda_opt,
    gcv_opt = gcv_opt,
    top4_opt = top4_opt,
    #gcv_matrix = gcv_matrix,
    plots = plot_list,
    call = match.call()
  )
  return(res)
}

