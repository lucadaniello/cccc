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
#'   statistics across penalty types and degrees.
#'
#' @return A list containing:
#' \describe{
#'   \item{m_opt}{Integer. Optimal spline degree minimizing GCV across all penalties.}
#'   \item{penalty_opt}{Character. The optimal penalty type (one of `"m-2"`, `"2"`, `"1"`, `"0"`).}
#'   \item{gcv_matrix}{Numeric matrix of GCV values. Rows correspond to penalty types, columns to spline degrees.}
#'   \item{plots}{(Optional) A named list of `ggplot` objects for `gcv`, `ocv`, `df`, and `sse`, if `plot = TRUE`.}
#'   \item{call}{The matched function call, useful for reproducibility.}
#' }
#'
#' @seealso [smoothingSelection()], [plotOptimalSmoothing()]
#'
#' @examples
#' \dontrun{
#' # Assume multiple smoothing selections have been computed:
#' results_m2 <- smoothingSelection(data, penalty_type = "m-2", plot = FALSE)
#' results_2  <- smoothingSelection(data, penalty_type = "2", plot = FALSE)
#' results_1  <- smoothingSelection(data, penalty_type = "1", plot = FALSE)
#' results_0  <- smoothingSelection(data, penalty_type = "0", plot = FALSE)
#'
#' # Compare and select best smoothing strategy
#' opt <- optimalSmoothing(list("m-2" = results_m2, "2" = results_2, "1" = results_1, "0" = results_0))
#' opt$m_opt
#' opt$penalty_opt
#' opt$plots$gcv
#' }
#'
#' @export

optimalSmoothing <- function(smoothing_results, plot = TRUE) {

  # Extract penalty labels (must be among the supported types)
  penalties <- names(smoothing_results)
  stopifnot(all(penalties %in% c("m-2", "2", "1", "0")))

  # Convert each smoothing result into a 3D array (temparray), one per penalty type
  temparray_list <- purrr::map(smoothing_results, ~ make_temparray(.x$summary_optimal))

  # Collect all unique spline degrees across all penalties
  all_degrees <- sort(unique(unlist(lapply(temparray_list, function(x) as.integer(dimnames(x)[[2]])))))

  # Initialize GCV matrix: rows = penalties, columns = spline degrees
  gcv_matrix <- matrix(NA, nrow = length(penalties), ncol = length(all_degrees),
                       dimnames = list(penalties, as.character(all_degrees)))

  # Populate GCV matrix with values extracted from each temparray
  for (pen in penalties) {
    temp <- temparray_list[[pen]]
    degrees <- dimnames(temp)[[2]]
    gcv_vals <- temp["gcv", , 1]
    gcv_matrix[pen, degrees] <- gcv_vals
  }

  # Identify the combination of penalty and spline degree with the lowest GCV
  min_pos <- which(gcv_matrix == min(gcv_matrix, na.rm = TRUE), arr.ind = TRUE)
  penalty_opt <- rownames(gcv_matrix)[min_pos[1]]
  m_opt <- as.integer(colnames(gcv_matrix)[min_pos[2]])

  # Optionally create comparative plots of selection criteria across penalty types
  plot_list <- NULL
  if (plot) {
    plot_list <- purrr::map(
      c("gcv", "ocv", "df", "sse"),
      function(stat) {
        # Create a tidy data frame with statistics across all penalties and degrees
        df_plot <- purrr::map2_dfr(temparray_list, names(temparray_list), function(arr, penalty) {
          tibble::tibble(
            penalty = penalty,
            degree = as.integer(dimnames(arr)[[2]]),
            value = arr[stat, , 1]
          )
        })

        # Generate ggplot for each criterion
        ggplot2::ggplot(df_plot, ggplot2::aes(x = degree, y = value, color = penalty)) +
          ggplot2::geom_line(linewidth = 0.7) +
          ggplot2::geom_vline(xintercept = m_opt, linetype = "dashed", color = "black") +
          ggplot2::labs(
            title = paste("Comparison of", toupper(stat), "across penalties"),
            x = "Spline degree (m)",
            y = stat,
            color = "Penalty"
          ) +
          ggplot2::theme_minimal(base_size = 11) +
          ggplot2::theme(legend.position = "bottom")
      }
    )
    names(plot_list) <- c("gcv", "ocv", "df", "sse")
  }

  # Return optimal results and optional plots
  return(list(
    m_opt = m_opt,
    penalty_opt = penalty_opt,
    gcv_matrix = gcv_matrix,
    plots = plot_list,
    call = match.call()
  ))
}

