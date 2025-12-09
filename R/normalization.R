#' Normalize Term-Document Matrix (TDM)
#'
#' This function normalizes the term-document matrix (TDM) contained within the list returned by \code{importData()},
#' using one of several normalization strategies. These strategies aim to adjust the raw keyword frequencies
#' for a more meaningful comparison across documents or time periods and across words of differing popularity.
#'
#' Available normalization types:
#' \describe{
#'   \item{\code{"nc"}}{Column-wise normalization by corpus size (tokens per year).
#'     Formula: \code{freq / dimCorpus * sc}}
#'   \item{\code{"nchi"}}{Chi-square-like normalization using row masses and expected frequencies.
#'     Formula: \code{(freq / tot_freq) / sqrt(Cmass) * sc}}
#'   \item{\code{"nM"}}{Normalization by the maximum frequency per row.
#'     First applies \code{"nc"}, then divides by row maximum.}
#'   \item{\code{"nmM"}}{Row-wise min-max normalization (scales to 0,1).
#'     Formula: \code{(freq - min) / (max - min)}}
#'   \item{\code{"nnl"}}{Non-linear normalization using asymmetry factors based on variance
#'     or mean-median skew. Adjusts for distributional asymmetry.}
#' }
#'
#' The function returns an updated version of the input \code{data} list where the \code{tdm} has been normalized
#' and a new logical element \code{norm} is set to \code{TRUE} to indicate normalization. The \code{tdm_long}
#' format is automatically regenerated. In the case of non-linear normalization (\code{"nnl"}) and
#' \code{p_asy = TRUE}, the list also includes a numeric vector \code{p_asy} with the asymmetry coefficients.
#'
#' @param data A list as returned by \code{importData()}, containing at minimum:
#'   \itemize{
#'     \item \code{tdm}: term-document matrix with keywords and yearly frequencies
#'     \item \code{corpus_info}: corpus metadata including \code{dimCorpus} (token counts)
#'     \item \code{year_cols}: indices of year columns in the TDM
#'   }
#' @param normty Character. Normalization type. Must be one of:
#'   \code{"nc"}, \code{"nchi"}, \code{"nM"}, \code{"nmM"}, or \code{"nnl"}.
#'   Default: \code{"nc"}.
#' @param sc Numeric. Scaling factor applied after normalization. If \code{NULL} (default),
#'   automatically set to 1000 for \code{"nc"} and \code{"nM"}, and 1 for other methods.
#'   Can be overridden by providing a specific value.
#' @param nnlty Character. Only used if \code{normty == "nnl"}. Type of asymmetry measure:
#'   \itemize{
#'     \item \code{"V"} (default): variance-based asymmetry
#'     \item \code{"M"}: mean-median-based asymmetry
#'   }
#' @param p_asy Logical. If \code{TRUE} (default) and \code{normty == "nnl"},
#'   includes the asymmetry vector in the output as \code{data$p_asy}.
#'
#' @return A list identical in structure to the input \code{data}, with these modifications:
#'   \itemize{
#'     \item \code{tdm}: normalized term-document matrix (same structure as input)
#'     \item \code{tdm_long}: regenerated long format with normalized frequencies
#'     \item \code{norm}: logical flag set to \code{TRUE}
#'     \item \code{normty}: character indicating the normalization method used
#'     \item \code{p_asy} (optional): numeric vector of asymmetry parameters
#'           (only if \code{normty == "nnl"} and \code{p_asy = TRUE})
#'   }
#'
#' @details
#' The function preserves the structure of the TDM, including metadata columns like
#' \code{keyword}, \code{tot_freq}, \code{int_freq}, and \code{zone}. Only the year
#' columns are normalized.
#'
#' For \code{"nchi"} normalization, the chi-square-like transformation emphasizes
#' deviations from expected frequencies, making it particularly useful for correspondence
#' analysis and identifying distinctive temporal patterns.
#'
#' The \code{"nnl"} method is useful when keyword distributions are highly asymmetric
#' across time, as it applies a non-linear transformation that accounts for skewness.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' tdm <- system.file("extdata", "tdm.csv", package = "cccc")
#' corpus <- system.file("extdata", "corpus.csv", package = "cccc")
#' data <- importData(tdm_file = tdm, corpus_file = corpus,
#'   sep_tdm = ";", sep_corpus_info = ";", zone = "stat")
#'
#' # Column normalization (default)
#' data_nc <- normalization(data, normty = "nc")
#'
#' # Chi-square normalization
#' data_nchi <- normalization(data, normty = "nchi", sc = 1000)
#'
#' # Max normalization
#' data_nM <- normalization(data, normty = "nM")
#'
#' # Min-max normalization
#' data_nmM <- normalization(data, normty = "nmM")
#'
#' # Non-linear normalization with variance-based asymmetry
#' data_nnl <- normalization(data, normty = "nnl", nnlty = "V", p_asy = TRUE)
#'
#' # Custom scaling factor
#' data_custom <- normalization(data, normty = "nc", sc = 500)
#' }
#'
normalization <- function(data,
                          normty = "nc",
                          sc = NULL,
                          nnlty = "V",
                          p_asy = TRUE) {

  # Input validation
  valid_types <- c("nc", "nchi", "nM", "nmM", "nnl")
  if (!normty %in% valid_types) {
    stop(paste0("Invalid normty. Must be one of: ", paste(valid_types, collapse = ", ")))
  }

  if (normty == "nnl" && !nnlty %in% c("V", "M")) {
    stop("Invalid nnlty. Must be 'V' (variance) or 'M' (mean-median).")
  }

  # Extract components
  tdm <- data$tdm
  corpus <- data$corpus_info
  year_cols <- data$year_cols
  other_cols <- setdiff(1:ncol(tdm), year_cols)

  # Set default scaling factor if not provided
  if (is.null(sc)) {
    sc <- if (normty %in% c("nc", "nM")) 1000 else 1
  }

  # Apply normalization based on type
  if (normty == "nc") {
    # Column-wise normalization by corpus size
    datn <- sweep(tdm[, year_cols], 2, 1 / corpus$dimCorpus, `*`) * sc

  } else if (normty == "nchi") {
    # Chi-square-like normalization
    Csum <- tdm %>%
      select(all_of(year_cols)) %>%
      summarise(across(everything(), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
      as.numeric()

    N <- sum(Csum)
    Cmass <- Csum / N

    # Row normalization first, then column normalization
    datnr <- sweep(tdm[, year_cols], 1, 1 / tdm$tot_freq, `*`)
    datn <- sweep(datnr, 2, 1 / sqrt(Cmass), `*`) * sc

  } else if (normty == "nM") {
    # Normalization by maximum frequency per row
    datnc <- sweep(tdm[, year_cols], 2, 1 / corpus$dimCorpus, `*`) * sc
    MRfreq <- apply(datnc, 1, max, na.rm = TRUE)

    # Handle division by zero
    MRfreq[MRfreq == 0] <- 1
    datn <- sweep(datnc, 1, 1 / MRfreq, `*`)

  } else if (normty == "nmM") {
    # Min-max normalization
    datnc <- sweep(tdm[, year_cols], 2, 1 / corpus$dimCorpus, `*`) * sc
    MRfreq <- apply(datnc, 1, max, na.rm = TRUE)
    mRfreq <- apply(datnc, 1, min, na.rm = TRUE)
    delta <- MRfreq - mRfreq

    # Handle division by zero (constant rows)
    delta[delta == 0] <- 1
    datn <- sweep(datnc - mRfreq, 1, 1 / delta, `*`)

  } else if (normty == "nnl") {
    # Non-linear normalization with asymmetry
    datnc <- sweep(tdm[, year_cols], 2, 1 / corpus$dimCorpus, `*`) * sc
    MRfreq <- apply(datnc, 1, max, na.rm = TRUE)
    mRfreq <- apply(datnc, 1, min, na.rm = TRUE)

    # Compute asymmetry parameter
    p <- if (nnlty == "V") {
      # Variance-based asymmetry
      dat_sort <- t(apply(datnc, 1, sort))
      n <- ncol(datnc)
      VL <- apply(dat_sort[, 1:(n %/% 2), drop = FALSE], 1, sd, na.rm = TRUE)
      VR <- apply(dat_sort[, (n %/% 2 + 1):n, drop = FALSE], 1, sd, na.rm = TRUE)

      # Handle division by zero
      denominator <- VR + VL
      denominator[denominator == 0] <- 1

      0.5 * (1 + (VR - VL) / denominator)
    } else {
      # Mean-median-based asymmetry
      mu <- rowMeans(datnc, na.rm = TRUE)
      me <- apply(datnc, 1, median, na.rm = TRUE)
      sdev <- apply(datnc, 1, sd, na.rm = TRUE)

      # Handle division by zero
      sdev[sdev == 0] <- 1

      0.5 * (1 + (mu - me) / sdev)
    }

    # Apply non-linear transformation
    datn <- datnc
    for (i in seq_len(nrow(datn))) {
      num <- p[i] * (datn[i, ] - mRfreq[i])
      den <- num + (1 - p[i]) * (MRfreq[i] - datn[i, ])

      # Handle division by zero
      den[den == 0] <- 1
      datn[i, ] <- num / den
    }
  }

  # Reconstruct TDM with normalized values
  result <- cbind(tdm[, other_cols], datn)

  # Update data object
  data$norm <- TRUE
  data$tdm <- result
  data$normty <- normty

  # Regenerate long format
  data <- tdm2long(data)

  # Add asymmetry parameters if requested
  if (normty == "nnl" && p_asy) {
    data$p_asy <- p
  }

  return(data)
}
