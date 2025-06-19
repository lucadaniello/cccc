#' Normalize Term-Document Matrix (TDM)
#'
#' This function normalizes the term-document matrix (TDM) contained within the list returned by `importData()`,
#' using one of several normalization strategies. These strategies aim to adjust the raw keyword frequencies
#' for more meaningful comparison across documents or time periods.
#'
#' Available normalization types:
#' - \code{"nc"}: column-wise normalization by corpus size (tokens per year).
#' - \code{"nchi"}: chi-square-like normalization using row masses and expected frequencies.
#' - \code{"nM"}: normalization by the maximum frequency per row.
#' - \code{"nmM"}: row-wise min-max normalization.
#' - \code{"nnl"}: non-linear normalization using asymmetry factors (based on either variance or mean-median skew).
#'
#' The function returns an updated version of the input `data` list where the `tdm` has been normalized
#' and a new logical element \code{norm} is set to \code{TRUE} to indicate normalization. In the case of
#' non-linear normalization (`nnl`) and \code{p_asy = TRUE}, the list also includes a numeric vector
#' \code{p_asy} with the asymmetry coefficients used for the transformation.
#'
#' @param data A list as returned by \code{importData()}
#' @param normty Character. Normalization type. One of \code{"nc"}, \code{"nchi"}, \code{"nM"}, \code{"nmM"}, \code{"nnl"}.
#' @param sc Numeric. Scaling factor applied after normalization. Default is \code{1000} for \code{"nc"} and \code{"nM"}, otherwise \code{1}.
#' @param nnlty Character. If \code{normty == "nnl"}, type of asymmetry measure: \code{"V"} (variance-based) or \code{"M"} (mean-median based).
#' @param p_asy Logical. If \code{TRUE} (default) and \code{normty == "nnl"}, includes the asymmetry vector \code{p_asy} in the output.
#'
#' @return A list identical in structure to the input `data`, with these modifications:
#'   \itemize{
#'     \item \code{tdm}: normalized term-document matrix.
#'     \item \code{norm}: logical flag set to \code{TRUE}.
#'     \item \code{normty}: Character. Indicates the type of normalization applied.
#'     \item \code{p_asy} (optional): numeric vector of asymmetry parameters, only if \code{normty == "nnl"} and \code{p_asy = TRUE}.
#'   }
#'
#' @examples
#' \donttest{
#' data <- importData("tdm.csv", "corpus.csv", sep_tdm = ";", sep_corpus_info = ",")
#' data_norm <- normalization(data, normty = "nc")
#' data_norm$norm # returns TRUE
#' }
#'
#' @export


normalization <- function(data,
                      normty = "nc",
                      sc = 1000,
                      nnlty = "V",
                      p_asy = TRUE) {
  tdm <- data$tdm
  corpus <- data$corpus_info
  year_cols <- data$year_cols
  other_cols <- setdiff(1:ncol(tdm), year_cols)

    sc <- if (normty %in% c("nc", "nM")) 1000 else 1

  if (normty == "nc") {
    # Norm by column colonna (with respect to dimCorpus)
    datn <- sweep(tdm[, year_cols], 2, 1 / corpus$dimCorpus, `*`) * sc

  } else if (normty == "nchi") {
    Csum <- tdm |>
      select(all_of(year_cols)) |>
      summarise(across(everything(), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
      as.numeric()

    N <- sum(Csum)
    Cmass <- Csum / N

    datnr <- sweep(tdm[, year_cols], 1, 1 / tdm$tot_freq, `*`)
    datn <- sweep(datnr, 2, 1 / sqrt(Cmass), `*`) * sc

  } else if (normty == "nM") {
    datnc <- sweep(tdm[, year_cols], 2, 1 / corpus$dimCorpus, `*`) * sc
    MRfreq <- apply(datnc, 1, max)
    datn <- sweep(datnc, 1, 1 / MRfreq, `*`)

  } else if (normty == "nmM") {
    datnc <- sweep(tdm[, year_cols], 2, 1 / corpus$dimCorpus, `*`) * sc
    MRfreq <- apply(datnc, 1, max)
    mRfreq <- apply(datnc, 1, min)
    delta <- MRfreq - mRfreq
    datn <- sweep(datnc - mRfreq, 1, 1 / delta, `*`)

  } else if (normty == "nnl") {
    datnc <- sweep(tdm[, year_cols], 2, 1 / corpus$dimCorpus, `*`) * sc
    MRfreq <- apply(datnc, 1, max)
    mRfreq <- apply(datnc, 1, min)

    p <- if (nnlty == "V") {
      dat_sort <- t(apply(datnc, 1, sort))
      n <- ncol(datnc)
      VL <- apply(dat_sort[, 1:(n %/% 2)], 1, sd)
      VR <- apply(dat_sort[, (n %/% 2 + 1):n], 1, sd)
      0.5 * (1 + (VR - VL) / (VR + VL))
    } else {
      mu <- rowMeans(datnc)
      me <- apply(datnc, 1, median)
      sdev <- apply(datnc, 1, sd)
      0.5 * (1 + (mu - me) / sdev)
    }

    datn <- datnc
    for (i in seq_len(nrow(datn))) {
      num <- p[i] * (datn[i, ] - mRfreq[i])
      den <- num + (1 - p[i]) * (MRfreq[i] - datn[i, ])
      datn[i, ] <- num / den
    }
  }

  result <- cbind(tdm[, other_cols], datn)
  data$norm <- TRUE
  data$tdm <- result
  data <- tdm2long(data)
  data$normty <- normty

  if (normty == "nnl" && p_asy) {
    data$p_asy = p
  }


  return(data)
}
