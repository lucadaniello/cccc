#' Import Term Document Matrix and Corpus Information
#'
#' This function reads and processes a Term Document Matrix (TDM) and corpus information from CSV or Excel files.
#' It standardizes the format, renames columns, cleans keywords, computes total frequency per keyword,
#' and classifies terms into frequency zones. It also creates a column representing
#' the frequency interval associated with each zone.
#'
#' @param tdm_file Character. Path to the term-document matrix file (CSV or Excel).
#'
#' tdm_file description:
#' \tab The first column must contain the list of terms, while all other columns must be labeled with the corresponding years.
#'
#' @param corpus_file Character. Path to the corpus information file (CSV or Excel).
#'
#' corpus_file description:
#' \tab The first column must contain the list of years.
#' \tab The second column the total number of tokens per year.
#' \tab The third column the number of documents per year.
#' \tab The fourth column (if present) any additional metadata.
#'
#' @param sep_tdm Character or NULL. Separator used in the TDM CSV file (e.g., "," or ";"). Ignored if the file is Excel.
#' @param sep_corpus_info Character or NULL. Separator used in the corpus information CSV file. Ignored if the file is Excel.
#' @param zone Character. Zone classification strategy to use. Either \code{"stat"} (default: statistical quartiles or balanced classes) or \code{"ling"} (linguistic frequency-based).
#' @param verbose Logical. If TRUE (default), the function prints progress messages. If FALSE, it operates silently.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{tdm}{A tibble representing the cleaned and processed term-document matrix. Contains:
#'     \itemize{
#'       \item \code{keyword}: the lexical unit or term.
#'       \item \code{tot_freq}: the total frequency of the term across all years.
#'       \item \code{int_freq}: a string representation of the frequency interval for the assigned zone.
#'       \item \code{zone}: the assigned frequency zone (e.g., \code{"VL"}, \code{"L"}, \code{"H"}, \code{"VH"} or \code{"low"}, \code{"medium"}, \code{"high"} depending on strategy).
#'       \item Yearly frequency columns (one per year).
#'     }}
#'   \item{corpus_info}{A tibble containing corpus-level yearly metadata:
#'     \itemize{
#'       \item \code{years}: year of observation.
#'       \item \code{dimCorpus}: total number of tokens.
#'       \item \code{nDoc}: number of documents.
#'       \item \code{metadata} (optional): additional information.
#'     }}
#'   \item{norm}{Logical. Indicates whether the term-document matrix has been normalized. Default is \code{FALSE}. If the TDM is normalized using the \code{normalize()} function, this will be set to \code{TRUE}.}
#'   \item{year_cols}{A numeric vector indicating which columns in the TDM refer to yearly frequencies.}
#'   \item{zone}{Character vector of unique frequency zones used.}
#'   \item{colors}{Character vector of default colors associated with zones.}
#' }
#'
#' @examples
#' \donttest{
#' data <- importData("tdm.csv", "corpus.csv", sep_tdm = ";", sep_corpus_info = ",")
#' tdm <- data$tdm
#' corpus_info <- data$corpus_info
#' }
#'
#' @export

importData <- function(tdm_file, corpus_file, sep_tdm = NULL, sep_corpus_info = NULL, zone ="stat", verbose = TRUE) {

  # Import data
  tdm <- read_data(tdm_file, sep_tdm) %>% as_tibble()
  corpus_info <- read_data(corpus_file, sep_corpus_info) %>% as_tibble()

  # Identify years' cols in the tdm
  year_cols <- grep("\\d", names(tdm), value = TRUE)

  tdm <- tdm %>%
    rename(keyword = 1) %>%
    mutate(across(all_of(year_cols), as.numeric),
           keyword = str_replace_all(keyword, "\\?", ""),
           tot_freq = rowSums(across(all_of(year_cols)), na.rm = TRUE))

  if (verbose) {
    n_missing <- sum(is.na(select(tdm, all_of(year_cols))))
    message("There are ", n_missing, " missing values in the tdm.\n")
  }



  switch(zone,
         stat={
           Q <- quantile(-tdm$tot_freq, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

           # Zone variable - quartile distribution
           tdm$zone <- cut(-tdm$tot_freq,
                          breaks = Q,
                          include.lowest = TRUE,
                          labels = c("VH", "H", "L", "VL"))
         },
         ling={
           tdm <- tdm %>% arrange(desc(tot_freq))
           freq_vector <- tdm$tot_freq

           high_limit <- which(duplicated(freq_vector))[1] - 1
           if (is.na(high_limit)) high_limit <- 1

           sorted_unique <- sort(unique(freq_vector))
           low_cutoff <- ifelse(any(diff(sorted_unique) > 1),
                                sorted_unique[which(diff(sorted_unique) > 1)[1]],
                                max(freq_vector))

           tdm <- tdm %>%
             mutate(zone = case_when(
               row_number() <= high_limit ~ "high",
               tot_freq <= low_cutoff ~ "low",
               TRUE ~ "medium"
             ),
             zone = factor(zone, levels = c("high", "medium", "low")))
         })

  freq_labels <- tdm %>%
    group_by(zone) %>%
    summarise(int_freq = paste0("[", min(tot_freq), "-", max(tot_freq), "]"), .groups = "drop")

  tdm <- tdm %>%
    left_join(freq_labels, by = "zone") %>%
    relocate(keyword, tot_freq, int_freq, zone)

  # Corpus info
  corpus_info <- corpus_info %>%
    rename(years = 1, dimCorpus = 2, nDoc = 3)

  if (ncol(corpus_info) == 4) names(corpus_info)[4] <- "metadata"


  data <- list(tdm = tdm, corpus_info = corpus_info, norm=FALSE,
               year_cols = grep("\\d", names(tdm)),
               zone=unique(tdm$zone),
               colors=colorlist()[1:length(unique(tdm$zone))])
  data <- tdm2long(data)
  return(data)
}



