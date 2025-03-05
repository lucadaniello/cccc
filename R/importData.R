#' Import Term Document Matrix and Corpus Information
#' 
#' This function reads and processes a Term Document Matrix (TDM) and corpus information from CSV or Excel files.
#' It standardizes the format, renames columns, cleans keywords, and computes total frequency per keyword.
#' 
#' @param tdm_path Character. Path to the term-document matrix file (CSV or Excel).
#'  
#' tdm_path file description:
#' \tab The first column must contain the list of terms, while all other columns must be labeled with the corresponding years.  
#' 
#' @param corpus_path Character. Path to the corpus information file (CSV or Excel). 
#' 
#' corpus_path file description:
#' \tab The first column must contain the list of years
#' \tab The second column the total number of tokens per year
#' \tab The third column the number of documents per year
#' \tab The fourth column (if present) any additional metadata. 
#' 
#' @param sep Character. Separator used in CSV files (default is ",").
#' 
#' @return A list containing:
#' 	- `tdm`: The cleaned and processed term-document matrix, with a new column `tot_freq` representing the row sums of keyword occurrences.
#' 	- `corpus_info`: The corpus information dataset.
#' 
#' @examples
#' 
#' \donttest{
#' 
#' result <- importData("tdm.csv", "corpus.csv", sep=";")
#' tdm <- result$tdm
#' corpus_info <- result$corpus_info
#' 
#' }
#' @export
#' 
importData <- function(tdm_path, corpus_path, sep = ",") {
  
  # Function to read CSV or Excel
  read_data <- function(file_path, sep) {
    if (grepl("\\.csv$", file_path)) {
      return(read_delim(file_path, delim = sep,show_col_types = FALSE ))
    } else if (grepl("\\.xlsx$", file_path)) {
      return(read_excel(file_path))
    } else {
      stop("Unsupported file format. Use CSV or Excel.")
    }
  }
  
  # Data import
  tdm <- read_data(tdm_path, sep) %>% as_tibble()
  corpus_info <- read_data(corpus_path, sep) %>% as_tibble()
  
  # Rename main columns
  tdm <- tdm %>% rename(keyword = 1, tot_freq = ncol(tdm))
  
  # Extract years from corpus_info
  corpus_info <- corpus_info %>% 
    rename(years = 1, dimCorpus = 2, nDoc = 3) 
  
  # If a fourth column exists, rename it "metadata"
  if (ncol(corpus_info) == 4) {
    names(corpus_info)[4] <- "metadata"
  }
  
  # Map years in tdm
  ind_d <- grep("[0-9]", names(tdm))
  # names(tdm)[ind_d] <- corpus_info$years
  
  # Clean keywords: remove anomalous characters
  tdm$keyword <- gsub("[?]", "", tdm$keyword)
  
  # Identify and handle missing values
  missing_values <- which(is.na(tdm[ind_d]), arr.ind = TRUE)
  cat("There are", nrow(missing_values), "missing values in the tdm.\n")
  
  # Words with NA have been removed
  
  # Compute the total frequency sum per keyword
  tdm <- tdm %>% mutate(tot_freq = rowSums(select(., all_of(names(.)[ind_d])), na.rm = TRUE))
  
  return(list(tdm = tdm, corpus_info = corpus_info))
}


