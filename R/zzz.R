#' @import ggplot2
#' @import dplyr
#' @import readr
#' @import readxl
#' @import stringr
#' @import ggnewscale
#' @import stringr
#' @import tidyr

.onAttach<-function(...){
  packageStartupMessage("cccc package")
}

# Utility functions

## Helper: read CSV o Excel
read_data <- function(path, sep) {
  if (str_ends(path, ".csv")) {
    if (is.null(sep)) stop("CSV file provided, but separator is NULL.")
    read_delim(path, delim = sep, show_col_types = FALSE)
  } else if (str_ends(path, ".xlsx?$")) {
    readxl::read_excel(path)
  } else {
    stop("Unsupported file format.")
  }
}

## from tdm to long format
tdm2long <- function(data){
  ind_d <- data$year_cols
  n_d <- length(ind_d)
  n <- nrow(data$tdm)
  year <- data$corpus_info %>% select(years) %>% pull()
  data$tdm_long <- data$tdm %>%
    pivot_longer(
      cols = (min(ind_d)):ncol(.),  # columns to pivot
      names_to = "year",
      values_to = "freq"
    ) %>% arrange(year, desc(tot_freq)) %>%
    mutate(chrono=rep(1:n_d,rep(n,n_d))) %>%
    group_by(year) %>%
    mutate(cont=rep(1:n())) %>%
    ungroup() %>%
    select("keyword", "year", "cont", "chrono", "freq","tot_freq", "int_freq","zone")
  return(data)
}

# colorlist <- function(){
#   c("#BA55D3","#00BFFF","#A2CD5A","#DAA520", # for zone
#     "#DC143C", "#FF4500","#BC80BD","#BEBADA", # for keywords
#     "#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#A65628","#F781BF","#999999","#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F"
#     ,"#B3B3B3","#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#B15928","#8DD3C7","#BEBADA"
#     ,"#FB8072","#80B1D3","#FDB462","#B3DE69","#D9D9D9","#BC80BD","#CCEBC5")
# }

colorlist <- function(type="light"){
  if (type=="light"){
    c(	"#54FF9F","#00C5CD","#8470FF","#828282", # for zone
      "#EE2C2C","#FFA500","#FF00FF","#1F78B4", # for keywords
      "#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#A65628","#F781BF","#999999","#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F"
      ,"#B3B3B3","#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#B15928","#8DD3C7","#BEBADA"
      ,"#FB8072","#80B1D3","#FDB462","#B3DE69","#D9D9D9","#BC80BD","#CCEBC5")
  } else {
    c("#FFFFE0",  # lightyellow
    "#C1FFC1",  # darkseagreen1
    "#E9967A",  # darksalmon
    "#8B6969",  # rosybrown4
    "#00C5CD",  # turquoise3
    "#54FF9F",  # seagreen1
    "#8470FF",   # lightslateblue
    "#BEBADA", # for keywords
      "#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#A65628","#F781BF","#999999","#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F"
      ,"#B3B3B3","#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#B15928","#8DD3C7","#BEBADA"
      ,"#FB8072","#80B1D3","#FDB462","#B3DE69","#D9D9D9","#BC80BD","#CCEBC5")
  }

}
