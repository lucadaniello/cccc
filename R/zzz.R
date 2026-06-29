utils::globalVariables(c(
  ".", "chrono", "degree", "df", "df_gcv", "dimCorpus", "example", "freq",
  "gcv", "gcv_min", "int_freq", "keyword", "kw_color", "label", "lambda",
  "median", "metric", "nDoc", "ocv", "ocv_min", "penalty", "quantile",
  "reorder", "sd", "setNames", "smooth", "sse", "time", "tot_freq", "value",
  "year", "years", "zone", "zone_color", "zone_label","cluster_num", "letter",
  "Freq", "color", "perc", "cluster", "cont", "available", "selected", "rank", "penalty_type"
))

#' @import ggplot2
#' @import dplyr
#' @import readr
#' @import readxl
#' @import stringr
#' @import ggnewscale
#' @import purrr
#' @import tidyr
#' @importFrom stats cor
#'
.onAttach<-function(...){
  packageStartupMessage("cccc package")
}

# Utility functions

read_data <- function(path, sep) {
  if (path == "" || !file.exists(path)) {
    stop("File not found: make sure the path is correct and the file exists.")
  }

  ext <- tolower(tools::file_ext(path))

  if (ext == "csv") {
    if (is.null(sep)) stop("CSV file provided, but separator is NULL.")
    readr::read_delim(path, delim = sep, show_col_types = FALSE)
  } else if (ext %in% c("xls", "xlsx")) {
    readxl::read_excel(path)
  } else {
    stop("Unsupported file format: ", ext)
  }
}



## Convert TDM from wide format to long format
tdm2long <- function(data){
  ind_d <- data$year_cols
  n_d <- length(ind_d)
  n <- nrow(data$tdm)

  # Extract year names and convert to numeric
  year_names <- names(data$tdm)[ind_d]
  year <- suppressWarnings(as.numeric(year_names))

  # Check if year conversion was successful
  if (any(is.na(year))) {
    stop("Year column names must be numeric or convertible to numeric (e.g., '1965', '1966', ...)")
  }

  # Create chronological index starting from 1
  chrono <- 1 + year - year[1]

  data$tdm_long <- data$tdm %>%
    pivot_longer(
      cols = all_of(ind_d),  # Use all_of() for robust selection
      names_to = "year",
      values_to = "freq"
    ) %>%
    arrange(year, desc(tot_freq)) %>%
    mutate(
      cont=rep(1:n_d, each = n),  # Contiguous time index
      chrono = rep(chrono, each = n)   # Chronological index from 1
      #,year = as.numeric(year)           # Ensure year is numeric
    ) %>%
    select("keyword", "year", "cont", "chrono", "freq","tot_freq", "int_freq","zone")
  return(data)
}

# panel plot for smoothing selection
make_summary_panel <- function(summary_opt, opt_gcv, opt_ocv, penalty_type, normty) {
  # Ristruttura in long format
  df_long <- summary_opt %>%
    pivot_longer(cols = c(df_gcv, sse, ocv_min, gcv_min),
                 names_to = "metric", values_to = "value") %>%
    mutate(metric = recode(metric,
                           df_gcv = "df",
                           sse = "sse",
                           ocv_min = "ocv",
                           gcv_min = "gcv")) %>%
    mutate(metric = factor(metric, levels = c("df", "sse", "ocv", "gcv")))

  highlight <- opt_gcv[, c(1,4:5,7,6)] %>%
    #dplyr::select(degree, df, sse, ocv, gcv) %>%
    filter(gcv == min(gcv)) %>% slice(1)
  highlight <- highlight %>%
    tidyr::pivot_longer(cols = c(df, sse, ocv, gcv),
                        names_to = "metric", values_to = "value")
  highlight_ocv <- opt_ocv[, c(1,7)] %>%
    #dplyr::select(degree, ocv) %>%
    filter(ocv == min(ocv)) %>% slice(1)
  highlight$degree[3] <- highlight_ocv$degree[1]
  highlight$value[3] <- highlight_ocv$ocv[1]
  highlight$metric <- factor(highlight$metric, levels = c("df", "sse", "ocv", "gcv"))


  p <- ggplot(df_long, aes(x = degree, y = value)) +
    geom_line(aes(group = 1), color = "gray40", linewidth = 0.6) +
    geom_point(color = "black", size = 0.8) +
    geom_point(data = highlight, aes(x = degree, y = value), color = "firebrick", size = 1.2) +
    geom_vline(data = highlight, aes(xintercept = degree), linetype = "dashed", color = "firebrick", linewidth = 0.4) +
    geom_segment(data = highlight, aes(x = 1, y = value, xend = degree, yend = value),
                 linetype = "dashed", color = "firebrick", linewidth = 0.4) +
    facet_wrap(~metric, scales = "free_y", ncol = 2) +
    labs(title = "Optimal Smoothing Summary",
         x = "Spline order (m)",
         y = NULL,
         subtitle = paste0("Penalty: ", penalty_type, " | Norm: ", normty)
    ) +
    theme_minimal(base_size = 11) +
    theme(strip.text = element_text(face = "bold"))

  return(p)
}



#make_temparray auxiliary function that converts the summary_optimal (or results) of smoothingSelection() into a compatible 3D array

make_temparray <- function(summary_opt, stats = c("lLambda", "df", "sse", "gcv")) {
  ord <- summary_opt$degree
  arr <- array(NA, dim = c(length(stats), length(ord), 1),
               dimnames = list(stats, as.character(ord), NULL))

  arr["lLambda", , 1] <- summary_opt$log_lambda_gcv
  arr["df", , 1]  <- summary_opt$df_gcv
  arr["sse", , 1] <- summary_opt$sse
  arr["gcv", , 1] <- summary_opt$gcv_min
  return(arr)
}



#Reconstruct Smoothed Functional Data Object
# Compute the optimal smoothing spline fit for keyword frequency curves
# data A list returned by importData
# opt_results A list returned by optimalSmoothing(resSmoothing) where resSmoothing is a list with all smoothingSeleciton penalty type outputs

getWsmooth <- function(data, opt_result) {
  # Step 1: Extract the term-document matrix as a numeric matrix of keyword frequencies over time
  mat <- data$tdm %>%
    dplyr::select(dplyr::any_of(data$year_cols)) %>%
    as.matrix()

  # Step 2: Assign keywords as row names
  rownames(mat) <- data$tdm$keyword

  # Step 3: Filter out keywords with zero total frequency across all years
  mat <- mat[rowSums(mat) > 0, , drop = FALSE]

  # Step 4: Define the time points (e.g., we place the knots at observed time points)
  samplpo <- names(data$tdm)[data$year_cols] %>% as.numeric
  fdtime <- 1+samplpo-samplpo[1]
  br <- fdtime

  # Step 5: Extract optimal degree and penalty type as well as other parameters from the optimization result
  m_opt <- opt_result$m_opt
  penalty_opt <- opt_result$penalty_opt
  llambda_opt <- opt_result$llambda_opt
  gcv_opt <- opt_result$gcv_opt

  lambda <- 10^llambda_opt

  # Step 7: Define the B-spline basis with the selected degree
  basis <- fda::create.bspline.basis(breaks = br, norder = m_opt)

  # Step 8: Choose the order of the differential operator based on the penalty type
  lfd <- switch(penalty_opt,
                "m-2" = max(0, m_opt - 2),
                "2" = if (m_opt > 3) 2 else if (m_opt == 3) 1 else 0,
                "1" = if (m_opt > 2) 1 else 0,
                "0" = 0)

  # Step 9: Define the smoothing configuration using basis, differential operator, and lambda
  fdpar <- fda::fdPar(basis, lfd, lambda)

  # Step 10: Apply smoothing to the data (transpose: keywords become columns)
  smoothed <- fda::smooth.basis(argvals = fdtime, y = t(mat), fdParobj = fdpar)

  # Step 11: Label the coefficient matrix columns with the keyword names
  colnames(smoothed$fd$coefs) <- rownames(mat)

  # Step 12: Attach degree and penalty info to the output object
  smoothed$penalty <- penalty_opt
  smoothed$order <- m_opt

  # Step 13: Return the smoothed functional data object
  return(smoothed)
}


#' Get Optimization Rules for Clustering Validation Criteria
#'
#' Internal function that returns the set of optimization rules for each criterion.
#' Used by buildCIVf() to decide how to interpret each criterion's values.
#'
#' @return Named character vector of optimization rules.
#' @keywords internal
get_opt_civ <- function() {
  c(
    Ball_Hall = "maxd2",
    Ball_2 = "mind1",
    Banfeld_Raftery = "min",
    C_index = "min",
    Cindex_2 = "min",
    Calinski_Harabasz = "max",
    Davies_Bouldin = "min",
    DB_2 = "min",
    Det_Ratio = "mind2",
    Dunn = "max",
    Gamma = "max",
    G_plus = "min",
    GDI12 = "max",
    GDI13 = "max",
    GDI21 = "max",
    GDI22 = "max",
    GDI23 = "max",
    GDI31 = "max",
    GDI32 = "max",
    GDI33 = "max",
    GDI41 = "max",
    GDI42 = "max",
    GDI43 = "max",
    GDI51 = "max",
    GDI52 = "max",
    GDI53 = "max",
    Ksq_DetW = "maxd2",
    Log_Det_Ratio = "mind2",
    Log_SS_Ratio = "mind2",
    McClain_Rao = "min",
    PBM = "max",
    Point_Biserial = "max",
    Ratkowsky_Lance = "max",
    Ray_Turi = "min",
    Scott_Symons = "min",
    SD = "min",
   #S_Dbw = "min", function used to perform needs to be find
    Silhouette = "max",
    Tau = "max",
    Trace_W = "maxd2",
    Trace_WiB = "mind2",
    Wemmert_Gancarski = "max",
    Xie_Beni = "min",
    KL = "max",
    Gap = "nonneg",
    Hartigan = "less10",
    BIC = "mind2"
  )
}


#' Compute Internal Clustering Validity Criterion
#'
#' Internal utility function to compute a specified internal clustering validation index,
#' using either the \pkg{clusterCrit} or \pkg{clusterSim} packages.
#'
#' @param data Numeric matrix of observations (e.g., term trajectories).
#' @param clustering Integer vector of cluster assignments.
#' @param criterion Character. Name of the criterion to compute.
#'
#' @return A numeric value corresponding to the criterion.
#'

computeCriterion <- function(data, clustering, criterion) {
  if (criterion %in% clusterCrit::getCriteriaNames(isInternal = TRUE)) {
    intcrit <- clusterCrit::intCriteria(
      traj = data,
      part = as.integer(unname(clustering)),
      crit = criterion
    )
    return(intcrit[[1]])
  }

  # clusterSim ## index.SDbw deprecated
  # if (criterion == "S_Dbw") {
  #   return(clusterSim::index.SDbw(data, clustering, centrotypes = "centroids"))
  # }

  if (criterion == "Point_Biserial") {
    return(clusterSim::index.G1(data, clustering))
  }

  stop(paste("Unsupported criterion:", criterion))
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
    c("#6C7B8B", "#8470FF","#00C5CD", "#54FF9F" ,# for zone
      "#CD3700", "#FFA500","#FF83FA", "#000000",# for keywords
      "#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#A65628","#F781BF","#999999","#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F"
      ,"#B3B3B3","#A6CEE3","#1F78B4","#B2DF8A","#33A02C", "#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#B15928","#8DD3C7","#BEBADA"
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
