#' Create a ggplot of term curves by faceting
#'
#' Draw curves of term frequency.
#' @param data A (molten) dataframe of the term by documents table.
#' @param norm Logical value: true (by default) if frequencies have been normalized.
#' @param keyword_selection A list...
#' @param r A scalar for thinning labels of time x-scale.
#' @param scales Type of scale for facets: "fixed" (the default) or "free"
#' @param leg Whether legend must be drawn (T is default) or not
#' @param themety Type of theme.
#' @param size_class size of lines per frequency class.
#' @param x_lab x-axis label: "year" (default), user can set a more complete label (e.g., "year/volume").
#' @return A ggplot object.
#' @export
#' @examples
#' # facetPlot(dat_df)


facetPlot <- function(data, keyword_selection = list(type="frequency", n=3, kw.list=NULL),
                       r = 4, scales = "fixed", leg = TRUE, themety = "light",
                       size_class = NULL, x_lab = "year"){
  y_lab <- ifelse(data$norm, "keyword (normalized) frequency", "keyword frequency")

  zone_levels <- levels(data$zone)
  n_zones <- length(zone_levels)

  m <- keyword_selection$n

  if(themety=="light") {
    col_class <- setNames(data$colors_light, zone_levels)
    if(is.null(size_class)) size_class <- c(.35,.35,.2,.1)
  } else {
    col_class <- setNames(data$colors_dark, zone_levels)
  }
  col_class <- paste0(col_class, "70") # alpha
  col_kw <- colorlist(type=themety)[n_zones + seq(1, m)]

  d <- data$tdm_long

  type <- keyword_selection$type

  switch (type,
          "random" = {
            kw <- d %>%
              select(keyword,zone) %>%
              distinct() %>%
              group_by(zone) %>%
              slice_sample(n = m) %>%
              ungroup() %>%
              mutate(kw_color = rep(col_kw,n_zones),
                     zone_color = rep(col_class,rep(m,n_zones)),
                     id = rep(1:m,n_zones)) %>%
              arrange(id) %>%
              select(-id)
          },
          "frequency" = {
            kw <- d %>%
              select(keyword,zone,tot_freq) %>%
              distinct() %>%
              group_by(zone) %>%
              arrange(desc(tot_freq)) %>%
              slice_head(n = m) %>%
              ungroup() %>%
              select(zone, keyword) %>%
              mutate(kw_color = rep(col_kw,n_zones),
                     zone_color = rep(col_class,rep(m,n_zones)),
                     id = rep(1:m,n_zones)) %>%
              arrange(id) %>%
              select(-id)
          },
          "list" ={
            kw <- tibble(keyword=keyword_selection$kw.list) %>%
              left_join(d %>% select(keyword,zone) %>% distinct(),
                        by = "keyword") %>%
              select(zone, keyword) %>%
              mutate(kw_color = rep(col_kw,n_zones),
                     zone_color = rep(col_class,rep(m,n_zones)),
                     id = rep(1:m,n_zones)) %>%
              arrange(id) %>%
              select(-id)
          }
  )

  kw_df <- d %>% filter(keyword %in% kw$keyword) %>%
    left_join(kw %>% select(keyword, kw_color, zone_color), by = "keyword")

  # Assicurati che l’ordine sia quello corretto (come appare in kw)
  ordered_keywords <- kw$keyword

  # Imposta l’ordine manuale
  kw_df$keyword <- factor(kw_df$keyword, levels = ordered_keywords)

  year <- data$corpus_info %>%  select(years) %>% pull() %>% unique()
  n_d <- length(year)
  n_y <- diff(range(year))+1
  xaxlab <- year[1]+0:(n_y-1)
  xaxlab[-seq(1, n_y, by=r)] <- ""

  base_theme <- if (themety == "light") theme_classic() else theme_dark()

  # Colori per le zone
  zone_colors <- setNames(col_class, levels(d$zone))

  # Colori per le keyword (già associati nella tua colonna `kw_color`)
  kw_colors <- setNames(kw_df$kw_color, kw_df$keyword)

  # Combina i due
  all_colors <- c(zone_colors, kw_colors)

  opts <- base_theme +
    theme(plot.margin = unit(c(0.0,0.0,0.2,0),"line"),
          axis.text = element_text(angle=90),
          axis.text.x = element_text(vjust=0.5),
          axis.text.y = element_text(hjust=0.5),
          legend.position = if (leg) "bottom" else "none",
          legend.key = element_rect(colour = NA, fill = NA),
          legend.key.width=unit(3,"lines"),
          legend.key.height=unit(.6,"lines"),
          legend.text = element_text(size=rel(.95)),
          legend.box.spacing =unit(0.0,"cm"),
          panel.grid = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  p <- ggplot(d, aes(x=chrono,y=freq,group=keyword)) +
    xlab(x_lab) +
    ylab(y_lab) +
    scale_x_continuous(expand=c(0.0065,0.0), breaks=1:n_y, labels=xaxlab) +
    scale_y_continuous(expand=c(0.01,0.0)) + opts

  if(scales=="fixed"){
    p <- p +
      facet_wrap(~zone,nrow=2) +
      geom_line(data = d, aes(colour = zone), size = .1) +
      geom_line(data = kw_df, aes(colour = keyword)) +
      scale_color_manual(values = all_colors)
  } else {
    p <- p +
      facet_wrap(~zone, nrow = 2, scales = "free_y") +
      geom_line(data = d, aes(colour = zone), size = .1) +
      geom_line(data = kw_df, aes(colour = keyword)) +
      scale_color_manual(values = all_colors)
  }

  return(p)
}



