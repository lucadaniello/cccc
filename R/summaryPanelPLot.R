# ## oooooook
#
# # Crea il grafico finale stile pannello
# make_summary_panel <- function(summary_opt, penalty_type, normty) {
#   # Ristruttura in long format
#   df_long <- summary_opt %>%
#     pivot_longer(cols = c(df_gcv, sse, gcv_min, ocv_min),
#                  names_to = "metric", values_to = "value") %>%
#     mutate(metric = recode(metric,
#                            df_gcv = "df",
#                            sse = "sse",
#                            gcv_min = "gcv",
#                            ocv_min = "ocv"))
#
#   highlight <- df_long %>%
#     group_by(metric) %>%
#     filter(value == min(value, na.rm = TRUE)) %>%
#     slice(1) %>%
#     ungroup()
#
#   p <- ggplot(df_long, aes(x = degree, y = value)) +
#     geom_line(aes(group = 1), color = "gray40") +
#     geom_point(color = "black", size = 2) +
#     geom_point(data = highlight, aes(x = degree, y = value), color = "firebrick", size = 3) +
#     geom_hline(data = highlight, aes(yintercept = value), linetype = "dashed", color = "firebrick", linewidth = 0.4) +
#     facet_wrap(~metric, scales = "free_y", ncol = 2) +
#     labs(title = "Optimal Smoothing Summary",
#          x = "Spline degree (m)",
#          y = NULL,
#          subtitle = paste0("Penalty: ", penalty_type, " | Norm: ", normty)) +
#     theme_minimal(base_size = 11) +
#     theme(strip.text = element_text(face = "bold"))
#
#   return(p)
# }
