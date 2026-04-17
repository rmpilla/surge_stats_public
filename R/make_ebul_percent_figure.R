make_ebul_percent_figure <- function(lake_scale_percent_ebul){
  
  
  long_vs_percEbul <- ggplot() +
    geom_hline(yintercept = 0.5, linetype = 2) +
    geom_smooth(data = lake_scale_percent_ebul, aes(x = long, y = percent_ebul), method = "loess", span = 0.95, color = "black") +
    geom_point(data = lake_scale_percent_ebul, aes(x = long, y = percent_ebul, fill = so4_shallow), 
               shape = 21, size = 3) +
    scale_x_continuous(breaks = seq(-120, -60, by = 10), labels = paste0(abs(seq(-120, -60, by = 10)), "°W")) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_viridis_c(trans = "log10", na.value = "white", name = bquote(SO[4]~"(mg "~L^-1*"):"),
                         labels = scales::comma_format(big.mark = ",")) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = bquote("Longtitude"), y = "Percent of total emission as ebullition", title = "A") +
    theme_bw()+
    theme(legend.position = "bottom",
          legend.key.width = unit(1, units = "cm"),
          legend.title = element_text(vjust = 1))
  
  
  
  # long_vs_so4 <- ggplot() +
  #   geom_smooth(data = lake_scale_percent_ebul, aes(x = long, y = so4_shallow), method = "loess", span = 0.95, color = "black") +
  #   geom_point(data = lake_scale_percent_ebul, aes(x = long, y = so4_shallow, fill = percent_ebul), 
  #              shape = 21, size = 3) +
  #   scale_x_continuous(breaks = seq(-120, -60, by = 10), labels = paste0(abs(seq(-120, -60, by = 10)), "°W")) +
  #   scale_y_continuous(trans = "log10", labels = scales::comma_format(big.mark = ",")) +
  #   #scale_fill_viridis_c(na.value = "white", labels = scales::percent) +
  #   scale_fill_distiller(palette = "RdGy", limits = c(0, 1), name = "Percent of\ntotal emissions\nas ebullition:",
  #                        labels = scales::percent) +
  #   labs(x = "Longtitude", y = bquote(SO[4]~"(mg "~L^-1*")")) +
  #   theme_bw() +
  #   theme(legend.position = "top")
  
  
  # long_vs_meanDepth <- ggplot() +
  #   geom_smooth(data = lake_scale_percent_ebul, aes(x = long, y = mean_depth), method = "loess", span = 0.95, color = "black") +
  #   geom_point(data = lake_scale_percent_ebul, aes(x = long, y = mean_depth, fill = percent_ebul), 
  #              shape = 21, size = 3) +
  #   scale_x_continuous(breaks = seq(-120, -60, by = 10), labels = paste0(abs(seq(-120, -60, by = 10)), "°W")) +
  #   #scale_y_continuous(trans = "log10") +
  #   #scale_fill_viridis_c(na.value = "white", labels = scales::percent) +
  #   scale_fill_distiller(palette = "RdGy", limits = c(0, 1)) +
  #   labs(x = "Longtitude", y = "Mean depth (m)") +
  #   theme_bw()
  
  
  
  
  so4_vs_percEbul <- ggplot() +
    geom_hline(yintercept = 0.5, linetype = 2) +
    geom_smooth(data = lake_scale_percent_ebul, aes(x = so4_shallow, y = percent_ebul), method = "loess", span = 0.95, color = "black") +
    geom_point(data = lake_scale_percent_ebul, aes(x = so4_shallow, y = percent_ebul), 
               shape = 1, stroke = 1, size = 3) +
    scale_x_continuous(trans = "log10", labels = scales::comma_format(big.mark = ",")) +
    scale_y_continuous(labels = scales::percent) +
    #scale_fill_viridis_c(na.value = "white") +
    labs(x = bquote(SO[4]~"(mg "~L^-1*")"), y = "Percent of total emission as ebullition", title = "B") +
    coord_cartesian(ylim = c(0, 1)) +
    theme_bw()
  
  
  # meanDepth_vs_percEbul <- ggplot() +
  #   geom_hline(yintercept = 0.5, linetype = 2) +
  #   geom_smooth(data = lake_scale_percent_ebul, aes(x = mean_depth, y = percent_ebul), method = "loess", span = 0.95, color = "black") +
  #   geom_point(data = lake_scale_percent_ebul, aes(x = mean_depth, y = percent_ebul),
  #              shape = 1, stroke = 1, size = 3) +
  #   scale_x_continuous(labels = scales::comma_format(big.mark = ",")) +
  #   scale_y_continuous(labels = scales::percent) +
  #   #scale_fill_viridis_c(na.value = "white") +
  #   labs(x = bquote("Mean depth (m)"), y = "Percent of total emission as ebullition") +
  #   coord_cartesian(ylim = c(0, 1)) +
  #   theme_bw() +
  #   theme(legend.position = "top")
  
  
  
  ebul_long_so4_figure <- (long_vs_percEbul + so4_vs_percEbul)
  
  return(ebul_long_so4_figure)
  
}