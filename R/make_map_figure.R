make_map_figure <- function(emissions_lake_data,
                            lake_polygons){
  
  polygons_emissions <- lake_polygons %>%
    sf::st_centroid(lake_polygons) %>%
    right_join(emissions_lake_data)
  
  
  us_states <- tigris::states(cb = TRUE, 
                              resolution = "20m") %>%
    filter(!NAME %in% c("Alaska", "Hawaii")) %>%
    sf::st_transform(crs = sf::st_crs(polygons_emissions))
  
  
  OhInKy_map <- us_states %>% 
    filter(NAME %in% c("Ohio", "Indiana", "Kentucky"))
  
  OhInKy_emissions <- polygons_emissions %>%
    sf::st_filter(OhInKy_map)
  
  bbox = sf::st_bbox(OhInKy_map) %>%
    c() %>% as.data.frame() %>% t()
  
  
  us_pr_map_plot <- ggplot() +
    geom_sf(data = us_states) +
    geom_rect(data = bbox, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "black", linewidth = 1, fill = NA) +
    geom_sf(data = polygons_emissions, aes(fill = ch4_total_lake, size = ch4_total_lake), shape = 21) +
    scale_fill_distiller(trans = "log10", palette = "YlOrRd", direction = 1,
                         breaks = c(1, 3, 10, 30, 100, 300, 1000), na.value = "grey50",
                         name = bquote(CH[4]~"emission rate:")) +
    scale_size_continuous(range = c(0.75, 3), trans = "log10",
                          breaks = c(1, 3, 10, 30, 100, 300, 1000),
                          name = bquote(CH[4]~"emission rate:")) +
    guides(fill = guide_legend(title.position = "top"),
           size = guide_legend(title.position = "top")) +
    labs(title = "a)") +
    theme_bw()
  
  
  
  OhInKy_map_plot <- ggplot() +
    geom_sf(data = OhInKy_map) +
    geom_sf(data = OhInKy_emissions, aes(fill = ch4_total_lake, size = ch4_total_lake), shape = 21) +
    scale_fill_distiller(trans = "log10", palette = "YlOrRd", direction = 1,
                         breaks = c(1, 3, 10, 30, 100, 300, 1000), na.value = "grey50",
                         limits = range(polygons_emissions$ch4_total_lake, na.rm = T),
                         name = bquote(CH[4]~"emission rate:")) +
    scale_size_continuous(range = c(0.75, 3), breaks = c(1, 3, 10, 30, 100, 300, 1000), trans = "log10",
                          limits = range(polygons_emissions$ch4_total_lake, na.rm = T),
                          name = bquote(CH[4]~"emission rate:")) +
    guides(fill = guide_legend(title.position = "top"),
           size = guide_legend(title.position = "top")) +
    labs(title = "b)") +
    theme_bw()
  
  
  map_plot_figure <- (us_pr_map_plot + OhInKy_map_plot) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  return(map_plot_figure)
  
}
