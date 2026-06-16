make_map_figure <- function(emissions_lake_data,
                            lake_polygons){
  
  polygons_emissions <- lake_polygons %>%
    sf::st_centroid(lake_polygons) %>%
    right_join(emissions_lake_data) %>%
    mutate(lon = sf::st_coordinates(.)[, 1],
           lat = sf::st_coordinates(.)[, 2])
  
  
  us_states <- tigris::states(cb = TRUE, 
                              resolution = "20m") %>%
    filter(!NAME %in% c("Alaska", "Hawaii")) %>%
    sf::st_transform(crs = sf::st_crs(polygons_emissions))
  
  
  OhInKy_map <- us_states %>% 
    filter(NAME %in% c("Ohio", "Indiana", "Kentucky"))
  
  OhInKy_emissions <- polygons_emissions %>%
    sf::st_filter(OhInKy_map) %>%
    mutate(lon = sf::st_coordinates(.)[, 1],
           lat = sf::st_coordinates(.)[, 2])
  
  bbox = sf::st_bbox(OhInKy_map) %>%
    c() %>% as.data.frame() %>% t()

  
  ecoregion <- st_read("input_data/aggr_ecoregions/aggr_ecoregions_simple.shp") %>%
    st_make_valid()
  
  # Check CRS
  st_crs(ecoregion)
  ecoregion <- st_transform(ecoregion, st_crs(polygons_emissions))

  # SET UP CUSTOM COLORS FOR ECOREGIONS---------
  # Custom color pallette for ecoregion polygons.  Attempted to mirror
  # https://www.epa.gov/national-aquatic-resource-surveys/
  # ecoregional-results-national-lakes-assessment-2012
  ecoregion <- left_join(ecoregion,
                         tribble(~ WSA9_NAME, ~ cols,
                                 "Coastal Plains", "orange1",
                                 "Northern Appalachians", "lightpink1",
                                 "Northern Plains", "darksalmon",
                                 "Southern Appalachians", "mediumturquoise",
                                 "Southern Plains", "khaki4",
                                 "Temperate Plains", "forestgreen", 
                                 "Upper Midwest", "deepskyblue4",
                                 "Western Mountains", "saddlebrown",
                                 "Xeric", "lightskyblue4"))
  
  ecoregion_OhInKy <- ecoregion %>%
    st_intersection(OhInKy_map)
  
  
  us_pr_map_plot <- ggplot() +
    geom_sf(data = ecoregion, aes(fill = WSA9_NAME), alpha = 0.5) +
    scale_fill_manual(name = "Eco-Region:", values = unique(ecoregion$cols)) +
    guides(fill = guide_legend(title.position = "top", ncol = 2)) +
    
    geom_sf(data = us_states, alpha = 0) +
    geom_rect(data = bbox, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "black", linewidth = 1.25, fill = NA) +
    
    ggnewscale::new_scale_fill() +
    scatterpie::geom_scatterpie(data = polygons_emissions %>% as.data.frame(), 
                                cols = c("ch4_diffusion_lake", "ch4_ebullition_lake"),
                                aes(x = lon, y = lat, r = log10(ch4_total_lake) * 5e4)) +
    scale_fill_manual(values = c("grey20", "grey80"),  #"#d95f02", "#7570b3"),
                      name = bquote(CH[4]~"emission mechanism:"),
                      labels = c("diffusion", "ebullition")) +
    scatterpie::geom_scatterpie_legend(breaks = c(0, 2, 4) * 5e4, radius = c(0, 1, 2, 3, 4) * 5e4,
                                       labeller = function(x){10^(x / 5e4)},
                                       x = -13500000, y = 2700000) +
    annotate(geom = "text", x = -13100000, y = 3200000, size = 4, label = deparse(bquote(mg~CH[4]~m^-2~d^-1)), parse = T) +
    guides(fill = guide_legend(title.position = "top", ncol = 1)) +
    labs(x = NULL, y = NULL, title = "A") +
    theme_bw() +
    theme(legend.position = "right",
          legend.title = element_text(hjust = 0.5))
  
  
  legend_plot <- ggpubr::as_ggplot(ggpubr::get_legend(us_pr_map_plot))
  
  us_pr_map_plot <- us_pr_map_plot +
    theme(legend.position = "none")
  
  
  OhInKy_map_plot <- ggplot() +
    geom_sf(data = ecoregion_OhInKy, aes(fill = WSA9_NAME), alpha = 0.5) +
    scale_fill_manual(name = "Eco-Region:", values = unique(ecoregion$cols)) +
    guides(fill = guide_legend(title.position = "top", ncol = 2)) +
    
    geom_sf(data = OhInKy_map, alpha = 0) +
    scatterpie::geom_scatterpie(data = OhInKy_emissions %>% as.data.frame(), 
                                cols = c("ch4_diffusion_lake", "ch4_ebullition_lake"),
                                aes(x = lon, y = lat, r = log10(ch4_total_lake) * 1e4)) +
    scale_fill_manual(values = c("grey20", "grey80", #"#d95f02", "#7570b3",
                                 "orange1", "lightpink1", "mediumturquoise", "forestgreen", "deepskyblue4"), na.value = "grey50",
                      name = bquote(CH[4]~"emission rate (mg"~CH[4]~m^-2~h^-1*"):"),
                      #labels = c("diffusion", "ebullition", unique(ecoregion_OhInKy$WSA9_NAME))
                      guide = "none") +
    scatterpie::geom_scatterpie_legend(breaks = c(0, 2, 4) * 1e4, radius = c(0, 1, 2, 3, 4) * 1e4,
                                       labeller = function(x){ 10^(x / 1e4)},
                                       x = -9920000, y = 4900000) +
    annotate(geom = "text", x = -9878000, y = 4980000, size = 4, label = deparse(bquote(mg~CH[4]~m^-2~d^-1)), parse = T) +
    guides(fill = guide_legend(title.position = "top", ncol = 1)) +
    labs(x = NULL, y = NULL, title = "B") +
    theme_bw() +
    theme(legend.position = "none")
  
  
  
  ## previous version WITHOUT pie-chart points
  
  # us_pr_map_plot <- ggplot() +
  #   geom_sf(data = us_states) +
  #   geom_rect(data = bbox, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "black", linewidth = 1.25, fill = NA) +
  #   geom_sf(data = polygons_emissions, aes(fill = ch4_total_lake, size = ch4_total_lake), shape = 21) +
  #   scale_fill_viridis_c(trans = "log10",
  #                        breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000), na.value = "grey50",
  #                        name = bquote("total"~CH[4]~"emission rate (mg"~CH[4]~m^-2~h^-1*"):")) +
  #   scale_size_continuous(range = c(0.75, 3), trans = "log10",
  #                         breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000),
  #                         name = bquote("total"~CH[4]~"emission rate (mg"~CH[4]~m^-2~h^-1*"):")) +
  #   guides(fill = guide_legend(title.position = "top"),
  #          size = guide_legend(title.position = "top")) +
  #   labs(title = "A") +
  #   theme_bw()
  # 
  # OhInKy_map_plot <- ggplot() +
  #   geom_sf(data = OhInKy_map) +
  #   geom_sf(data = OhInKy_emissions, aes(fill = ch4_total_lake, size = ch4_total_lake), shape = 21) +
  #   scale_fill_viridis_c(trans = "log10",
  #                        breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000), na.value = "grey50",
  #                        limits = range(polygons_emissions$ch4_total_lake, na.rm = T),
  #                        name = bquote("total"~CH[4]~"emission rate (mg"~CH[4]~m^-2~h^-1*"):")) +
  #   scale_size_continuous(range = c(0.75, 3), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000), trans = "log10",
  #                         limits = range(polygons_emissions$ch4_total_lake, na.rm = T),
  #                         name = bquote("total"~CH[4]~"emission rate (mg"~CH[4]~m^-2~h^-1*"):")) +
  #   guides(fill = guide_legend(title.position = "top"),
  #          size = guide_legend(title.position = "top")) +
  #   labs(title = "B") +
  #   theme_bw()
  
  map_plot_figure <- us_pr_map_plot /
    (OhInKy_map_plot + legend_plot) +
    plot_layout(heights = c(1, 0.75))

    
  return(map_plot_figure)
  
}
