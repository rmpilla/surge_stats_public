get_ebul_percent <- function(emissions_lake_data,
                             lake_polygons, 
                             dat_trans){
  
  lake_scale_percent_ebul <- lake_polygons %>%
    sf::st_centroid(.) %>%
    sf::st_transform(., crs = 4326) %>%
    right_join(emissions_lake_data) %>%
    mutate(long = sf::st_coordinates(.)[, 1],
           lat = sf::st_coordinates(.)[, 2],
           lake_id = factor(lake_id),
           visit = factor(visit)) %>%
    left_join(dat_trans %>%
                select(lake_id, visit, so4_shallow, mean_depth) %>%
                distinct()) %>%
    mutate(ebul_diff_ratio = ch4_ebullition_lake / ch4_diffusion_lake,
           percent_ebul = ch4_ebullition_lake / (ch4_ebullition_lake + ch4_diffusion_lake))
  


  return(lake_scale_percent_ebul)
  
}