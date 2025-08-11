# Screen for outliers and transform as needed

trans_dat <- function(dat_raw, modeling_vars){
  # Morphology variables----

  # 2. INSPECT FOR OUTLIERS-------------
  # interactive cleveland dotplots
  p_morpho <- dat_raw %>% 
    select(lake_id, all_of(morpho_vars)) %>% 
    # morpho variables are repeated for each site in a lake
    # filter to unique value to simplify the plot
    distinct() %>% 
    pivot_longer(cols = !lake_id, names_to = "morpho_var", values_to = "value") %>%
    mutate(f_lake_id = as.factor(lake_id)) %>% # better spacing along y-axis
    ggplot(aes(value, f_lake_id)) +
    geom_point() +
    facet_wrap(~ morpho_var, scales = "free_x") +
    # suppress y-axis tick labels
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank())
  
  ggplotly(p_morpho)
  
  # The following have outliers.
  # The two Missouri River reservoirs are outliers.
  # log transform the variables to reduce the influence of outliers.
  # dynamic_ratio, lake_id: 69
  # max_length, lake_id: 3, 70, 69
  # shoreline_development, lake_id: 69
  # shoreline_length, lake_id: 69, 70
  # max_width, lake_id: 69
  # mean_width, lake_id: 69, 70
  # surface_area, lake_id: 69, 70
  # volume, lake_id: 69, 70
  
  # log10 transform variables with outliers
  dat_morph <- dat_raw %>%
    mutate(across(c(dynamic_ratio, max_length, shoreline_development,
                    shoreline_length, max_width, mean_width, surface_area,
                    volume, site_depth),
                  ~ log10(.x), .names = "l_{.col}"))
  
  # Inspect the transformed variables for outliers again
  p_morpho_log <- dat_morph %>% 
    select(lake_id, starts_with("l_")) %>% 
    # morpho variables are repeated for each site in a lake
    # filter to unique value to simplify the plot
    distinct() %>% 
    pivot_longer(cols = !lake_id, names_to = "morpho_var", values_to = "value") %>%
    mutate(f_lake_id = as.factor(lake_id)) %>% # better spacing along y-axis
    ggplot(aes(value, f_lake_id)) +
    geom_point() +
    facet_wrap(~ morpho_var, scales = "free_x") +
    # suppress y-axis tick labels
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank())
  
  ggplotly(p_morpho_log)
  
  # 69 still sticks out but not as bad. Probably ok
  
  # update list of morp variables
  l_morpho_vars <- c("l_surface_area",         
                     "l_shoreline_length",     
                     "l_shoreline_development",
                     "l_max_width", # Maximum shore to shore distance that is perpendicular to the maximum lake length            
                     "l_mean_width", # Lake surface area divided by fetch        
                     "l_max_length", # longest open water distance of a lake (fetch)     
                     "l_site_depth",
                     "mean_depth",           
                     "max_depth",            
                     "l_volume",               
                     "littoral_fraction",    
                     "circularity", # circular lakes (value approaches 1) vs elongated lakes (value approaches 0).",          
                     "l_dynamic_ratio") 
  
  
  
  
  dat <- dat_raw %>%
    mutate(across(all_of(original_vars$morpho_vars),
                  ~ log10(.x), .names = "l_{.col}"))
  
  return(dat)
}
