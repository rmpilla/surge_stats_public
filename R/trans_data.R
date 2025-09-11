# Screen for outliers and transform as needed

trans_dat <- function(dat_impute, modeling_vars){
  # Morphology variables----
  
  # 2. INSPECT FOR OUTLIERS-------------
  # interactive cleveland dotplots
  {dat_impute %>% 
      select(
        lake_id, 
        all_of(
          unname( # must remove name attribute for all_of
            modeling_vars$morpho_vars
          )
        )
      ) %>% 
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
            axis.title.y = element_blank(),
            panel.spacing.y = unit(1.5, "cm"))
  } %>%
    ggplotly()
  
  # The following have outliers.
  # log transform the variables to reduce the influence of outliers.
  morpho_to_trans <- c(
    #"buoyf", #lake_id: 119, 57, 16. Add 1e-02 before log transforming
    "dynamic_ratio", #lake_id: 69
    "max_length", #lake_id: 3, 70, 69
    "shoreline_development", #lake_id: 69
    "shoreline_length", #lake_id: 69, 70
    "max_width", #lake_id: 69
    "mean_width", #lake_id: 69, 70
    "surface_area", #lake_id: 69, 70
    "volume" #lake_id: 69, 70
  )
  
  
  # log10 transform variables with outliers and inspect distribution
  {dat_impute %>%
      mutate(
        across(all_of(morpho_to_trans),
               ~ log10(.x), .names = "l_{.col}"),
        l_buoyf = log10(buoyf + 1e-2) # add small constant to accomodate negative value
      ) %>%
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
            axis.title.y = element_blank(),
            panel.spacing.y = unit(1.5, "cm"))} %>%
    ggplotly()
  
  # 69 still sticks out but not as bad. Probably ok
  # Implement transformations in main data frame
  dat_trans <- dat_impute %>%
    mutate(
      across(all_of(morpho_to_trans),
             ~ log10(.x), .names = "l_{.col}"),
      l_buoyf = log10(buoyf + 1e-2) # add small constant to accommodate negative value
    )
  
  
  return(dat_trans)

}
