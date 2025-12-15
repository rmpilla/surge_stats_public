# Screen for outliers and transform as needed

trans_dat <- function(dat_impute, modeling_vars){
  
  
  # 1. Morphology variables----
  
  # inspect for outliers
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
  
  # 2. trophic variables----
  
  # inspect for outliers
  # interactive cleveland dotplots
  {dat_impute %>% 
      select(
        lake_id, 
        all_of(
          unname( # must remove name attribute for all_of
            modeling_vars$trophic_vars
          )
        )
      ) %>% 
      # some variables are repeated for each site in a lake
      # filter to unique value to simplify the plot
      distinct() %>% 
      pivot_longer(cols = !lake_id, names_to = "trophic_var", values_to = "value") %>%
      mutate(f_lake_id = as.factor(lake_id)) %>% # better spacing along y-axis
      ggplot(aes(value, f_lake_id)) +
      geom_point() +
      facet_wrap(~ trophic_var, scales = "free_x") +
      # suppress y-axis tick labels
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.spacing.y = unit(1.5, "cm"))
  } %>%
    ggplotly()
  
  # The following have outliers.
  # log transform the variables to reduce the influence of outliers.
  trophic_to_trans <- c(
    "op_shallow", # lake_id 231
    "tp_shallow", # lake_id 231
    "chla_sonde_shallow" # lake_id 166, but has zeroes, must add small constant
  )
  
  # log10 transform variables with outliers and inspect distribution
  {dat_impute %>%
      mutate(
        across(all_of(trophic_to_trans),
               ~ log10(.x), .names = "l_{.col}")
      ) %>%
      select(lake_id, starts_with("l_")) %>% 
      # trophic variables are repeated for each site in a lake
      # filter to unique value to simplify the plot
      distinct() %>% 
      pivot_longer(cols = !lake_id, names_to = "trophic_var", values_to = "value") %>%
      mutate(f_lake_id = as.factor(lake_id)) %>% # better spacing along y-axis
      ggplot(aes(value, f_lake_id)) +
      geom_point() +
      facet_wrap(~ trophic_var, scales = "free_x") +
      # suppress y-axis tick labels
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.spacing.y = unit(1.5, "cm"))} %>%
    ggplotly()
  
  # much better
  # Implement transformations in main data frame
  dat_trans <- dat_trans %>%
    mutate(
      across(all_of(trophic_to_trans[!trophic_to_trans == "chla_sonde_shallow"]), # omit "chla_sonde_shallow"
             ~ log10(.x), .names = "l_{.col}"),
      l_chla_sonde_shallow = log10(chla_sonde_shallow + 0.001) # add small positive for negative values
      )
  
  
  # 3. climate variables----
  
  # inspect for outliers
  # interactive cleveland dotplots
  {dat_trans %>% 
      select(
        lake_id, 
        all_of(
          unname( # must remove name attribute for all_of
            modeling_vars$clim_vars
          )
        )
      ) %>% 
      # morpho variables are repeated for each site in a lake
      # filter to unique value to simplify the plot
      distinct() %>% 
      pivot_longer(cols = !lake_id, names_to = "climate_var", values_to = "value") %>%
      mutate(f_lake_id = as.factor(lake_id)) %>% # better spacing along y-axis
      ggplot(aes(value, f_lake_id)) +
      geom_point() +
      facet_wrap(~ climate_var, scales = "free_x") +
      # suppress y-axis tick labels
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.spacing.y = unit(1.5, "cm"))
  } %>%
    ggplotly()
  
  # The following have outliers.
  # log transform the variables to reduce the influence of outliers.
  clim_to_trans <- c(
    "precipitation_chamber", # 146, 47
    "precipitation_trap", # 147, 148
    "wind_speed_chamber" # 68, 69, 45
  )
  
  # log10 transform variables with outliers and inspect distribution
  {dat_impute %>%
      mutate(
        across(all_of(clim_to_trans),
               ~ log10(.x + 0.001), .names = "l_{.col}") # small constant for 0
      ) %>%
      select(lake_id, starts_with("l_")) %>% 
      # climate variables are repeated across many/all sites in a lake
      # filter to unique value to simplify the plot
      distinct() %>% 
      pivot_longer(cols = !lake_id, names_to = "climate_var", values_to = "value") %>%
      mutate(f_lake_id = as.factor(lake_id)) %>% # better spacing along y-axis
      ggplot(aes(value, f_lake_id)) +
      geom_point() +
      facet_wrap(~ climate_var, scales = "free_x") +
      # suppress y-axis tick labels
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.spacing.y = unit(1.5, "cm"))} %>%
    ggplotly()
  
  # better
  # Implement transformations in main data frame
  dat_trans <- dat_trans %>%
    mutate(
      across(all_of(clim_to_trans),
             ~ log10(.x + 0.001), .names = "l_{.col}")
    )
  
  
  # 4. temp variables----
  
  # inspect for outliers
  # interactive cleveland dotplots
  {dat_trans %>% 
      select(
        lake_id, 
        all_of(
          unname( # must remove name attribute for all_of
            modeling_vars$temp_vars
          )
        )
      ) %>% 
      distinct() %>% 
      pivot_longer(cols = !lake_id, names_to = "temp_var", values_to = "value") %>%
      mutate(f_lake_id = as.factor(lake_id)) %>% # better spacing along y-axis
      ggplot(aes(value, f_lake_id)) +
      geom_point() +
      facet_wrap(~ temp_var, scales = "free_x") +
      # suppress y-axis tick labels
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.spacing.y = unit(1.5, "cm"))
  } %>%
    ggplotly()
  
  # No outliers notes
 
  
  # 5. hydrology variables----
  
  # inspect for outliers
  # interactive cleveland dotplots
  {dat_trans %>% 
      select(
        lake_id, 
        all_of(
          unname( # must remove name attribute for all_of
            modeling_vars$hydrol_vars[1:2] # numeric variables
          )
        )
      ) %>% 
      distinct() %>% 
      pivot_longer(cols = !lake_id, names_to = "hydrol_var", values_to = "value") %>%
      mutate(f_lake_id = as.factor(lake_id)) %>% # better spacing along y-axis
      ggplot(aes(value, f_lake_id)) +
      geom_point() +
      facet_wrap(~ hydrol_var, scales = "free_x") +
      # suppress y-axis tick labels
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.spacing.y = unit(1.5, "cm"))
  } %>%
    ggplotly()
  
  
  # The following have outliers.
  # log transform the variables to reduce the influence of outliers.
  hydrol_to_trans <- c(
    "residence_time" # 296
  )
  
  # log10 transform variables with outliers and inspect distribution
  {dat_impute %>%
      mutate(
        across(all_of(hydrol_to_trans),
               ~ log10(.x), .names = "l_{.col}") # small constant for 0
      ) %>%
      select(lake_id, starts_with("l_")) %>% 
      # climate variables are repeated across many/all sites in a lake
      # filter to unique value to simplify the plot
      distinct() %>% 
      pivot_longer(cols = !lake_id, names_to = "hydrol_var", values_to = "value") %>%
      mutate(f_lake_id = as.factor(lake_id)) %>% # better spacing along y-axis
      ggplot(aes(value, f_lake_id)) +
      geom_point() +
      facet_wrap(~ hydrol_var, scales = "free_x") +
      # suppress y-axis tick labels
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.spacing.y = unit(1.5, "cm"))} %>%
    ggplotly()
  
  # not much better
  # Implement transformations in main data frame
  dat_trans <- dat_trans %>%
    mutate(
      across(all_of(hydrol_to_trans),
             ~ log10(.x), .names = "l_{.col}")
    )
  
  # 6. chemical vars----
  # inspect for outliers
  # interactive cleveland dotplots
  {dat_trans %>% 
      select(
        lake_id, 
        all_of(
          unname( # must remove name attribute for all_of
            modeling_vars$chem_vars
          )
        )
      ) %>% 
      distinct() %>% 
      pivot_longer(cols = !lake_id, names_to = "chem_var", values_to = "value") %>%
      mutate(f_lake_id = as.factor(lake_id)) %>% # better spacing along y-axis
      ggplot(aes(value, f_lake_id)) +
      geom_point() +
      facet_wrap(~ chem_var, scales = "free_x") +
      # suppress y-axis tick labels
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.spacing.y = unit(1.5, "cm"))
    } %>%
    ggplotly()
  
  
  # The following have outliers.
  # log transform the variables to reduce the influence of outliers.
  chem_to_trans <- c(
    "nh4_shallow", # 1017, 166, 79
    "so4_shallow", # 188, 78, 75, 49
    "toc_shallow" # 79, 78, 75
  )
  
  # log10 transform variables with outliers and inspect distribution
  {dat_impute %>%
      mutate(
        across(all_of(chem_to_trans),
               ~ log10(.x), .names = "l_{.col}") # small constant for 0
      ) %>%
      select(lake_id, starts_with("l_")) %>% 
      # climate variables are repeated across many/all sites in a lake
      # filter to unique value to simplify the plot
      distinct() %>% 
      pivot_longer(cols = !lake_id, names_to = "chem_var", values_to = "value") %>%
      mutate(f_lake_id = as.factor(lake_id)) %>% # better spacing along y-axis
      ggplot(aes(value, f_lake_id)) +
      geom_point() +
      facet_wrap(~ chem_var, scales = "free_x") +
      # suppress y-axis tick labels
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.spacing.y = unit(1.5, "cm"))} %>%
    ggplotly()
  
  # better
  # Implement transformations in main data frame
  dat_trans <- dat_trans %>%
    mutate(
      across(all_of(chem_to_trans),
             ~ log10(.x), .names = "l_{.col}")
    )
  
  
  # 7. other vars----
  # inspect for outliers
  # interactive cleveland dotplots
  {dat_impute %>% 
      select(
        lake_id, 
        all_of(
          unname( # must remove name attribute for all_of
            modeling_vars$other_vars
          )
        )
      ) %>% 
      select(lake_id, where(is.numeric)) %>% # ag_eco9 is factor
      distinct() %>% 
      pivot_longer(cols = !lake_id, names_to = "other_var", values_to = "value") %>%
      mutate(f_lake_id = as.factor(lake_id)) %>% # better spacing along y-axis
      ggplot(aes(value, f_lake_id)) +
      geom_point() +
      facet_wrap(~ other_var, scales = "free_x") +
      # suppress y-axis tick labels
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.spacing.y = unit(1.5, "cm"))
  } %>%
    ggplotly()
  
  
  # The following have outliers.
  # log transform the variables to reduce the influence of outliers.
  other_to_trans <- c(
    "year_completed" # a few < 1850!
  )
  
  # log10 transform variables with outliers and inspect distribution
  {dat_impute %>%
      mutate(
        l_year_completed = log10(year_completed)
      ) %>%
      select(lake_id, starts_with("l_")) %>% 
      # climate variables are repeated across many/all sites in a lake
      # filter to unique value to simplify the plot
      distinct() %>% 
      pivot_longer(cols = !lake_id, names_to = "other_var", values_to = "value") %>%
      mutate(f_lake_id = as.factor(lake_id)) %>% # better spacing along y-axis
      ggplot(aes(value, f_lake_id)) +
      geom_point() +
      facet_wrap(~ other_var, scales = "free_x") +
      # suppress y-axis tick labels
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            panel.spacing.y = unit(1.5, "cm"))} %>%
    ggplotly()
  
  # no improvement. Don't transform
  
# 8. factors----
  # models require nominal vars as factors.
  dat_trans <- dat_trans %>% 
    mutate(
      across(c(lake_id, ag_eco9_nm, ag_eco9, e_i_type), as.factor)
    )
  
  return(dat_trans)

}
