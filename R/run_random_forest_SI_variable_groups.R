# FIT SPATIAL NESTED RANDOM FOREST FROM FINAL MODELING VARS

run_random_forest_SI_variable_groups <- function(dat_trans,
                                                 modeling_vars_final){
  
  
  # transform response variables given skewness
  dat_trans$l_ch4_total = log1p(dat_trans$ch4_total)
  dat_trans$l_ch4_diffusion = log1p(dat_trans$ch4_diffusion)
  dat_trans$l_ch4_ebullition = log1p(dat_trans$ch4_ebullition)
  
  morpho_var_swap <- c("shoreline_development",
                       "dynamic_ratio")
  
  random_forest_variable_group = list()
  
  for(i in 1:length(morpho_var_swap)){
    
    swap_local <- rapply(modeling_vars_final, function(x) replace(x, x == "surface_area", morpho_var_swap[i]), how = "replace")
    swap_local_global <- rapply(swap_local, function(x) replace(x, x == "surface_area_global", morpho_var_swap[i]), how = "replace")
    
    # project to UTM
    dat_trans <- st_as_sf(
      dat_trans,
      coords = c("long", "lat"),
      crs = 4326
    ) %>%
      st_transform(crs = 5070) # Albers equal area conus
    
    formula_list <- swap_local_global %>%
      # Combine model variables into string with "+"
      # map_depth applies function to specified level of nested list
      map_depth(., .depth = 2, ~ paste0(., collapse = " + ")) %>%
      
      # simplify list structure
      unlist(., recursive = FALSE) %>%
      
      # Now add response variables -- use log-transformed version
      # within an imap function, .y is the name of the list element
      imap(., ~ if (grepl("diffusion", .y)) { # if element name contains diffusion
        paste0("l_ch4_diffusion ~ ", .) # then paste diffusion as response
      } else if (grepl("ebullition", .y)) {
        paste0("l_ch4_ebullition ~ ", .)
      } else if (grepl("total", .y)) {
        paste0("l_ch4_total ~ ", .)
      } 
      ) %>%
      map(., ~ as.formula(.)) # convert to formula
    
  
    set.seed(102322)
    random_forest_object <- map(formula_list, \(x) spmodel::splmRF(x, 
                                                                   data = dat_trans %>% 
                                                                     select(all.vars(x), lake_id, sample_month) %>% 
                                                                     na.omit(.), 
                                                                   random = ~ (1 | lake_id) + (1 | sample_month),
                                                                   spcov_type = "exponential", # coords inherited from sf object
                                                                   quantreg = T,
                                                                   keep.inbag = T,
                                                                   na.action = "na.omit",
                                                                   importance = "permutation",
                                                                   local = TRUE # faster performance, omit for final model
    ) 
    )
    
    names(random_forest_object) <- paste0(names(random_forest_object), "__", morpho_var_swap[i])
    
    random_forest_variable_group <- c(random_forest_variable_group, random_forest_object)
  }
  
  return(random_forest_variable_group)
  
}