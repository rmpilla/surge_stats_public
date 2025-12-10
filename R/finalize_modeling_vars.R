# FINALIZE LIST OF MODELING VARS

# 1. Update list of modeling vars based on collinearity within predictor groups.
# see get_final_hydrol_vars.R, get_final_morpho_vars.R, get_final_chem_vars.R,
# get_final_clim_vars.R, get_final_temp_vars.R, get_final_trophic_vars.R for
# collinearity assessments.

# 2. Create a list of all possible variable combinations. Necessary because
# we have 3 different collections of morpho vars (morpho_vars_surface_area,
# morpho_var_shoreline, morpho_var_dynamic) adn 2 different collections of
# hydrology vars (hydrol_vars_e_i, hydrol_vars_e_i_type)

# 3. Inspect all groups of predictor variables for collinearity. Remove
# vars as necessary.



# Here we create modeling_vars_final which accounts for data transformations
# and collinearity

finalize_modeling_vars <- function(modeling_vars_update,
                                   final_morpho_vars,
                                   final_hydrol_vars,
                                   final_trophic_vars,
                                   
                                   # Using all climate, chem and temp vars
                                   # final_clim_vars,
                                   # final_chem_vars,
                                   # final_temp_vars
                                   
                                   # need data to assess collinearity among
                                   # all predictors
                                   dat_trans
                                   ) {
 
  # 1. UPDATE LIST OF MODELING VARS BASED ON COLLINEARITY WITHIN PREDICTOR GROUPS---- 
  # start with modeling_vars_update which is a reduced list of vars after 
  # considering missingness and data transformations. See update_modeling_vars.R
  modeling_vars_update2 <- modeling_vars_update
  
  ## MORPHO----
  # finalize morpho modeling vars
  modeling_vars_update2$morpho_vars_surface_area <- final_morpho_vars %>%
    filter(group == "surface_area_group") %>% pull(variable)
  
  modeling_vars_update2$morpho_var_shoreline <- final_morpho_vars  %>%
    filter(group == "shoreline_group") %>% pull(variable)
  
  modeling_vars_update2$morpho_var_dynamic <- final_morpho_vars  %>%
    filter(group == "dynamic_ratio_group") %>% pull(variable)
  
  # delete $morph_var from modeling_vars_update2. It is replaced with above
  modeling_vars_update2$morpho_vars <- NULL
  
  ## HYDROL----
  # finalize hydrol modeling vars
  modeling_vars_update2$hydrol_vars_e_i <- final_hydrol_vars %>%
    filter(group == "e_i_group") %>% pull(variable)
  
  modeling_vars_update2$hydrol_vars_e_i_type <- final_hydrol_vars  %>%
    filter(group == "e_i_type_group") %>% pull(variable)
  
  # delete hydrol_vars, replaced with above
  modeling_vars_update2$hydrol_vars <- NULL
  
  ## CLIMATE----
  # using all variables. no action required
  
  ## CHEMISTRY----
  # using all variables. No action required
  
  ## TEMPERATURE----
  # using all variables. No action required
  
  ## TROPHIC----
  modeling_vars_update2$trophic_vars <- final_trophic_vars %>%
    pull(variable)
  
  ## OTHER VARS----
  # year_completed, ag_eco9_nm, keep both, no action required
  
  
  # 2. CREATE A LIST OF ALL POSSIBLE VARIABLE COMBINATIONS----
  all_vars <- lst(
    # combinations of morpho_SA and both hydrol groups
    sa_e_i = 
      modeling_vars_update2 %>% 
      .[
        names(.) %in% c( # list elements to include
          "trophic_vars", "clim_vars", "temp_vars", "chem_vars", "other_vars",
          "morpho_vars_surface_area", "hydrol_vars_e_i"
        )
      ] %>%
      flatten_chr(),
    
    sa_e_i_type = 
      modeling_vars_update2 %>% 
      .[
        names(.) %in% c( 
          "trophic_vars", "clim_vars", "temp_vars", "chem_vars", "other_vars",
          "morpho_vars_surface_area", "hydrol_vars_e_i_type"
        )
      ] %>%
      flatten_chr(),
    
    # combinations of morpho_shoreline and both hydrol groups
    shoreline_e_i_type = 
      modeling_vars_update2 %>% 
      .[
        names(.) %in% c(
          "trophic_vars", "clim_vars", "temp_vars", "chem_vars", "other_vars",
          "morpho_var_shoreline", "hydrol_vars_e_i_type"
        )
      ] %>%
      flatten_chr(),
    
    shoreline_e_i =
      modeling_vars_update2 %>% 
      .[
        names(.) %in% c(
          "trophic_vars", "clim_vars", "temp_vars", "chem_vars", "other_vars",
          "morpho_var_shoreline", "hydrol_vars_e_i"
        )
      ] %>%
      flatten_chr(),
    
    # combinations of morpho_dynamic and both hydrol groups
    dynamic_e_i_type = 
      modeling_vars_update2 %>% 
      .[
        names(.) %in% c(
          "trophic_vars", "clim_vars", "temp_vars", "chem_vars", "other_vars",
          "morpho_var_dynamic", "hydrol_vars_e_i_type"
        )
      ] %>%
      flatten_chr(),
    
    dynamic_e_i =
      modeling_vars_update2 %>% 
      .[
        names(.) %in% c(
          "trophic_vars", "clim_vars", "temp_vars", "chem_vars", "other_vars",
          "morpho_var_dynamic", "hydrol_vars_e_i"
        )
      ] %>%
      flatten_chr()
  ) 
  
  
  # 3. INSPECT ALL GROUPS OF PREDICTOR VARIABLES FOR COLLINEARITY. 
  # Remove vars as necessary.
  
  
  # Calculate VIF of all predictors
  # Should used (GVIF^(1/2Df))^2 for categorical values
  # https://stats.stackexchange.com/questions/70679/
  # which-variance-inflation-factor-should-i-be-using-textgvif-or-textgvif
  # ag_eco9_nm = 1.87 (GVIF^(1/2Df))^2
  vif <- map(all_vars, ~ dat_trans %>% select(all_of(.x)) %>% corvif(.))
  
  # STEP 1 SELECTION
  # In all predictor sets, do_deep and do_shallow have VIF > 7
  # I think do_deep is more likely to be related to CH4, so 
  # exclude do_shallow and see what happens
  vif <- map(all_vars, ~ .[!(. == "do_shallow")]) %>%
    map(., ~ dat_trans %>% select(all_of(.x)) %>% corvif(.)) 
  
  # STEP 2 SELECTION
  # OK, after removing do_shallow, littoral_fraction has a VIF>5 in all groups
  # remove and see what happens
  # sa_e_i group: littoral_fraction = 6
  # sa_e_i_type group: littoral_fraction = 6
  # shoreline_e_i_type group: littoral_fraction = 5.1
  # shoreline_e_i_type group: littoral_fraction = 5.5
  # dynamic_e_i_type group: littoral_fraction = 5.2
  # dynamic_e_i_type group: littoral_fraction = 5.4
  vif <- map(all_vars, ~ .[!(. %in%  c("do_shallow", "littoral_fraction"))]) %>%
    map(., ~ dat_trans %>% select(all_of(.x)) %>% corvif(.)) 
  
  # STEP 3 SELECTION
  # After remove do_shallow and littoral_fraction, everything except the below
  # have a VIF < 4
  # shoreline_e_i_type group: mean_depth = 4.5, l_shoreline_development = 4.4
  # shoreline_e_i group: mean_depth = 4.3, l_shoreline_development = 4.4
  # dynamic_e_i_type group: l_dynamic_ratio = 4.2, wind_speed_trap = 4.1
  # remove mean_depth from shoreline groups and wind speed_trap from dynamic_e_i_type
  vif <- map(all_vars, ~ .[!(. %in%  c("do_shallow", "littoral_fraction"))]) %>%
    map_if(., 
           grepl("shoreline_e_i", names(.)), # condition 
           ~ .[!(. == "mean_depth")] # if TRUE, then omit mean_depth
    ) %>%
    map_if(., 
           grepl("dynamic_e_i_type", names(.)), # condition 
           ~ .[!(. == "wind_speed_trap")] # if TRUE, then omit wind_speed_trap
    ) %>%
    map(., ~ dat_trans %>% select(all_of(.x)) %>% corvif(.)) 
  vif
  
  # STEP 4 SELECTION
  # l_dynamic_ratio is borderline at VIF ~= 4. will retain.
  # will get rid of the other two offending variables below 
  # sa_e_i_type group: wind_speed_trap = 4.087493 
  # shoreline_e_i_type group: l_max_length = 4.05
  # dynamic_e_i_type group: l_dynamic_ratio = 4.087
  # dynamic_e_i_ group: l_dynamic_ratio = 3.95
  vif <- map(all_vars, ~ .[!(. %in%  c("do_shallow", "littoral_fraction"))]) %>%
    map_if(., 
           grepl("shoreline_e_i", names(.)), # condition 
           ~ .[!(. == "mean_depth")] # if TRUE, then omit mean_depth
    ) %>%
    map_if(., 
           grepl("dynamic_e_i_type", names(.)), # condition 
           ~ .[!(. == "wind_speed_trap")] # if TRUE, then omit wind_speed_trap
    ) %>%
    map_if(., 
           grepl("sa_e_i_type", names(.)), # condition 
           ~ .[!(. == "wind_speed_trap")] # if TRUE, then omit wind_speed_trap
    ) %>%
    map_if(., 
           grepl("shoreline_e_i_type", names(.)), # condition 
           ~ .[!(. == "l_max_length")] # if TRUE, then omit wind_speed_trap
    ) %>% 
    map(., ~ dat_trans %>% select(all_of(.x)) %>% corvif(.)) 
  vif
  
  # STEP 6, UPDATE LIST OF VARIABLES
  # Good, all variables < 4 except below. Will we proceed with this.
  # dynamic_e_i_type group: l_dynamic_ratio = 4.087
modeling_vars_final <- map(
  # exclude do_shallow and littoral_fraction from all groups
  all_vars, ~ .[!(. %in%  c("do_shallow", "littoral_fraction"))]
  ) %>%
  # exclude mean_depth from shoreline_e_i group
    map_if(., 
           grepl("shoreline_e_i", names(.)), # condition 
           ~ .[!(. == "mean_depth")] # if TRUE, then omit mean_depth
    ) %>%
  # exclude wind_speed_trap from dynamic_e_i_type group
    map_if(., 
           grepl("dynamic_e_i_type", names(.)), # condition 
           ~ .[!(. == "wind_speed_trap")] # if TRUE, then omit wind_speed_trap
    ) %>%
  # exclude wind_speed_trap from sa_e_i_type group
    map_if(., 
           grepl("sa_e_i_type", names(.)), # condition 
           ~ .[!(. == "wind_speed_trap")] # if TRUE, then omit wind_speed_trap
    ) %>%
  # exclude l_max_length from shoreline_e_i_type group
    map_if(., 
           grepl("shoreline_e_i_type", names(.)), # condition 
           ~ .[!(. == "l_max_length")] # if TRUE, then omit wind_speed_trap
    ) 
  
    return(modeling_vars_final)
  
}