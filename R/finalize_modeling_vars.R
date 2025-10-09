# FINALIZE LIST OF MODELING VARS

# modeling_vars_update is a reduced list of vars after considering missingness.
# See update_modeling_vars.R for details.

# Here we create modeling_vars_final which considers collinearity () 

finalize_modeling_vars <- function(modeling_vars_update,
                                   final_morpho_vars,
                                   final_hydrol_vars
                                   
                                   # Using all climate, chem and temp vars
                                   # final_clim_vars,
                                   # final_chem_vars,
                                   # final_temp_vars
                                   ) {
  
  # start with list of vars that accounts for missingness
  modeling_vars_final <- modeling_vars_update
  
  # MORPHO----
  # finalize morpho modeling vars
  modeling_vars_final$morpho_vars_surface_area <- final_morpho_vars %>%
    filter(group == "surface_area_group") %>% pull(variable)
  
  modeling_vars_final$morpho_var_shoreline <- final_morpho_vars  %>%
    filter(group == "shoreline_group") %>% pull(variable)
  
  modeling_vars_final$morpho_var_dynamic <- final_morpho_vars  %>%
    filter(group == "dynamic_ratio_group") %>% pull(variable)
  
  # delete $morph_var from modeling_vars_final. It iw replaced with above
  modeling_vars_final$morpho_vars <- NULL
  
  # HYDROL----
  # finalize hydrol modeling vars
  modeling_vars_final$hydrol_vars_e_i <- final_hydrol_vars %>%
    filter(group == "e_i_group") %>% pull(variable)
  
  modeling_vars_final$hydrol_vars_e_i_type <- final_hydrol_vars  %>%
    filter(group == "e_i_type_group") %>% pull(variable)
  
  # delete hydrol_vars, replaced with above
  modeling_vars_final$hydrol_vars <- NULL
  
  # CLIMATE----
  # using all variables. no action required
  
  # CHEMISTRY----
  # using all variables. No action required
  
  # TEMPERATURE----
  # using all variables. No action required
  
  # TROPHIC----
  # TBD
  
  # OTHER VARS----
  # year_completed, ag_eco9_nm, keep both, no action required
  
    return(modeling_vars_final)
  
}