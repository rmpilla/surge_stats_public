# FUNCTION TO RESTRICT LIST OF CLIMATE VARIABLES BASED ON COLINEARITY
# this function does nothing because no strong correlations were detected.

get_final_clim_vars <- function(dat_trans, modeling_vars_update) {
  
# 1. RELATIONSHIPS----------
# Lets see which of these variables are related to CH4. This can help
# inform which variables to retain when assessing collinearity in next step.
## local
  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$local_vars_all$clim_vars # local_vars_all and local_vars_all_no_nla are the same
      ) # close unname
    ), # close all_of
    ch4_diffusion, ch4_ebullition, ch4_total) %>% 
    GGally::ggpairs(upper = list(continuous = "cor")) +
    theme(
      axis.text = element_blank(),
      strip.text = element_text(size = 5) # Strip text (e.g., variable names in diagonal)
    )

# 0.088, CH4 ebu and delta_bp_ebu
  
## global
  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$global_vars$clim_vars # 
      ) # close unname
    ), # close all_of
    ch4_diffusion, ch4_ebullition, ch4_total) %>% 
    GGally::ggpairs(upper = list(continuous = "cor")) +
    theme(
      axis.text = element_blank(),
      strip.text = element_text(size = 5) # Strip text (e.g., variable names in diagonal)
    )
# no much 
    
# 2. INSPECT VARIABLES FOR COLLINEARITY----
  # local. These will be further broken up by mechanism
  
  ## local diffusion----
  # VIF < 2, all good
  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$local_vars_all$clim_vars # local_vars_all and local_vars_all_no_nla are the same
      ) # close unname
    ), # close all_of
    -precipitation_trap, -wind_speed_trap, -delta_bp_ebu # exclude ebullition vars
    ) %>%
    corvif(.)
    
  ## local ebullition----
  # VIF < 2, all good
  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$local_vars_all$clim_vars # local_vars_all and local_vars_all_no_nla are the same
      ) # close unname
    ), # close all_of
    -precipitation_chamber, -wind_speed_chamber, -delta_bp_diff # exclude diffusion vars
    ) %>%
    corvif(.)    

  
  ## global----
  # strong correlation among catchment and watershed variables.
  # go with catchment
  clim_vars_global <- dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$global_vars$clim_vars #
      ) # close unname
    ) # close all_of
    ) %>% # close select
    # exclude variables to achieve VIF < 3
    select(
      -contains("ws") # exclude watershed / select catchment variables
    ) %>%
    corvif(.) %>%
    rownames_to_column(var = "variable") # corvif assigns variable names to row names   

return(final_clim_vars = lst(clim_vars_global))
  
  
  
}