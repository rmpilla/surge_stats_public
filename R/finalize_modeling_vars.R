# FINALIZE LIST OF MODELING VARS

# 1. Update list of modeling vars based on collinearity within predictor groups.
# see get_final_hydrol_vars.R, get_final_morpho_vars.R, get_final_chem_vars.R,
# get_final_clim_vars.R, get_final_temp_vars.R, get_final_trophic_vars.R for
# collinearity assessments.

# 2. Create a list of predictor variables for each response (total, diffusion,
# and ebullition) and model (local_vars_all, local_vars_no_nla, global).

# 3. Inspect all groups of predictor variables for collinearity. Remove
# vars as necessary.

# Here we create modeling_vars_final which accounts for data transformations
# and collinearity

finalize_modeling_vars <- function(modeling_vars_update,
                                   
                                   # lists, local_all, local_no_nla, global
                                   final_morpho_vars, 
                                   final_hydrol_vars, 
                                   final_trophic_vars, 
                                   final_landscape_vars, 
                                   
                                   # lists, global
                                   final_clim_vars,
                                   final_temp_vars,
                                   
                                   # Using all chem vars
                                   # final_chem_vars,
                                   
                                   # need data to assess collinearity among
                                   # all predictors
                                   dat_trans
                                   ) {
 
  # 1. UPDATE LIST OF MODELING VARS BASED ON COLLINEARITY WITHIN PREDICTOR GROUPS---- 
  # start with modeling_vars_update which is a reduced list of vars after 
  # considering missingness and data transformations. See update_modeling_vars.R

  
  ## MORPHO----
  ### global group.
  modeling_vars_update2 <- modeling_vars_update %>%
    list_modify(
    ., # list to modify
              global_vars = list( # list element to modify
                # nested list element to replace
                morpho_vars  = final_morpho_vars$surface_area_vars_global %>%
                                   pull(variable)
                ) # close list
    ) %>% # close list
 
  ### local_vars_all group
  list_modify(
    ., # list to modify
    local_vars_all = list( # list element to modify
      # nested list element to replace
      morpho_vars  = final_morpho_vars$surface_area_vars_local_all %>%
        pull(variable)
    ) # close list
  ) %>% # close list
  
  ### local_vars_no_nla group
    list_modify(
      ., # list to modify
      local_vars_no_nla = list( # list element to modify
        # nested list element to replace
        morpho_vars  = final_morpho_vars$surface_area_vars_local_no_nla %>%
          pull(variable)
      ) # close list
    )  %>%
  
  ## HYDROL----
  ### global group.
    list_modify(
      ., # list to modify
      global_vars = list( # list element to modify
        # nested list element to replace
        hydrol_vars  = final_hydrol_vars$hydrol_vars_global
      ) # close list
    ) %>% # close list
    
    ### local_vars_all group
    list_modify(
      ., # list to modify
      local_vars_all = list( # list element to modify
        # nested list element to replace
        hydrol_vars  = final_hydrol_vars$hydrol_vars_local_all %>%
          pull(variable)
      ) # close list
    ) %>% # close list
    
    ### local_vars_no_nla group
    list_modify(
      ., # list to modify
      local_vars_no_nla = list( # list element to modify
        # nested list element to replace
        hydrol_vars  = final_hydrol_vars$hydrol_vars_local_no_nla
      ) # close list
    )  %>%
  
  ## CLIMATE----
  # global group
  list_modify(
    ., # list to modify
    global_vars = list( # list element to modify
      # nested list element to replace
      clim_vars  = final_clim_vars$clim_vars_global %>%
        pull(variable)
    ) # close list
  )  %>%
  
  ## CHEMISTRY----
  # using all variables. No action required
  
  ## TEMPERATURE----
  # global group
  list_modify(
    ., # list to modify
    global_vars = list( # list element to modify
      # nested list element to replace
      temp_vars  = final_temp_vars$temp_vars_global %>%
        pull(variable)
    ) # close list
  )  %>%
  
  ## TROPHIC----
  ### global group.
  list_modify(
    ., # list to modify
    global_vars = list( # list element to modify
      # nested list element to replace
      trophic_vars  = final_trophic_vars$trophic_vars_global %>%
        pull(variable)
    ) # close list
  ) %>% # close list
    
    ### local_vars_all group
    list_modify(
      ., # list to modify
      local_vars_all = list( # list element to modify
        # nested list element to replace
        trophic_vars  = final_trophic_vars$trophic_vars_local_all %>%
          pull(variable)
      ) # close list
    ) %>% # close list
    
    ### local_vars_no_nla group
    list_modify(
      ., # list to modify
      local_vars_no_nla = list( # list element to modify
        # nested list element to replace
        trophic_vars  = final_trophic_vars$trophic_vars_local_no_nla %>%
          pull(variable)
      ) # close list
    )
  
  ## OTHER VARS----
  # year_completed, ag_eco9_nm, keep both, no action required
  
  
  # 2. CREATE A LIST OF PREDICTOR BY EMISSION MECHANISM----
  
  modeling_vars_by_response <- 
    lst(
      ## list of predictors for ch4_tot response----
      ch4_total = modeling_vars_update2 %>% 
        # Remove clim_vars fro local_all and local_no_nla but retain clim_vars
        # for global model
        
        # eliminate clim_vars from local_vars_no_nla 
        list_modify(
          ., # list to modify
          local_vars_no_nla = list( # list element to modify
            # nested list element to eliminate
            clim_vars = purrr::zap() # zap removes element
          )
        ) %>% # close list_modify
        
        # eliminate clim_vars from local_vars_all
        list_modify(
          ., # list to modify
          local_vars_all = list( # list element to modify
            # nested list element to eliminate
            clim_vars = purrr::zap() # zap removes element
          )
        ), # close list_modify
      
      ## list of predictors for ch4_diffusion----
      ch4_diffusion = modeling_vars_update2,
      
      ## list of predictors for ch4_ebullition----
      ch4_ebullition = modeling_vars_update2
      
    ) # close parent list 
    
# 3. MODIFY LIST----
  # Attempted to accomplish these manipulations using all kinds of fancy
  # functions (map_depth, list_modify) but couldn't quite get there.
  # Using base R. Ugly, but readable and gets the job done.
  
  ## Modify diffusion predictors----
  ## Remove ebullition climate variables from list of diffusion predictors
  ## must do this for local_vars_all and local_vars_no_nla
  ### local_vars_all sublist----
  modeling_vars_by_response$ch4_diffusion$local_vars_all$clim_vars <- 
    modeling_vars_by_response$ch4_diffusion$local_vars_all$clim_vars[
      !(modeling_vars_by_response$ch4_diffusion$local_vars_all$clim_vars %in%
          c("precipitation_trap", "wind_speed_trap", "delta_bp_ebu"))
      ]
  
  ### local_vars_no_nla sublist----
  modeling_vars_by_response$ch4_diffusion$local_vars_no_nla$clim_vars <- 
    modeling_vars_by_response$ch4_diffusion$local_vars_no_nla$clim_vars[
      !(modeling_vars_by_response$ch4_diffusion$local_vars_no_nla$clim_vars %in%
          c("precipitation_trap", "wind_speed_trap", "delta_bp_ebu"))
    ]
  
  ## Modify ebullition predictors----
  ## Remove diffusion climate variables from list of ebullition predictors
  ## must do this for local_vars_all and local_vars_no_nla
  
  ### local_vars_all sublist----
  modeling_vars_by_response$ch4_ebullition$local_vars_all$clim_vars <- 
    modeling_vars_by_response$ch4_ebullition$local_vars_all$clim_vars[
      !(modeling_vars_by_response$ch4_ebullition$local_vars_all$clim_vars %in%
          c("precipitation_chamber", "wind_speed_chamber", "delta_bp_diff"))
    ]
  
  ### local_vars_no_nla sublist----
  modeling_vars_by_response$ch4_ebullition$local_vars_no_nla$clim_vars <- 
    modeling_vars_by_response$ch4_ebullition$local_vars_no_nla$clim_vars[
      !(modeling_vars_by_response$ch4_ebullition$local_vars_no_nla$clim_vars %in%
          c("precipitation_chamber", "wind_speed_chamber", "delta_bp_diff"))
    ]
  
  ## Flatten into simpler list
  modeling_vars_by_response <- map_depth(
    modeling_vars_by_response, # list to modify
    .depth = 2, # apply function at depth 2
    ~ .x %>% flatten_chr(.) # combines variable groups (e.g. morpho, trophic) into a flat vector
    ) 
  
  
  
  # 4. INSPECT FOR COLLINEARITY.---- 
  # Calculate VIF of all predictors
  # Should used (GVIF^(1/2Df))^2 for categorical values
  # https://stats.stackexchange.com/questions/70679/
  # which-variance-inflation-factor-should-i-be-using-textgvif-or-textgvif
  # ag_eco9_nm = 1.87 (GVIF^(1/2Df))^2
  

  ## VIF for tot, diff, and ebu----  
  vif_ch4_tot <- map(
    modeling_vars_by_response$ch4_total, 
    ~ dat_trans %>% select(all_of(unname(.x))) %>% drop_na %>% corvif(.) 
    )

  vif_ch4_diff <- map(
    modeling_vars_by_response$ch4_diffusion, 
    ~ dat_trans %>% select(all_of(unname(.x))) %>% drop_na %>% corvif(.) 
    ) 
  
  vif_ch4_ebu <- map(
      modeling_vars_by_response$ch4_ebullition, 
    ~ dat_trans %>% select(all_of(unname(.x))) %>% drop_na %>% corvif(.) 
    ) 
  
  ## ch4_tot selection----
  # STEP 1 Selection
  ## local_vars_all
  # do_deep and do_shallow have VIF > 17 in local_vars_all set
  # I think do_deep is more likely to be related to CH4, so 
  # exclude do_shallow and see what happens
  vif_ch4_tot
  
  # all vif <3 when do_shallow excluded
  dat_trans %>% select(
    all_of(
      unname(
        modeling_vars_by_response$ch4_total$local_vars_all
        )
      ), 
     -do_shallow
    ) %>%
    drop_na %>%
    corvif(.)
      
  # STEP 2 Omit vars from list
  modeling_vars_by_response$ch4_total$local_vars_all %<>%
    .[!(. %in% c("do_shallow"))]

    
  ## ch4_diff selection----
  # STEP 1 Selection
  # do_deep and do_shallow have VIF > 15 in local_vars_all set
  # I think do_deep is more likely to be related to CH4, so 
  # exclude do_shallow and see what happens
  vif_ch4_diff
  
  # all vif <3 when do_shallow excluded
  dat_trans %>% select(
    all_of(
      unname(
        modeling_vars_by_response$ch4_diffusion$local_vars_all
      )
    ), 
    -do_shallow
  ) %>%
    drop_na %>%
    corvif(.)
  
  # STEP 2 Omit vars from list
  modeling_vars_by_response$ch4_diffusion$local_vars_all %<>%
    .[!(. %in% c("do_shallow"))]
  
  
  ## ch4_ebullition selection----
  # STEP 1 Selection
  # do_deep and do_shallow have VIF > 15 in local_vars_all set
  # I think do_deep is more likely to be related to CH4, so 
  # exclude do_shallow and see what happens
  vif_ch4_ebu
  
  # all vif <3 when do_shallow excluded
  dat_trans %>% select(
    all_of(
      unname(
        modeling_vars_by_response$ch4_ebullition$local_vars_all
        )
      ), 
    -do_shallow
  ) %>%
    drop_na %>%
    corvif(.)
  
  # STEP 2 Omit vars from list
  modeling_vars_by_response$ch4_ebullition$local_vars_all %<>%
    .[!(. %in% c("do_shallow"))] 
  
  
  
  modeling_vars_final <- modeling_vars_by_response
  
  return(modeling_vars_final)
  
}
