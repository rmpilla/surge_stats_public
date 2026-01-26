# FUNCTION TO RESTRICT LIST OF HYDROL VARIABLES BASED ON COLINEARITY

get_final_hydrol_vars <- function(dat_trans, modeling_vars_update) {
  
# 1. RELATIONSHIPS----------
  
  # 2 hydrology variables: e_i and l_residence_time
  modeling_vars_update$local_vars_all$hydrol_vars # only in local_vars_all list
  
# Lets see which of these variables are related to CH4. This can help
# inform which variables to retain when assessing collinearity in next step.
  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$local_vars_all$hydrol_vars
      ) # close unname
    ), # close all_of
    ch4_diffusion, ch4_ebullition, ch4_total) %>% 
    GGally::ggpairs(upper = list(continuous = wrap("cor", size = 2.5))) +
    theme(
      axis.text = element_blank(),
      strip.text = element_text(size = 5) # Strip text (e.g., variable names in diagonal)
    )

# CH4 ebullition and residence time -0.145

# 2. INSPECT VARIABLES FOR COLLINEARITY-------------
  # Assess collinearity using variance inflation factor (VIF). VIF measuring 
  # how much a predictor's standard error is inflated due to correlations with 
  # other predictors in the model, providing a more comprehensive view than 
  # simple pairwise correlations. Cutoffs of 10 (Zuur 386) 3 (Zuur 387, 536)
  # are recommended
  
# e_i and l_residence_time
  # VIF < 2, all good
e_i_vif <- dat_trans %>%
  select(all_of(
    unname( # attributes must be stripped for all_of
      modeling_vars_update$local_vars_all$hydrol_vars
    ) # close unname
  )) %>%
  # hydrol variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>%
  corvif(.)

  # create dataframe containing the two options.
  # this will be used to finalize the list of modeling vars
  
  final_hydrol_vars <- lst(
    hydrol_vars_local_all = e_i_vif %>% 
      rownames_to_column(var = "variable"),
    hydrol_vars_local_no_nla = NULL,
    hydrol_vars_global = NULL
  )
  
return(final_hydrol_vars)

}





