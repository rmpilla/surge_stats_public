# FUNCTION TO RESTRICT LIST OF HYDROL VARIABLES BASED ON COLINEARITY

get_final_hydrol_vars <- function(dat_trans, modeling_vars_update) {
  
# 1. RELATIONSHIPS----------
  
  # 3 hydrology variables: e_i, e_i_type, and l_residence_time
  # e_i_type is categorical. Most likely e_i and e_i_type shouldn't
  # both be used in model.
  modeling_vars_update$hydrol_vars
  
# Lets see which of these variables are related to CH4. This can help
# inform which variables to retain when assessing collinearity in next step.
  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$hydrol_vars
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
      modeling_vars_update$hydrol_vars
    ) # close unname
  )) %>%
    select(-e_i_type) %>%
  # hydrol variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>%
  corvif(.)

  
  # e_i_type (categorical) and l_residence_time
  # VIF < 2, all good
  e_i_type_vif <- dat_trans %>%
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$hydrol_vars
      ) # close unname
    )) %>%
    select(-e_i) %>%
    # hydrol variables are repeated for each site in a lake
    # filter to unique value to simplify the plot
    distinct() %>%
    corvif(.)

  
  # create dataframe containing the two options.
  # this will be used to finalize the list of modeling vars
  
  final_hydrol_vars <- imap( # imap to conveniently access list element name
    lst( # lst conveniently assigns object name to list element
      e_i_vif, e_i_type_vif
    ), 
    ~.x %>% 
      rownames_to_column(var = "variable") %>% # corvif assigns variable names to row names
      mutate(group = .y) # add name of variable group (e.g. surface_area_vif)
  ) %>%
    bind_rows %>% # collapse into single df
    mutate(
      group = str_replace(group, "vif", "group")
    )
return(final_hydrol_vars)

}





