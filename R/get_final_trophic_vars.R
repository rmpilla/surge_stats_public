# FUNCTION TO RESTRICT LIST OF TROPHIC VARIABLES BASED ON COLINEARITY


get_final_trophic_vars <- function(dat_trans, modeling_vars_update) {
  
  # 1. RELATIONSHIPS----------
  # Lets see which of these variables are related to CH4. This can help
  # inform which variables to retain when assessing collinearity in next step.
  
  ## local vars first
  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$local_vars_all$trophic_vars
      ) # close unname
    ), # close all_of
    ch4_diffusion, ch4_ebullition, ch4_total) %>% 
    GGally::ggpairs(upper = list(continuous = "cor")) +
    theme(
      axis.text = element_blank(),
      strip.text = element_text(size = 5) # Strip text (e.g., variable names in diagonal)
    )
  
  # nothing jumps out
  
  ## global vars
  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$global_vars$trophic_vars
      ) # close unname
    ), # close all_of
    ch4_diffusion, ch4_ebullition, ch4_total) %>% 
    GGally::ggpairs(upper = list(continuous = "cor")) +
    theme(
      axis.text = element_blank(),
      strip.text = element_text(size = 5) # Strip text (e.g., variable names in diagonal)
    )
  
  # Noteable correlations with LAGOS remotely sensed data
  
  # 2. INSPECT VARIABLES FOR COLLINEARITY-------------
  ## local vars
  # yikes, looks like lots of correlation
  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$local_vars_all$trophic_vars
      ) # close unname
    )
    ) %>%
    # chem variables are repeated for each site in a lake
    # filter to unique value to simplify the plot
    distinct() %>%
    #cor(use = "pairwise.complete.obs") %>%
    GGally::ggpairs(upper = list(continuous = "cor")) +
    theme(strip.text = element_text(size = 5),
          axis.text = element_blank())
  
  ## global vars
  # yikes, looks like lots of correlation
  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$global_vars$trophic_vars
      ) # close unname
    )
    ) %>%
    # chem variables are repeated for each site in a lake
    # filter to unique value to simplify the plot
    distinct() %>%
    #cor(use = "pairwise.complete.obs") %>%
    GGally::ggpairs(upper = list(continuous = "cor")) +
    theme(strip.text = element_text(size = 5),
          axis.text = element_blank())
  
  # Assess collinearity using variance inflation factor (VIF). VIF measuring 
  # how much a predictor's standard error is inflated due to correlations with 
  # other predictors in the model, providing a more comprehensive view than 
  # simple pairwise correlations. Cutoffs of 10 (Zuur 386) 3 (Zuur 387, 536)
  # are recommended
  
  # all VIF ....
  ## local all
  trophic_vars_local_all <- dat_trans %>%
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$local_vars_all$trophic_vars
      ) # close unname
    )) %>%
    # sequentially remove predictors with greatest VIF,
    select(
           # TN is highest at 8.6 (>3.0 threshold), but TN is a better
           # trophic status predictor than no2_3 which has a VIF of 3.9.
           # Choosing to omit no2_3 and retain TN
           -no2_3_shallow
    ) %>%
    # some variables are repeated for each site in a lake
    # filter to unique value to simplify the plot
    distinct() %>%
    corvif(.) %>%
    rownames_to_column(var = "variable") # corvif assigns variable names to row names
 
  ## local no nla
  trophic_vars_local_no_nla <- dat_trans %>%
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$local_vars_no_nla$trophic_vars
      ) # close unname
    )) %>%
    # sequentially remove predictors with greatest VIF,
    select(
           # TN is highest at 4.7 (>3.0 threshold), but TN is a better
           # trophic status predictor than no2_3 which has a VIF of 2.9.
           # Choosing to omit no2_3 and retain TN
           -no2_3_shallow
    ) %>%
    # some variables are repeated for each site in a lake
    # filter to unique value to simplify the plot
    distinct() %>%
    corvif(.) %>%
    rownames_to_column(var = "variable") # corvif assigns variable names to row names
  
  ## global
  trophic_vars_global <- dat_trans %>%
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$global_vars$trophic_vars
      ) # close unname
    )) %>%
    # sequentially remove predictors with greatest VIF,
    select(-contains("visit2"), # these are mostly NA and break corvif
           -contains("season") # remove season estimates, retain month estimates
    ) %>%
    # some variables are repeated for each site in a lake
    # filter to unique value to simplify the plot
    distinct() %>%
    corvif(.) %>%
    rownames_to_column(var = "variable") # corvif assigns variable names to row names
  
  final_trophic_vars <- lst(
    trophic_vars_local_all = trophic_vars_local_all,
    trophic_vars_local_no_nla = trophic_vars_local_no_nla,
    trophic_vars_global)
  
return(final_trophic_vars)
}