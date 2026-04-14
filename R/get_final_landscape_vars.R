# FUNCTION TO RESTRICT LIST OF LANDSCAPE VARIABLES BASED ON COLINEARITY


get_final_landscape_vars <- function(dat_trans, modeling_vars_update) {
  
  # 1. RELATIONSHIPS----------
  # Lets see which of these variables are related to CH4. This can help
  # inform which variables to retain when assessing collinearity in next step.
  
  ## local vars first
  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$local_vars_all$landscape_vars
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
        modeling_vars_update$global_vars$landscape_vars
      ) # close unname
    ), # close all_of
    ch4_diffusion, ch4_ebullition, ch4_total) %>% 
    GGally::ggpairs(upper = list(continuous = "cor")) +
    theme(
      axis.text = element_blank(),
      strip.text = element_text(size = 5) # Strip text (e.g., variable names in diagonal)
    )
  
  # nothing jumps out
  
  # 2. INSPECT VARIABLES FOR COLLINEARITY-------------
  ## local vars
  # 0.74 rho
  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$local_vars_all$landscape_vars
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
  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$global_vars$landscape_vars
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
  ## local
  landscape_vars_local <- dat_trans %>%
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$local_vars_all$landscape_vars
      ) # close unname
    )) %>%
    # sequentially remove predictors with greatest VIF,
    # select(
    # ) %>%
    # # some variables are repeated for each site in a lake
    # filter to unique value to simplify the plot
    distinct() %>%
    corvif(.) %>%
    rownames_to_column(var = "variable") # corvif assigns variable names to row names
  
  ## global
  landscape_vars_global <- dat_trans %>%
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$global_vars$landscape_vars
      ) # close unname
    )) %>%
    # sequentially remove predictors with greatest VIF,
    select(-agkffactcat, # 4.6
           -nitrogencat # 3.3
    ) %>%
    # some variables are repeated for each site in a lake
    # filter to unique value to simplify the plot
    distinct() %>%
    corvif(.) %>%
    rownames_to_column(var = "variable") # corvif assigns variable names to row names
 
  
# FINAL VARS---- 
  final_landscape_vars <- lst(
    landscape_vars_local_all = landscape_vars_local,
    landscape_vars_local_no_nla = landscape_vars_local,
    landscape_vars_global
    )
  
return(final_landscape_vars)
}