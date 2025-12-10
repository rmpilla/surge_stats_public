# FUNCTION TO RESTRICT LIST OF CHEM VARIABLES BASED ON COLINEARITY
# this function does nothing because no strong correlations were detected.

get_final_chem_vars <- function(dat_trans, modeling_vars_update) {
  
# 1. RELATIONSHIPS----------
# Lets see which of these variables are related to CH4. This can help
# inform which variables to retain when assessing collinearity in next step.

  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$chem_vars
      ) # close unname
    ), # close all_of
    ch4_diffusion, ch4_ebullition, ch4_total) %>% 
    GGally::ggpairs(upper = list(continuous = wrap("cor", size = 2.5))) +
    theme(
      axis.text = element_blank(),
      strip.text = element_text(size = 5) # Strip text (e.g., variable names in diagonal)
    )

# No noteable correlations

# 2. INSPECT VARIABLES FOR COLLINEARITY-------------
# very little correlations. do_shallow and do_deep are correlated
# at 0.494, close to 0.5 cutoff suggested by Zuur (pg. 473)
  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$chem_vars
      ) # close unname
    )
    ) %>%
  # chem variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>%
  #cor(use = "pairwise.complete.obs") %>%
  GGally::ggpairs(upper = list(continuous = wrap("cor", size = 2.5))) +
  theme(strip.text = element_text(size = 5),
        axis.text = element_blank())
  
  
  # Assess collinearity useing variance inflation factor (VIF). VIF measuring 
  # how much a predictor's standard error is inflated due to correlations with 
  # other predictors in the model, providing a more comprehensive view than 
  # simple pairwise correlations. Cutoffs of 10 (Zuur 386) 3 (Zuur 387, 536)
  # are recommended
  
  # all VIF < 2, no problem, keep all
dat_trans %>%
  select(all_of(
    unname( # attributes must be stripped for all_of
      modeling_vars_update$chem_vars
    ) # close unname
  )) %>%
  # morpho variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>%
  corvif(.)


}





