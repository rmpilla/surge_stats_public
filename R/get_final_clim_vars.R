# FUNCTION TO RESTRICT LIST OF CLIMATE VARIABLES BASED ON COLINEARITY
# this function does nothing because no strong correlations were detected.

get_final_clim_vars <- function(dat_trans, modeling_vars_update) {
  
# 1. RELATIONSHIPS----------
# Lets see which of these variables are related to CH4. This can help
# inform which variables to retain when assessing collinearity in next step.

  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$clim_vars
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
# yes, some correlation
  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$clim_vars
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

  

  # VIF < 2, all good
  dat_trans %>%
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$clim_vars
      ) # close unname
    )) %>%
    # filter to unique values
    distinct() %>%
    corvif(.)

# no action required, use all vars
}