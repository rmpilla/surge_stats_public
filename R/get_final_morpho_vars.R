# FUNCTION TO RESTRICT LIST OF MORPHO VARIABLES BASED ON COLINEARITY

get_final_morpho_vars <- function(dat_trans, modeling_vars_update) {
  
# 1. RELATIONSHIPS----------
# Lets see which of these variables are related to CH4. This can help
# inform which variables to retain when assessing collinearity in next step.

  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$local_vars_all$morpho_vars # doesn't matter which list we use
      ) # close unname
    ), # close all_of
    ch4_diffusion, ch4_ebullition, ch4_total) %>% 
    GGally::ggpairs(upper = list(continuous = "cor")) +
    theme(
      axis.text = element_blank(),
      strip.text = element_text(size = 5) # Strip text (e.g., variable names in diagonal)
    )

# Wow, correlations between CH4 and morpho variables is super weak.
# strongest is with littoral fraction

# 2. INSPECT VARIABLES FOR COLLINEARITY-------------
# oh, oh, lots of pairwise correlations > 0.5 (cutoff suggested by Zuur, pg. 473)
  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$local_vars_all$morpho_vars # doesn't matter which list we use
      ) # close unname
    )
    ) %>%
  # morpho variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>%
  #cor(use = "pairwise.complete.obs") %>%
  GGally::ggpairs(upper = list(continuous = "cor")) +
  theme(strip.text = element_text(size = 5),
        axis.text = element_blank())
  
  
  # Assess collinearity useing variance inflation factor (VIF). VIF measuring 
  # how much a predictor's standard error is inflated due to correlations with 
  # other predictors in the model, providing a more comprehensive view than 
  # simple pairwise correlations. Cutoffs of 10 (Zuur 386) 3 (Zuur 387, 536)
  # are recommended
  
  # same story, lots of correlation
dat_trans %>%
  select(all_of(
    unname( # attributes must be stripped for all_of
      modeling_vars_update$local_vars_all$morpho_vars
    ) # close unname
  )) %>%
  # morpho variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>%
  corvif(.)


# SURFACE AREA: LOCAL MODEL----
# Define predictors that include surface area
# sequentially remove predictors with VIF > 3

surface_area_vif_local <- dat_trans %>%
  # start with all predictors
  select(all_of(
    unname( # attributes must be stripped for all_of
      modeling_vars_update$local_vars_all$morpho_vars
    ) # close unname
  )) %>%
  # sequentially remove predictors with greatest VIF, but keep surface area
    select(-shoreline_length, -mean_width, -volume,
           -max_width, -dynamic_ratio, -max_depth,
           -shoreline_development, -max_length) %>%
  # morpho variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>%
  corvif(.)

surface_area_vif_local

# SURFACE AREA: GLOBAL MODEL----
# Define predictors that include surface area
# sequentially remove predictors with VIF > 3

surface_area_vif_global <- dat_trans %>%
  # start with all predictors
  select(all_of(
    unname( # attributes must be stripped for all_of
      modeling_vars_update$global_vars$morpho_vars
    ) # close unname
  )) %>%
  # sequentially remove predictors with greatest VIF, but keep surface area
  select(-shoreline_length, -mean_width, -volume,
         -max_width, -max_depth, -dynamic_ratio,
         -shoreline_development, -max_length) %>%
  # morpho variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>%
  corvif(.)

surface_area_vif_global

# SHORELINE COMPLEXITY----
# Define predictors that include shoreline complexity
# sequentially remove predictors with VIF > 3
# l_shoreline_development, l_max_length, mean_depth, littoral_fraction, l_buoyf

shoreline_vif <- dat_trans %>%
  # start with all predictors
  select(all_of(
    unname( # attributes must be stripped for all_of
      modeling_vars_update$local_vars_all$morpho_vars
    ) # close unname
  )) %>%
  # sequentially remove predictors with greatest VIF, but keep surface area
  select(-shoreline_length, -surface_area, -mean_width,
         -dynamic_ratio, -max_width, -max_depth, -volume,
         -max_length) %>%
  # morpho variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>%
  corvif(.)

shoreline_vif

# DYNAMIC RATIO----
# Define predictors that include shoreline complexity
# sequentially remove predictors with VIF > 3
# max_depth, site_depth, littoral_fraction, circularity,
# l_dynamic_ratio, l_buoyf

dynamic_ratio_vif <- dat_trans %>%
  # start with all predictors
  select(all_of(
    unname( # attributes must be stripped for all_of
      modeling_vars_update$local_vars_all$morpho_vars
    ) # close unname
  )) %>%
  # sequentially remove predictors with greatest VIF, but keep surface area
  select(-shoreline_length, -surface_area, -mean_width,
         -max_width, -max_depth, -shoreline_development,
         -max_length, -volume
    #      -mean_depth, , ,
    #      , -l_max_length
    ) %>%
  # morpho variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>%
  corvif(.)

dynamic_ratio_vif

# LITERATURE----
# Casas-Ruiz et al. (2021) found that surface area, dynamic ratio, and 
# shoreline_complexity were related to CH4 concentration (in that order 
# of importance):

# "Littoral fraction and mean_depth were strongly correlated (rho = 0.99)
# and littoral fraction was omitted.
# Lake area and volume were also included in
# the elastic net models. Lake area was included because
# there is a size-scaling of lake morphometry, such that
# larger lakes are more fractal, less circular and more
# dish-like (figure 2). Thus, by including lake area in the
# models we can separately assess the influence of lake
# size and morphometry on the concentration of the
# different C species. Lake volume was included because
# larger lakes have a higher dilution capacity and generally
# present longer water residence times."

# Casas-Ruiz wanted to explain CH4 concentration, which
# is why the "dilution capacity" of volume was included. Not as relevant
# for our analysis. Also, their analysis included some strong collinearity:
# surface area ~ shoreline complexity: 0.74
# surface area ~ mean depth: 0.6
# shoreline complexity ~ circularity: 0.73
# shoreline complexity ~ mean depth: 0.56
# They justified this by arguing that "We chose elastic net over stepwise 
# ordinary least square regression because the penalization implemented in 
# the elastic net protects the estimates of the coefficients against 
# collinearity induced enhancement."

# here are the morpho variables used in Casas-Ruiz, plus littoral fraction
# everything is correlated
dat_trans %>%
  select(surface_area, volume, shoreline_development, circularity,
         dynamic_ratio, littoral_fraction, site_depth) %>%
  distinct() %>%
  corvif(.)



# FINAL OPTIONS----
surface_area_vif_local
surface_area_vif_global
shoreline_vif
dynamic_ratio_vif


# After reviewing the model results with all three groups, 
# we are going to proceed with the surface area group. Circularity
# showed up as an important predictor, but the partial dependence plot
# revealed decreasing emissions with increasing circularity, which is
# counterintuitive. This is likely due to the negative relationship 
# between circularity and surface area (smaller reservoirs are more 
# round than larger reservoirs). While circularity has a relatively 
# low variance inflation factor score of 1.4, its correlation with 
# surface area has a rho of 0.5. I propose that we exclude this 
# variable from the surface area morphometry group. 


final_morpho_vars <- lst(
  surface_area_vars_local_all = surface_area_vif_local,
  surface_area_vars_local_no_nla = surface_area_vif_local,
  surface_area_vars_global = surface_area_vif_global
) %>%
  map(., ~ rownames_to_column(., var = "variable") %>%
  filter(variable != "circularity")
  )

return(final_morpho_vars)

}





