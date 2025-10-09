# FUNCTION TO RESTRICT LIST OF MORPHO VARIABLES BASED ON COLINEARITY

get_final_morpho_vars <- function(dat_trans, modeling_vars_update) {
  
# 1. RELATIONSHIPS----------
# Lets see which of these variables are related to CH4. This can help
# inform which variables to retain when assessing collinearity in next step.

  dat_trans %>% 
    select(all_of(
      unname( # attributes must be stripped for all_of
        modeling_vars_update$morpho_vars
      ) # close unname
    ), # close all_of
    ch4_diffusion, ch4_ebullition, ch4_total) %>% 
    GGally::ggpairs(upper = list(continuous = wrap("cor", size = 2.5))) +
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
        modeling_vars_update$morpho_vars
      ) # close unname
    )
    ) %>%
  # morpho variables are repeated for each site in a lake
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
  
  # same story, lots of correlation
dat_trans %>%
  select(all_of(
    unname( # attributes must be stripped for all_of
      modeling_vars_update$morpho_vars
    ) # close unname
  )) %>%
  # morpho variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>%
  corvif(.)

#-#-#-#
# Given the strong correlations across many variables, we need to decide
# which to keep and which to omit. Since we have site-specific depth, retaining
# lake-scale depth estimates (mean_depth, max_depth) may not be necessary.
# Use variance inflation factor to assess collinearity across all variables
dat_trans %>%
  select(all_of(
    unname( # attributes must be stripped for all_of
      modeling_vars_update$morpho_vars
    ) # close unname
  )) %>%
  select(-mean_depth, -max_depth) %>% # remove these predictors
  # morpho variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>%
  corvif(.)

# SURFACE AREA----
# Define predictors that inlcude surface area
# sequentially remove predictors with VIF > 3
# l_surface_area, site_depth, littoral_fraction, circularity, l_buoyf

surface_area_vif <- dat_trans %>%
  # start with all predictors
  select(all_of(
    unname( # attributes must be stripped for all_of
      modeling_vars_update$morpho_vars
    ) # close unname
  )) %>%
  # sequentially remove predictors with greatest VIF, but keep surface area
   select(-l_shoreline_length, -l_volume, -l_mean_width,
          -l_dynamic_ratio, -l_max_length, -l_max_width,
          -max_depth, -l_shoreline_development) %>%
  # morpho variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>%
  corvif(.)

surface_area_vif

# SHORELINE COMPLEXITY----
# Define predictors that include shoreline complexity
# sequentially remove predictors with VIF > 3
# l_shoreline_development, l_max_length, mean_depth, littoral_fraction, l_buoyf

shoreline_vif <- dat_trans %>%
  # start with all predictors
  select(all_of(
    unname( # attributes must be stripped for all_of
      modeling_vars_update$morpho_vars
    ) # close unname
  )) %>%
  # sequentially remove predictors with greatest VIF, but keep surface area
  select(-l_shoreline_length, -l_surface_area, -l_volume,
         -l_dynamic_ratio, -l_mean_width, -max_depth,
         -l_max_width, -circularity) %>%
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
      modeling_vars_update$morpho_vars
    ) # close unname
  )) %>%
  # sequentially remove predictors with greatest VIF, but keep surface area
  select(-l_shoreline_length, -l_surface_area, -l_volume,
         -mean_depth, -l_mean_width, -l_shoreline_development,
         -l_max_width, -l_max_length) %>%
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
  select(l_surface_area, l_volume, l_shoreline_development, circularity,
         l_dynamic_ratio, littoral_fraction, site_depth) %>%
  distinct() %>%
  corvif(.)



# FINAL OPTIONS----
surface_area_vif
shoreline_vif
dynamic_ratio_vif

# create dataframe containing the three options.
# this will be used to finalize the list of modeling vars

final_morpho_vars <- imap( # imap to conveniently access list element name
  lst( # lst conveniently assigns object name to list element
    surface_area_vif, shoreline_vif, dynamic_ratio_vif
  ), 
  ~.x %>% 
    rownames_to_column(var = "variable") %>% # corvif assigns variable names to row names
    mutate(group = .y) # add name of variable group (e.g. surface_area_vif)
) %>%
  bind_rows %>% # collapse into single df
  mutate(
    group = str_replace(group, "vif", "group")
  )

return(final_morpho_vars)

}





