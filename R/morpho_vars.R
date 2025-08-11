# 1. DEFINE MORPHO VARIABLES--------
morpho_vars <- c("surface_area",         
                 "shoreline_length",     
                 "shoreline_development",
                 "max_width", # Maximum shore to shore distance that is perpendicular to the maximum lake length            
                 "mean_width", # Lake surface area divided by fetch        
                 "max_length", # longest open water distance of a lake (fetch)        
                 "mean_depth",           
                 "max_depth",
                 "site_depth", # point specific sample depth
                 "volume",               
                 "littoral_fraction",    
                 "circularity", # circular lakes (value approaches 1) vs elongated lakes (value approaches 0).",          
                 "dynamic_ratio", # Surface area to mean depth ratio of the lake. Low values indicate lakes that are bowl shaped, whereas high values are associated to dish-like lakes.
                 
                 # NWI variables
                 ## lacustrine + subclasses
                 "lacustrine", # < 30% persistent emergent vegetation
                 "limnetic", # >2.5m deep in lacustrine
                 "littoral", # <2.5m deep in lacustrine, or max extent of nonpersistent emergents
                 
                 ## palustrine + subclasses
                 "palustrine", # wetlands, <8Ha, max depth < 2.5m
                 
                 ## riverine + subclasses
                 "riverine", #  habitats contained within a channel
                 "intermittent", # seasonally inundated riverine
                 "lower_perennial", # low gradient riverine
                 "upper_perennial", # high gradient riverine
                 
                 ## classes of lacustrine/palustrine/riverine
                 "emergent",
                 "aquatic_bed")

# any missing values? NO
dat_raw %>% 
  select(lake_id, site_id, visit, all_of(morpho_vars)) %>% 
  # filter missing values
  filter(rowSums(is.na(.)) > 0)

# 2. INSPECT FOR OUTLIERS-------------
# interactive cleveland dotplots
p_morpho <- dat_raw %>% 
  select(lake_id, all_of(morpho_vars)) %>% 
  # morpho variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>% 
  pivot_longer(cols = !lake_id, names_to = "morpho_var", values_to = "value") %>%
  mutate(f_lake_id = as.factor(lake_id)) %>% # better spacing along y-axis
  ggplot(aes(value, f_lake_id)) +
  geom_point() +
  facet_wrap(~ morpho_var, scales = "free_x") +
  # suppress y-axis tick labels
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

ggplotly(p_morpho)

# The following have outliers.
# The two Missouri River reservoirs are outliers.
# log transform the variables to reduce the influence of outliers.
# dynamic_ratio, lake_id: 69
# max_length, lake_id: 3, 70, 69
# shoreline_development, lake_id: 69
# shoreline_length, lake_id: 69, 70
# max_width, lake_id: 69
# mean_width, lake_id: 69, 70
# surface_area, lake_id: 69, 70
# volume, lake_id: 69, 70

# log10 transform variables with outliers
dat_morph <- dat_raw %>%
  mutate(across(c(dynamic_ratio, max_length, shoreline_development,
                  shoreline_length, max_width, mean_width, surface_area,
                  volume, site_depth),
                ~ log10(.x), .names = "l_{.col}"))

# Inspect the transformed variables for outliers again
p_morpho_log <- dat_morph %>% 
  select(lake_id, starts_with("l_")) %>% 
  # morpho variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>% 
  pivot_longer(cols = !lake_id, names_to = "morpho_var", values_to = "value") %>%
  mutate(f_lake_id = as.factor(lake_id)) %>% # better spacing along y-axis
  ggplot(aes(value, f_lake_id)) +
  geom_point() +
  facet_wrap(~ morpho_var, scales = "free_x") +
  # suppress y-axis tick labels
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

ggplotly(p_morpho_log)

# 69 still sticks out but not as bad. Probably ok

# update list of morp variables
l_morpho_vars <- c("l_surface_area",         
                   "l_shoreline_length",     
                   "l_shoreline_development",
                   "l_max_width", # Maximum shore to shore distance that is perpendicular to the maximum lake length            
                   "l_mean_width", # Lake surface area divided by fetch        
                   "l_max_length", # longest open water distance of a lake (fetch)     
                   "l_site_depth",
                   "mean_depth",           
                   "max_depth",            
                   "l_volume",               
                   "littoral_fraction",    
                   "circularity", # circular lakes (value approaches 1) vs elongated lakes (value approaches 0).",          
                   "l_dynamic_ratio") # Surface area to mean depth ratio of the lake. Low values indicate lakes that are bowl shaped, whereas high values are associated to dish-like lakes.

# 3. RELATIONSHIPS----------
# Lets see which of these variables are related to CH4. This can help
# inform which variables to retain when assessing collinearity in next step.

dat_morph %>% 
  select(all_of(l_morpho_vars), ch4_diffusion,
         ch4_ebullition, ch4_total) %>% 
  GGally::ggpairs() 

# Wow, correlations between CH4 and morpho variables is super weak.


# 4. INSPECT VARIABLES FOR COLLINEARITY-------------
# oh, oh, lots of correlations > 0.5 (cutoff suggested by Zuur, pg. 473)
dat_morph %>% 
  select(all_of(l_morpho_vars)) %>%
  # morpho variables are repeated for each site in a lake
  # filter to unique value to simplify the plot
  distinct() %>%
  #cor(use = "pairwise.complete.obs") %>%
  GGally::ggpairs() +
  theme(strip.text = element_text(size = 6),
        axis.text = element_blank())

#-#-#-#
# Given the strong correlations across many variables, we need to decide
# which to keep and which to omit.

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

# here are the morpho variables used in Casas-Ruiz
# everything is correlated
dat_morph %>%
  select(l_surface_area, l_volume, l_shoreline_development, circularity,
         l_dynamic_ratio) %>%
  distinct() %>%
  GGally::ggpairs()
# surface area is highly correlated with everything except site_depth.
# Correlation with circularity is -0.499. If we keep
# surface area we need to omit everything else except site_depth and
# possibly circularity.
# rho < 0.5, good
dat_morph %>%
  select(l_surface_area, l_site_depth, circularity) %>%
  GGally::ggpairs(.)
# variance inflation factor (VIF, Zuur pg.386-387, 478-479, 535-536)
source("R/aed_corvif.R") # load vif function
# vif < 2, good
dat_morph %>%
  select(l_surface_area, l_site_depth, circularity) %>%
  corvif(.)


# Dynamic ratio is correlated with everything except depth (site, mean,
# or max) and littoral_fraction.
# rho up to -0.516
dat_morph %>%
  select(l_dynamic_ratio, l_site_depth, littoral_fraction) %>%
  GGally::ggpairs()
# vif < 2, good
dat_morph %>%
  select(l_dynamic_ratio, l_site_depth, littoral_fraction) %>%
  corvif()


# Shoreline development is correlated with everything except littoral_fraction
# (-0.49), site_depth (0.251), and mean depth (0.486).
# rho up to 0.592
dat_morph %>%
  select(
    l_shoreline_development, littoral_fraction, l_site_depth,
    mean_depth
  ) %>%
  GGally::ggpairs(.)
# vif up to 2.05, good
dat_morph %>%
  select(
    l_shoreline_development, littoral_fraction, l_site_depth,
    mean_depth
  ) %>%
  corvif(.)


# fetch (l_max length) + circularity (0.517) + l_site_depth
# rho up to 0.592
dat_morph %>%
  select(l_max_length, l_site_depth, circularity) %>%
  GGally::ggpairs(.)
# vif <2, good
dat_morph %>%
  select(l_max_length, l_site_depth, circularity) %>%
  corvif(.)




# 5. MODEL-----
# Full model with interaction
m_morpho1 <- lm(ch4_total ~ littoral_fraction * l_dynamic_ratio +
                  littoral_fraction * l_site_depth +
                  l_site_depth * l_dynamic_ratio,
                data = dat_morph)
summary(m_morpho1) # interaction p > 0.318
drop1(m_morpho1, test = "F") # interaction p > 0.318


# model selection
# base::step for backwards selection based on AIC
step(m_morpho1, direction = "backward") # selects CH4tot ~ littoral_fraction + l_site_depth

# refit model
m_morpho2 <- lm(ch4_total ~ littoral_fraction + l_site_depth,
                data = dat_morph)
summary(m_morpho2) # p < 0.0133
glance(m_morpho2)
tidy(m_morpho2)

# model validation
## homogeneity: plot standardized residuals vs fitted values
{augment(m_morpho2) %>%
  ggplot(aes(.fitted, .std.resid, text = .rownames)) +
  geom_point()} %>% 
  ggplotly(tooltip = "text") # outlier: .rownumber = 2328

## homogeneity: plot standardized residuals vs explanatory variable
{augment(m_morpho2) %>%
    ggplot(aes(littoral_fraction, .std.resid, text = .rownames)) +
    geom_point()} %>% 
  ggplotly(tooltip = "text") # outlier: .rownumber = 2328

{augment(m_morpho2) %>%
    ggplot(aes(l_site_depth, .std.resid, text = .rownames)) +
    geom_point()} %>% 
  ggplotly(tooltip = "text") # outlier: .rownumber = 2328

## normality: residual histogram
### lognormal due to outliers
augment(m_morpho2) %>%
  ggplot(aes(.std.resid)) +
  geom_density() +
  geom_rug(sides = "b")






