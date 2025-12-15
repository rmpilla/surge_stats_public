run_gamm <- function(modeling_vars_final,
                     dat_trans) {

# CREATE SOME GAM MODELS!

# interrogate modeling_vars_final
modeling_vars_final # a list of all possible combinations of predictor groups.

# 1. CREATE LIST OF ALL POSSIBLE COMBINATIONS OF RESPONSE AND----
#    PREDICTOR VARIABLES  
formula_list <- modeling_vars_final %>%
  # add smoother term s() and fit details. 
  # fx = FALSE, k = -1 instructs model to use cross validation to determine amount of smoothing
  # bs=cr means use cubic regression spline
  map(., ~ paste0("s(", ., ", fx = FALSE, k = -1, bs = \"cr\")", collapse = " + ")) %>%
  
  # CAN'T MODIFY CATEGORICAL VARIABLES WITH SMOOTHERS. REMOVE SMOOTHER
  # Note that the regex expressions require \ and \\ to escape the special 
  # meaning of ( and ". These are always confusing.
  # e_i_type
  map(., ~ str_replace_all(
    ., # current list element
    "s\\(e_i_type, fx = FALSE, k = -1, bs = \"cr\"\\)", # replace this. 
    "e_i_type" # replacement.
  )) %>%
  # 
  map(., ~ str_replace_all(
    ., # current list element
    "s\\(ag_eco9_nm, fx = FALSE, k = -1, bs = \"cr\"\\)", # replace this.
    "ag_eco9_nm" # replacement. 
  )) %>%
  # Now add response variables
  map(., ~ paste0(c("ch4_total", "ch4_diffusion", "ch4_ebullition"), " ~ ", .)) %>%
  # remove nesting structure, retainS names, but appends 1,2, or 3 for the 
  # nested elements. See below for fix
  unlist(., use.names = TRUE) 


# List element names are appended with 1,2, or 3 (see unlist above)
# replace these with the name of the response variable. This code
# chunk creates a new list containing the desired names for the formula_list.
formula_list_names <- imap_chr(formula_list, 
                               # .x is list element
                               # .y is element name
                               function(.x, .y) { 
                                 # append "total" when total_ch4 is response variable   
                                 if(grepl("total", .x)) {
                                   .y %>% gsub("[123]", "", .) %>% # remove numeric
                                     paste0(., "_total")
                                 } else if (grepl("diffusion", .x)) {
                                   .y %>% gsub("[123]", "", .) %>% # remove numeric
                                     paste0(., "_diffusion")
                                 } else if (grepl("ebullition", .x)) {
                                   .y %>% gsub("[123]", "", .) %>% # remove numeric
                                     paste0(., "_ebullition")
                                 } else {
                                   .y
                                 }
                               }
)

# Update formula_list names with those in formula_list_names.                            
formula_list <- setNames(formula_list, formula_list_names) %>%
  map(., ~as.formula(.))

# 2. DEFINE WEIGHTS----
# Add weights argument. Lets start with sa_e_i_total model
# consider multiple specifications
vf1 <- varIdent(form = ~1|lake_id)
vf2 <- varIdent(form = ~1|ag_eco9_nm)
vf3 <- varIdent(form = ~1|lake_id * ag_eco9_nm)

# 3. PREPARE DATA AND ADDRESS CONVERGENCE ISSUES----
# arguments to improve convergence 
lmc = lmeControl(niterEM = 5000, msMaxIter = 1000)

# as of 12/4/2025, daT_trans contains duplicate sites in lakes
# 147, 148, 250, 281. Need to resolve, but filter them out for now.
# See issue #16
dat_trans <- dat_trans %>% filter(!(lake_id %in% c(147:148, 250, 281)))

# 4. RUN GAM----
tic() # 63 minutes
# yikes, some of these fail to converge, and take a long time.
# maybe focus on a single weights and random specification, for now.
gam_sa_e_i_total <- lst(
  sa_e_i_total_gam = try(
    # no random component
    mgcv::gamm(formula_list[[1]], 
               data = dat_trans,
               control = lmc,
               method = "REML")
  ),
  sa_e_i_total = try(
    mgcv::gamm(formula_list[[1]], 
               data = dat_trans, 
               random = list(lake_id =~ 1),
               control = lmc,
               method = "REML")
    ),
  sa_e_i_total_vf1 = try(
    mgcv::gamm(formula_list[[1]], 
                                data = dat_trans,
                                weights = vf1, 
                                random = list(lake_id =~ 1),
                                control = lmc,
                                method = "REML")
    ),
  sa_e_i_total_vf2 = try(
    mgcv::gamm(formula_list[[1]], 
                                data = dat_trans,
                                weights = vf2, 
                                random = list(lake_id =~ 1),
                                control = lmc,
                                method = "REML")
    ),
  sa_e_i_total_vf3 = try(
    mgcv::gamm(formula_list[[1]], 
                                data = dat_trans,
                                weights = vf3, 
                                random = list(lake_id =~ 1),
                                control = lmc,
                                method = "REML")
    ),
  sa_e_i_total_vf2_sp = try(
    mgcv::gamm(formula_list[[1]], 
               data = dat_trans,
               weights = vf2, 
               random = list(lake_id =~ 1),
               correlation = corGaus(form = ~long + lat|visit), # corSpher failed
               control = lmc,
               method = "REML")
  ),
  )
toc()

# only the gam, gamm with no weights, and vf2 converged, extract models from list
sa_e_i_total_gam <- gam_sa_e_i_total$gam_sa_e_i_total_gam # no random, no weights
sa_e_i_total <- gam_sa_e_i_total$sa_e_i_total # random, no weights
sa_e_i_total_vf2 <- gam_sa_e_i_total$sa_e_i_total_vf2 # random, VF2
sa_e_i_total_vf2_sp <- gam_sa_e_i_total$sa_e_i_total_vf2_sp


# diagnostic plots (Zuur page 59 for guidance)
# plot 1: QQ plot
# plot 2: resid vs linear predictor (used to assess homogeneity)
# plot 3: histograms:: distribution of residuals
# plot 4: observed vs fitted
# all have problems, vf2 looks best
gam.check(sa_e_i_total_gam$gam) # no weights, no random
gam.check(sa_e_i_total$gam) # random, no weights
gam.check(sa_e_i_total_vf2$gam) # random, weights ~ ecoregion
gam.check(sa_e_i_total_vf2_sp$gam) # random, weights ~ ecoregion, spatial
  
  
# anova to see which model is prefferred
# model with random + vf2 + spatial is preferred
anova(sa_e_i_total_gam$lme, sa_e_i_total$lme, 
      sa_e_i_total_vf2$lme, sa_e_i_total_vf2_sp$lme)



# extract normalized residuals from best model
# the gamm is a list, residuals must be extracted from the lme component
E1 <- resid(sa_e_i_total_vf2_sp$lme, type = "normalized")
length(E1) #1219
nrow(dat_trans) #2546
# add residuals to dataframe
dat_trans_resid_std <-  dat_trans %>%
  select(modeling_vars_final$sa_e_i, lake_id, ch4_total) %>% 
  filter(complete.cases(.)) %>%
  mutate(resid_std = E1) 

# residuals vs ecoregion: not bad
ggplot(dat_trans_resid_std, aes(ag_eco9_nm, resid_std)) +
  geom_boxplot()

# residuals vs lake_id: issues remain
ggplot(dat_trans_resid_std, aes(lake_id, resid_std)) +
  geom_boxplot()

# model selection
# start with model that incorporates random effect, weights ~ ecoregion, spatial
anova(sa_e_i_total_vf2_sp$gam)
summary(sa_e_i_total_vf2_sp$gam) # can't be used for ecoregion (nominal with > 2 levels, but good for all other)

# update function doesn't work for gamm, formula must be updated manually
# step 1. s(turbidity_deep) p = 0.968
sa_e_i_total_vf2_sp_drop1 <- mgcv::gamm(update(formula_list[[1]], ~ . -s(turbidity_deep, fx = FALSE, k = -1, bs = "cr")), 
                                        data = dat_trans,
                                        weights = vf2, 
                                        random = list(lake_id =~ 1),
                                        correlation = corGaus(form = ~long + lat|visit), # corSpher failed
                                        control = lmc,
                                        method = "REML")
summary(sa_e_i_total_vf2_sp_drop1$gam)

# step 2. s(circularity) p = 0.99
# this failed to converge
sa_e_i_total_vf2_sp_drop2 <- mgcv::gamm(
  update(
    formula_list[[1]], ~ . - s(turbidity_deep, fx = FALSE, k = -1, bs = "cr") - 
      s(circularity, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSphere failed
  control = lmc,
  method = "REML"
)
summary(sa_e_i_total_vf2_sp_drop2$gam)

# step 3. removing s(circularity) didn't converge, try removing
# s(circularity) (p = 0.99) and the next least significant term 
# s(chl_predicted_sample_month_visit1) (p=0.83)
# this failed to converge
sa_e_i_total_vf2_sp_drop2 <- mgcv::gamm(
  update(
    formula_list[[1]], ~ . - s(turbidity_deep, fx = FALSE, k = -1, bs = "cr") - 
      s(circularity, fx = FALSE, k = -1, bs = "cr") -
      s(chl_predicted_sample_month_visit1, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(sa_e_i_total_vf2_sp_drop2$gam)


# step 4. removing s(circularity) and s(chl_predicted_sample_month_visit1) 
# failed to converge. Try again with a greater number of iterations
# this also failed.
sa_e_i_total_vf2_sp_drop2 <- mgcv::gamm(
  update(
    formula_list[[1]], ~ . - s(turbidity_deep, fx = FALSE, k = -1, bs = "cr") - 
      s(circularity, fx = FALSE, k = -1, bs = "cr") -
      s(chl_predicted_sample_month_visit1, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML"
)
summary(sa_e_i_total_vf2_sp_drop2$gam)

# STEP 5: perhaps we have too many smoothers to allow for convergence.----
# try eliminating smoother for predictors with edf <= 1.001 and p > 0.1
modeling_vars_final_few_s <- sub(
  "l_chla_sonde_shallow", 
  "s(l_chla_sonde_shallow, fx = FALSE, k = -1, bs = \"cr\")",
  modeling_vars_final$sa_e_i
) %>%
  sub(
    "l_precipitation_chamber", 
    "s(l_precipitation_chamber, fx = FALSE, k = -1, bs = \"cr\")",
    .
  ) %>%
  sub(
    "l_wind_speed_chamber", 
    "s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = \"cr\")",
    .
  ) %>%
  sub(
    "temp_deep", 
    "s(temp_deep, fx = FALSE, k = -1, bs = \"cr\")",
    .
  ) %>% 
  sub(
    "l_toc_shallow", 
    "s(l_toc_shallow, fx = FALSE, k = -1, bs = \"cr\")",
    .
  ) %>% 
  sub(
    "mean_depth", 
    "s(mean_depth, fx = FALSE, k = -1, bs = \"cr\")",
    .
  ) %>% 
  paste0(., collapse = " + ") %>%
  paste0("ch4_total ~ ", .) %>%
  as.formula(.)

# this converged quickly  
sa_e_i_total_vf2_sp_2 <- mgcv::gamm(
  modeling_vars_final_few_s,
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(sa_e_i_total_vf2_sp_2$gam)
anova(sa_e_i_total_vf2_sp_2$gam)

# 5.1----
# variable selection is tricky because of the nominal variable "ag_eco9_nm"
# Neither summary nor ANOVA give good p-values. Use liklihood ratio test (ML)
# to assess significance of ag_eco9_nm, then remove least significant term.
m1_full <- mgcv::gamm(
  modeling_vars_final_few_s,
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "ML" # for log liklihood test
)
summary(m1_full$lme) # 1219 observations

m1_a <- mgcv::gamm(
  update(modeling_vars_final_few_s, ~ . - ag_eco9_nm),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "ML"
)
summary(m1_a$lme) # 1219 observations
anova(m1_full$lme, m1_a$lme) # ag_eco9 is 0.056, hold onto it

# 5.2----
# based on summary, and the test for ag_eco9, wind_speed_trap is least significant
# remove and refit
m2_full_reml <- mgcv::gamm(
  update(modeling_vars_final_few_s, ~ . - wind_speed_trap),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m2_full_reml$gam) # turbidity_deep p = 0.967

# refit with reml in prep for log liklihood test to asses ag_eco9_nm
m2_full_ml <- mgcv::gamm(
  update(modeling_vars_final_few_s, ~ . - wind_speed_trap),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "ML"
)

m2_a_ml <- mgcv::gamm(
  update(modeling_vars_final_few_s, ~ . - wind_speed_trap - ag_eco9_nm),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "ML"
)
anova(m2_full_ml$lme, m2_a_ml$lme) # ag_eco9 is 0.06, hold onto it

# 5.3----
# based on summary (on the REML model) and the test for ag_eco9_nm, turbidity_deep
# is least significant. Remove and refit.
m3_full_reml <- mgcv::gamm(
  update(modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m3_full_reml$gam) # n=1282, chl_predicted_sample_month_visit1 p = 0.835

# since many variables have p-values far larger than ag_eco9_nm, will stop
# testing ag_eco9_nm. 

# 5.4----
# based on summary (on the REML model) chl_predicted_sample_month_visit1 
# is least significant. Remove and refit.
m4_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 
    ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m4_full_reml$gam) #  circularity  p = 0.77, n = 1282

# 5.5----
# based on summary (on the REML model) circularity 
# is least significant. Remove and refit.
m5_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m5_full_reml$gam) #  year_completed  p = 0.72, n = 1282

# 5.6----
# based on summary (on the REML model) year_completed
# is least significant. Remove and refit.
m6_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m6_full_reml$gam) #  l_precipitation_trap  p = 0.67, n = 1282


# 5.7----
# based on summary (on the REML model) l_precipitation_trap
# is least significant. Remove and refit.
m7_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m7_full_reml$gam) #  l_so4_shallow p = 0.58, n = 1282

# 5.8----
# based on summary (on the REML model) l_so4_shallow p = 0.58
# is least significant. Remove and refit.
m8_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m8_full_reml$gam) # s(l_precipitation_chamber) p = 0.35, n = 1282


# 5.9----
# based on summary (on the REML model) s(l_precipitation_chamber)
# is least significant. Remove and refit.
m9_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m9_full_reml$gam) #  l_buoyf p = 0.31, n = 1282


# 5.10----
# based on summary (on the REML model) l_buoyf
# is least significant. Remove and refit.
m10_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m10_full_reml$gam) # s(temp_deep) p = 0.24, n = 1282

# 5.11----
# based on summary (on the REML model) s(temp_deep)
# is least significant. Remove and refit.
m11_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m11_full_reml$gam) # do_deep p = 0.37, n = 1282

# 5.12----
# based on summary (on the REML model) do_deep
# is least significant. Remove and refit.
m12_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m12_full_reml$gam) # s(l_wind_speed_chamber) p = 0.35, n = 1296


# 5.13----
# based on summary (on the REML model) s(l_wind_speed_chamber)
# is least significant. Remove and refit.
m13_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep -
      s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m13_full_reml$gam) # l_surface_area p = 0.27, n = 1296


# 5.14----
# based on summary (on the REML model) l_surface_area
# is least significant. Remove and refit.
m14_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep -
      s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_surface_area
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m14_full_reml$gam) # doc_predicted_sample_month_visit1 p = 0.25, n = 1296


# 5.15----
# based on summary (on the REML model) doc_predicted_sample_month_visit1
# is least significant. Remove and refit.
m15_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep -
      s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_surface_area - doc_predicted_sample_month_visit1
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m15_full_reml$gam) # s(l_toc_shallow) p = 0.9113, n = 1392!



# 5.16----
# based on summary (on the REML model) s(l_toc_shallow)
# is least significant. Remove and refit.
m16_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep -
      s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_surface_area - doc_predicted_sample_month_visit1 -
      s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m16_full_reml$gam) # l_residence_time p = 0.76, n = 1392



# 5.17----
# based on summary (on the REML model)  l_residence_time
# is least significant. Remove and refit.
m17_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep -
      s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_surface_area - doc_predicted_sample_month_visit1 -
      s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
      l_residence_time
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m17_full_reml$gam) # l_op_shallow P = 0.41, n = 1392

# 5.18----
# based on summary (on the REML model)  l_op_shallow 
# is least significant. Remove and refit.
m18_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep -
      s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_surface_area - doc_predicted_sample_month_visit1 -
      s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
      l_residence_time - l_op_shallow 
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m18_full_reml$gam) # l_tp_shallow P = 0.24, n = 1392


# 5.19----
# based on summary (on the REML model)  l_op_shallow 
# is least significant. Remove and refit.
m19_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep -
      s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_surface_area - doc_predicted_sample_month_visit1 -
      s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
      l_residence_time - l_op_shallow - l_tp_shallow
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m19_full_reml$gam) # e_i p = 0.17, n = 1392

# 5.20----
# based on summary (on the REML model)  e_i 
# is least significant. Remove and refit.
m20_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep -
      s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_surface_area - doc_predicted_sample_month_visit1 -
      s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
      l_residence_time - l_op_shallow - l_tp_shallow - e_i 
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m20_full_reml$gam) # l_nh4_shallow p = 0.17, n = 1547

# 5.21----
# based on summary (on the REML model)  e_i 
# is least significant. Remove and refit.
m21_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep -
      s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_surface_area - doc_predicted_sample_month_visit1 -
      s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
      l_residence_time - l_op_shallow - l_tp_shallow - e_i -
      l_nh4_shallow 
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m21_full_reml$gam) # tn_shallow, p = 0.19, n = 1547


# 5.22----
# based on summary (on the REML model)  tn_shallow 
# is least significant. Remove and refit.
m22_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep -
      s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_surface_area - doc_predicted_sample_month_visit1 -
      s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
      l_residence_time - l_op_shallow - l_tp_shallow - e_i -
      l_nh4_shallow - tn_shallow
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m22_full_reml$gam) # ok, p values are getting small now. retest ecoregion

# Neither summary nor ANOVA give good p-values. Use liklihood ratio test (ML)
# to assess significance of ag_eco9_nm, then remove least significant term.
m22_full_ml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep -
      s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_surface_area - doc_predicted_sample_month_visit1 -
      s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
      l_residence_time - l_op_shallow - l_tp_shallow - e_i -
      l_nh4_shallow - tn_shallow
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "ML" # for log liklihood test
)
summary(m22_full_ml$gam) # 1547

m22_full_ml_a <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep -
      s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_surface_area - doc_predicted_sample_month_visit1 -
      s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
      l_residence_time - l_op_shallow - l_tp_shallow - e_i -
      l_nh4_shallow - tn_shallow - ag_eco9_nm),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "ML"
)
summary(m22_full_ml_a$gam) # 1547 observations
anova(m22_full_ml$lme, m22_full_ml_a$lme) # ag_eco9 is 0.22 

# remove ecoregion, p = 0.22


# 5.23----
# based on summary (on the REML model) and anova on the ML (test for ag_eco)
# ag_eco9_nm  is least significant. Remove and refit.
m23_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep -
      s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_surface_area - doc_predicted_sample_month_visit1 -
      s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
      l_residence_time - l_op_shallow - l_tp_shallow - e_i -
      l_nh4_shallow - tn_shallow - ag_eco9_nm
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m23_full_reml$gam) # temp_shallow, p = 0.597, n = 1547


# 5.24----
# based on summary (on the REML model) and anova on the ML (test for ag_eco)
# ag_eco9_nm  is least significant. Remove and refit.
m24_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep -
      s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_surface_area - doc_predicted_sample_month_visit1 -
      s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
      l_residence_time - l_op_shallow - l_tp_shallow - e_i -
      l_nh4_shallow - tn_shallow - ag_eco9_nm
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m24_full_reml$gam) # temp_shallow, p = 0.597, n = 1547


# 5.25----
# based on summary (on the REML model)  temp_shallow 
# is least significant. Remove and refit.
m25_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep -
      s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_surface_area - doc_predicted_sample_month_visit1 -
      s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
      l_residence_time - l_op_shallow - l_tp_shallow - e_i -
      l_nh4_shallow - tn_shallow - ag_eco9_nm - temp_shallow
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m25_full_reml$gam) # s(mean_depth), p = 0.334 n = 1547



# 5.26----
# based on summary (on the REML model)  temp_shallow 
# is least significant. Remove and refit.
m26_full_reml <- mgcv::gamm(
  update(
    modeling_vars_final_few_s, ~ . - wind_speed_trap - turbidity_deep - 
      chl_predicted_sample_month_visit1 - circularity - year_completed -
      l_precipitation_trap - l_so4_shallow - 
      s(l_precipitation_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_buoyf -
      s(temp_deep, fx = FALSE, k = -1, bs = "cr") -
      do_deep -
      s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
      l_surface_area - doc_predicted_sample_month_visit1 -
      s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
      l_residence_time - l_op_shallow - l_tp_shallow - e_i -
      l_nh4_shallow - tn_shallow - ag_eco9_nm - temp_shallow -
      s(mean_depth, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m26_full_reml$gam) # everything significant n = 1547
anova(m26_full_reml$gam)


# 5.27----
# refit model with smoothers for all predictors, see if EDF suggests linear or not
m27_full_reml <- mgcv::gamm(
  ch4_total ~ s(chla_lab_shallow, fx = FALSE, k = -1, bs = "cr") +
    s(l_chla_sonde_shallow, fx = FALSE, k = -1, bs = "cr") +
    s(site_depth, fx = FALSE, k = -1, bs = "cr"),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m27_full_reml$gam) # everything significant n = 1547
anova(m27_full_reml$gam) # depth and sonde are non-linear


# 5.28----
# partial dependence plots for smoothers
png("reports/ch4_tot_partial_dependence_plots_1.png")
plot(m27_full_reml$gam,  
     select = 1, # chla lab
     ylab = "ch4_total",
     scheme = 2)
dev.off()

png("reports/ch4_tot_partial_dependence_plots_2.png")
plot(m27_full_reml$gam,  
     select = 2, # chla sonde
     ylab = "ch4_total",
     scheme = 2)
dev.off()

png("reports/ch4_tot_partial_dependence_plots_3.png")
plot(m27_full_reml$gam,  
     select = 3, # chla lab
     ylab = "ch4_total",
     scheme = 2)
dev.off()


# # alternative method for partial dependence plots
# library(gratia)
# # Get data for plotting the linear effect of chla_lab_shallow
# df_plot_chla_lab <- data_slice(m26_full_reml$gam, 
#                       chla_lab_shallow = evenly(chla_lab_shallow, n = 100))
# df_plot_depth <- data_slice(m26_full_reml$gam, 
#                       site_depth = evenly(site_depth, n = 100))
# df_plot_sonde <- data_slice(m26_full_reml$gam, 
#                       l_chla_sonde_shallow = evenly(l_chla_sonde_shallow, n = 100))
# 
# fv_chla_lab <- fitted_values(m26_full_reml$gam, 
#                     data = df_plot_chla_lab, 
#                     scale = "linear predictor") %>%
#   select(-.row, -l_chla_sonde_shallow, -site_depth) %>%
#   mutate(name = "chla_lab_shallow") %>%
#   rename(value = chla_lab_shallow)
# 
# fv_depth <- fitted_values(m26_full_reml$gam, 
#                              data = df_plot_depth, 
#                              scale = "linear predictor") %>%
#   select(-.row, -l_chla_sonde_shallow, -chla_lab_shallow) %>%
#   mutate(name = "site_depth") %>%
#   rename(value = site_depth)
# 
# fv_sonde <- fitted_values(m26_full_reml$gam, 
#                              data = df_plot_sonde, 
#                              scale = "linear predictor") %>%
#   select(-.row, -site_depth, -chla_lab_shallow) %>%
#   mutate(name = "l_sonde_chla_shallow") %>%
#   rename(value = l_chla_sonde_shallow)
# 
# 
# # Plot using ggplot2
# ggplot(bind_rows(fv_chla_lab, fv_depth, fv_sonde),
#        aes(x = value, y = .fitted)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2) +
#   facet_wrap(~name, scale = "free_x")
# ggsave("reports/ch4_tot_partial_dependence_plots.tiff")

# 5.29----
# refit with gamma distribution to ensure no negative values
m27_full_reml_gamma <- mgcv::gamm(
  ch4_total ~ chla_lab_shallow + # m27_full_reml found this to be linear
    s(l_chla_sonde_shallow, fx = FALSE, k = -1, bs = "cr") +
    s(site_depth, fx = FALSE, k = -1, bs = "cr"),
  data = dat_trans,
  # weights = vf2, # must be respecified to use with gamma distribution
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  family = Gamma(link = "log"), # fails to converge with identity
  control = lmc,
  method = "REML"
)
summary(m27_full_reml_gamma$gam) # everything significant n = 1547
anova(m27_full_reml_gamma$gam) # depth and sonde are non-linear


# 5.30----
# partial dependence plots for smoothers n gamma model
gratia::draw(m27_full_reml_gamma, all_terms = TRUE) # only get smoothed terms

# alternative method for partial dependence plots
library(gratia)
# Get data for plotting the linear effect of chla_lab_shallow
df_plot_chla_lab <- data_slice(m27_full_reml_gamma$gam,
                      chla_lab_shallow = evenly(chla_lab_shallow, n = 100))
df_plot_depth <- data_slice(m27_full_reml_gamma$gam,
                      site_depth = evenly(site_depth, n = 100))
df_plot_sonde <- data_slice(m27_full_reml_gamma$gam,
                      l_chla_sonde_shallow = evenly(l_chla_sonde_shallow, n = 100))

fv_chla_lab <- fitted_values(m27_full_reml_gamma$gam,
                    data = df_plot_chla_lab,
                    scale = "response") %>% # not log scale
  select(-.row, -l_chla_sonde_shallow, -site_depth) %>%
  mutate(name = "chla_lab_shallow") %>%
  rename(value = chla_lab_shallow)

fv_depth <- fitted_values(m27_full_reml_gamma$gam,
                             data = df_plot_depth,
                             scale = "response") %>% # not log scale
  select(-.row, -l_chla_sonde_shallow, -chla_lab_shallow) %>%
  mutate(name = "site_depth") %>%
  rename(value = site_depth)

fv_sonde <- fitted_values(m27_full_reml_gamma$gam,
                             data = df_plot_sonde,
                             scale = "response") %>% # not log scale
  select(-.row, -site_depth, -chla_lab_shallow) %>%
  mutate(name = "l_sonde_chla_shallow") %>%
  rename(value = l_chla_sonde_shallow)


# Plot using ggplot2
ggplot(bind_rows(fv_chla_lab, fv_depth, fv_sonde) ,
       aes(x = value, y = .fitted)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2) +
  facet_wrap(~name, scale = "free_x")
ggsave("reports/ch4_tot_partial_dependence_plots_gamma.tiff")

# 5.31----
# predicted vs observed
predicted_values <- predict(m27_full_reml_gamma$gam, type = "response")
observed_values <- m27_full_reml_gamma$gam$model$ch4_total

plot_data <- data.frame(observed = observed_values, 
                        predicted = predicted_values)

ggplot(plot_data, aes(x = observed, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Observed vs. Predicted Values from mgcv::gamm",
       x = "Observed Values", y = "Predicted Values") +
  theme_minimal()
ggsave("reports/ch4_tot_gamm_predicted_observed.tiff")



tic()
gam_object <- map(formula_list, ~ mgcv::gam(., data = dat_trans, family = gaussian()))
toc()




return(gam_object)
}
