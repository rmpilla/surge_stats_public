run_gamm <- function(modeling_vars_final,
                     dat_trans) {

# CREATE SOME GAM MODELS!

# interrogate modeling_vars_final
modeling_vars_final # a list of all possible combinations of predictor groups.

# 1. MODEL SET UP-----
  
## 1.1 PREDICTORS-----
# create variable to enable visits to be nested in lake_id in random
# This combines the lake_id and visit (e.g., "1004.visit1", "1021.Visit1", "258.visit2")
  dat_trans$lake_visit_nested <- interaction(dat_trans$lake_id, dat_trans$visit, sep = "_")
  
# CREATE LIST OF ALL POSSIBLE COMBINATIONS OF RESPONSE AND
# PREDICTOR VARIABLES  
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

## 1.2. DEFINE WEIGHTS----
# Add weights argument. Lets start with sa_e_i_total model
# consider multiple specifications
vf1 <- varIdent(form = ~1|lake_id)
vf2 <- varIdent(form = ~1|ag_eco9_nm)
vf3 <- varIdent(form = ~1|lake_id * ag_eco9_nm)

## 1.3 CONVERGENCE ISSUES----
# arguments to improve convergence 
lmc = lmeControl(niterEM = 5000, msMaxIter = 1000)


# 2. RUN GAMS WITH ALL SMOOTHERS----
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
sa_e_i_total_gam <- gam_sa_e_i_total$sa_e_i_total_gam # no random, no weights
sa_e_i_total <- gam_sa_e_i_total$sa_e_i_total # random, no weights
#sa_e_i_total_vf2 <- gam_sa_e_i_total$sa_e_i_total_vf2 # random, VF2, failed
#sa_e_i_total_vf3 <- gam_sa_e_i_total$sa_e_i_total_vf3 # random, VF3, failed
sa_e_i_total_vf2_sp <- gam_sa_e_i_total$sa_e_i_total_vf2_sp # weight, random, spatial


# diagnostic plots (Zuur page 59 for guidance)
# plot 1: QQ plot
# plot 2: resid vs linear predictor (used to assess homogeneity)
# plot 3: histograms:: distribution of residuals
# plot 4: observed vs fitted
# all have problems, vf2 looks best
gam.check(sa_e_i_total_gam$gam) # no weights, no random
gam.check(sa_e_i_total$gam) # random, no weights
#gam.check(sa_e_i_total_vf2$gam) # random, weights ~ ecoregion
gam.check(sa_e_i_total_vf2_sp$gam) # random, weights ~ ecoregion, spatial
  
  
# anova to see which model is prefferred
# model with random + vf2 + spatial is preferred
anova(sa_e_i_total_gam$lme, sa_e_i_total$lme, 
       sa_e_i_total_vf2_sp$lme)



# extract normalized residuals from best model
# the gamm is a list, residuals must be extracted from the lme component
E1 <- resid(sa_e_i_total_vf2_sp$lme, type = "normalized")
length(E1) #1275
nrow(dat_trans) #2368
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
# step 1. s(wind_speed_trap) p = 0.965
sa_e_i_total_vf2_sp_drop1 <- mgcv::gamm(update(formula_list[[1]], ~ . -s(wind_speed_trap, fx = FALSE, k = -1, bs = "cr")), 
                                        data = dat_trans,
                                        weights = vf2, 
                                        random = list(lake_id =~ 1),
                                        correlation = corGaus(form = ~long + lat|visit), # corSpher failed
                                        control = lmc,
                                        method = "REML")
summary(sa_e_i_total_vf2_sp_drop1$gam)

# step 2. s(turbidity_deep) p = 0.89
# this failed to converge
sa_e_i_total_vf2_sp_drop2 <- mgcv::gamm(
  update(
    formula_list[[1]], ~ . - s(wind_speed_trap, fx = FALSE, k = -1, bs = "cr") - 
      s(turbidity_deep, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSphere failed
  control = lmc,
  method = "REML"
)
summary(sa_e_i_total_vf2_sp_drop2$gam)

# step 3. removing s(turbidity_deep) didn't converge, try removing
# s(turbidity_deep) (p = 0.89) and the next least significant term 
# s(l_precipitation_trap) (p=0.83)
# this failed to converge
sa_e_i_total_vf2_sp_drop2 <- mgcv::gamm(
  update(
    formula_list[[1]], ~ . - s(wind_speed_trap, fx = FALSE, k = -1, bs = "cr") - 
      s(turbidity_deep, fx = FALSE, k = -1, bs = "cr") -
      s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(sa_e_i_total_vf2_sp_drop2$gam)


# step 4. removing s(turbidity_deep) and s(l_precipitation_trap)
# failed to converge. Try again with a greater number of iterations
# this also failed.
sa_e_i_total_vf2_sp_drop2 <- mgcv::gamm(
  update(
    formula_list[[1]], ~ . - s(wind_speed_trap, fx = FALSE, k = -1, bs = "cr") - 
      s(turbidity_deep, fx = FALSE, k = -1, bs = "cr") -
      s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML"
)
summary(sa_e_i_total_vf2_sp_drop2$gam)

# 3. TOTAL GAM, UNTRANSFORMED---- 
## 3.1 minimize smoothers----

# perhaps we have too many smoothers to allow for convergence.
# try smoothers for predictors with edf > 1.1 and those we think are important
sa_e_i_total_formula <- modeling_vars_final$sa_e_i_type %>%
  # add smoother term s() and fit details. 
  # fx = FALSE, k = -1 instructs model to use cross validation to determine amount of smoothing
  # bs=cr means use cubic regression spline
  sub("chla_lab_shallow",
      "s(chla_lab_shallow, fx = FALSE, k = -1, bs = \"cr\")",
      .) %>%
  sub("l_chla_sonde_shallow",
      "s(l_chla_sonde_shallow, fx = FALSE, k = -1, bs = \"cr\")",
      .) %>%
  sub("site_depth",
      "s(site_depth, fx = FALSE, k = -1, bs = \"cr\")",
      .) %>%
  sub("temp_deep",
      "s(temp_deep, fx = FALSE, k = -1, bs = \"cr\")",
      .) %>%
  # collapse into one string
  paste(., collapse = "+") %>%
  # Now add response variables
  paste0("ch4_total ~ ", .) %>%
  # convert to formula
  as.formula(.)



# this converged quickly  
sa_e_i_total_vf2_sp_2 <- mgcv::gamm(
  sa_e_i_total_formula,
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~ 1, lake_visit_nested =~ 1), # random visit effect nested in lake
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(sa_e_i_total_vf2_sp_2$gam)
anova(sa_e_i_total_vf2_sp_2$gam)

## 3.1----
# variable selection is tricky because of the nominal variable "ag_eco9_nm"
# Neither summary nor ANOVA give good p-values. Use liklihood ratio test (ML)
# to assess significance of ag_eco9_nm, then remove least significant term.
m1_full <- mgcv::gamm(
  sa_e_i_total_formula,
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "ML" # for log liklihood test
)
summary(m1_full$lme) # 1275 observations

m1_a <- mgcv::gamm(
  update(sa_e_i_total_formula, ~ . - ag_eco9_nm),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "ML"
)
summary(m1_a$lme) # 1275 observations
anova(m1_full$lme, m1_a$lme) # ag_eco9 is 0.018, hold onto it

## 3.2----
# based on summary, and the test for ag_eco9, chl_predicted_sample_month_visit1
# is least significant
# remove and refit
m2_full_reml <- mgcv::gamm(
  update(sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m2_full_reml$gam) # l_tp_shallow p = 0.911

# refit with reml in prep for log liklihood test to asses ag_eco9_nm
m2_full_ml <- mgcv::gamm(
  update(sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "ML"
)

m2_a_ml <- mgcv::gamm(
  update(sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - ag_eco9_nm),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "ML"
)
anova(m2_full_ml$lme, m2_a_ml$lme) # ag_eco9 is 0.0178, hold onto it

## 3.3----
# based on summary (on the REML model) and the test for ag_eco9_nm, l_tp_shallow
# is least significant. Remove and refit.
m3_full_reml <- mgcv::gamm(
  update(sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
           l_tp_shallow),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m3_full_reml$gam) # n=1175, turbidity_deep p = 0.85

# since many variables have p-values far larger than ag_eco9_nm, will stop
# testing ag_eco9_nm. 

## 3.4----
# based on summary (on the REML model) turbidity_deep 
# is least significant. Remove and refit.
m4_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep
    ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m4_full_reml$gam) #  site depth  p = 0.945, n = 1337

## 3.5----
# based on summary (on the REML model) site_depth 
# is least significant. Remove and refit.
m5_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m5_full_reml$gam) #  year_completed  p = 0.875, n = 1337

## 3.6----
# based on summary (on the REML model) year_completed
# is least significant. Remove and refit.
m6_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m6_full_reml$gam) #  l_residence_time p = 0.74, n = 1337


## 3.7----
# based on summary (on the REML model) l_residence_time
# is least significant. Remove and refit.
m7_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m7_full_reml$gam) #  l_precipitation_trap p = 0.70, n = 1337

## 3.8----
# based on summary (on the REML model) l_precipitation_trap p = 0.70
# is least significant. Remove and refit.
m8_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m8_full_reml$gam) # e_i_typestorage p = 0.59, n = 1337


## 3.9----
# based on summary (on the REML model) e_i_typestorage
# is least significant. Remove and refit.
m9_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)

# removing e_i brought in many more observations
summary(m9_full_reml$gam) #  doc_predicted_sample_month_visit1 p = 0.98, n = 1616

# removing e_i brought in several hundred observations. Add site-depth back
# in, refit, see if it is significant with more observations
mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
) %>% purrr::pluck("gam") %>% 
summary() # s(site_depth) still doesn't do anything for us

# continue by removing doc_predicted_sample_month_visit1 

## 3.10----
# based on summary (on the REML model) doc_predicted_sample_month_visit1
# is least significant. Remove and refit.
m10_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ .  - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m10_full_reml$gam) # tn_shallow p = 0.92, n = 1724

## 3.11----
# based on summary (on the REML model) tn_shallow
# is least significant. Remove and refit.
m11_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ .  - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m11_full_reml$gam) # l_nh4_shallow p = 0.76, n = 1724

## 3.12----
# based on summary (on the REML model) l_nh4_shallow
# is least significant. Remove and refit.
m12_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow - l_nh4_shallow
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m12_full_reml$gam) # l_op_shallow  p = 0.65, n = 1724


## 3.13----
# based on summary (on the REML model) l_op_shallow
# is least significant. Remove and refit.
m13_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow - l_nh4_shallow -
      l_op_shallow
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m13_full_reml$gam) # circularity  p = 0.58, n = 1724


## 3.14----
# based on summary (on the REML model) circularity 
# is least significant. Remove and refit.
# oh oh, not converging
m14_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow - l_nh4_shallow -
      l_op_shallow - circularity
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML"
)
summary(m14_full_reml$gam) 


## 3.15----
## 3.14 didn't converge. try removing next 2 least significant predictors.
# based on summary (on the REML model) circularity and mean_depth 
# are least significant. Remove and refit.
# oh, oh, wont converge. Try with simpler random structure
m15_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow - l_nh4_shallow -
      l_op_shallow - circularity - mean_depth
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1), # removed lake_visit_nested =~ 1
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML"
)
summary(m15_full_reml$gam) # l_so4_shallow  p = 0.61, n = 1724!



## 3.16----
# based on summary (on the REML model) l_so4_shallow 
# is least significant. Remove and refit.
m16_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow - l_nh4_shallow -
      l_op_shallow - circularity - mean_depth - l_so4_shallow
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m16_full_reml$gam) # l_toc_shallow p = 0.39, n = 1787



## 3.17----
# based on summary (on the REML model)  l_toc_shallow
# is least significant. Remove and refit.
m17_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow - l_nh4_shallow -
      l_op_shallow - circularity - mean_depth - l_so4_shallow -
      l_toc_shallow
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m17_full_reml$gam) # temp_shallow P = 0.35, n = 1815

## 3.18----
# based on summary (on the REML model)  temp_shallow 
# is least significant. Remove and refit.
m18_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow - l_nh4_shallow -
      l_op_shallow - circularity - mean_depth - l_so4_shallow -
      l_toc_shallow - temp_shallow
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m18_full_reml$gam) # l_precipitation_chamber  P = 0.33, n = 1815


## 3.19----
# based on summary (on the REML model)  l_precipitation_chamber 
# is least significant. Remove and refit.
m19_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow - l_nh4_shallow -
      l_op_shallow - circularity - mean_depth - l_so4_shallow -
      l_toc_shallow - temp_shallow - l_precipitation_chamber
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m19_full_reml$gam) # l_wind_speed_chamber  p = 0.2, n = 1815

## 3.20----
# based on summary (on the REML model)  l_wind_speed_chamber  
# is least significant. Remove and refit.
m20_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow - l_nh4_shallow -
      l_op_shallow - circularity - mean_depth - l_so4_shallow -
      l_toc_shallow - temp_shallow - l_precipitation_chamber -
      l_wind_speed_chamber
    ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m20_full_reml$gam) # l_buoyf p = 0.20, n = 1815

## 3.21----
# based on summary (on the REML model)  l_buoyf 
# is least significant. Remove and refit.
m21_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow - l_nh4_shallow -
      l_op_shallow - circularity - mean_depth - l_so4_shallow -
      l_toc_shallow - temp_shallow - l_precipitation_chamber -
      l_wind_speed_chamber - l_buoyf
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m21_full_reml$gam) # ok, p values are getting small now. retest ecoregion

# Neither summary nor ANOVA give good p-values. Use liklihood ratio test (ML)
# to assess significance of ag_eco9_nm, then remove least significant term.
m21_full_ml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow - l_nh4_shallow -
      l_op_shallow - circularity - mean_depth - l_so4_shallow -
      l_toc_shallow - temp_shallow - l_precipitation_chamber -
      l_wind_speed_chamber - l_buoyf
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "ML" # for log liklihood test
)
summary(m21_full_ml$gam) # 1547

m21_full_ml_a <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow - l_nh4_shallow -
      l_op_shallow - circularity - mean_depth - l_so4_shallow -
      l_toc_shallow - temp_shallow - l_precipitation_chamber -
      l_wind_speed_chamber - l_buoyf - ag_eco9_nm
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1, lake_visit_nested =~ 1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "ML"
)
summary(m21_full_ml_a$gam) # 1877 observations
anova(m21_full_ml$lme, m21_full_ml_a$lme) # ag_eco9 is 0.045, keep it 


## 3.22----
# based on summary (on the REML model)  do_deep (p=0.05976)
# is least significant. Remove and refit.
m22_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow - l_nh4_shallow -
      l_op_shallow - circularity - mean_depth - l_so4_shallow -
      l_toc_shallow - temp_shallow - l_precipitation_chamber -
      l_wind_speed_chamber - l_buoyf - do_deep
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m22_full_reml$gam) # surface area is 0.04. smoothers are significant.

# test ecoregion one last time

# Neither summary nor ANOVA give good p-values. Use liklihood ratio test (ML)
# to assess significance of ag_eco9_nm, then remove least significant term.
m22_full_ml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow - l_nh4_shallow -
      l_op_shallow - circularity - mean_depth - l_so4_shallow -
      l_toc_shallow - temp_shallow - l_precipitation_chamber -
      l_wind_speed_chamber - l_buoyf - do_deep
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "ML" # for log liklihood test
)
summary(m22_full_ml$gam) # 1547

m22_full_ml_a <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow - l_nh4_shallow -
      l_op_shallow - circularity - mean_depth - l_so4_shallow -
      l_toc_shallow - temp_shallow - l_precipitation_chamber -
      l_wind_speed_chamber - l_buoyf - do_deep - ag_eco9_nm
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "ML"
)
summary(m22_full_ml_a$gam) # 1547 observations
anova(m22_full_ml$lme, m22_full_ml_a$lme) # ag_eco9 is 0.22 

# remove ecoregion, p = 0.0503


## 3.23----
# based on summary (on the REML model) and anova on the ML (test for ag_eco)
# ag_eco9_nm  is least significant. Remove and refit.
m23_full_reml <- mgcv::gamm(
  update(
    sa_e_i_total_formula, ~ . - chl_predicted_sample_month_visit1 - 
      l_tp_shallow - turbidity_deep - 
      s(site_depth, fx = FALSE, k = -1, bs = "cr") - 
      year_completed - l_residence_time - l_precipitation_trap - e_i_type -
      doc_predicted_sample_month_visit1 - tn_shallow - l_nh4_shallow -
      l_op_shallow - circularity - mean_depth - l_so4_shallow -
      l_toc_shallow - temp_shallow - l_precipitation_chamber -
      l_wind_speed_chamber - l_buoyf - do_deep - ag_eco9_nm
  ),
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m23_full_reml$gam) #  everything significatn, n = 1892




## 3.24---
# refit model with smoothers for all predictors, see if EDF suggests linear or not
m24_full_reml <- mgcv::gamm(
  ch4_total ~ s(l_surface_area, fx = FALSE, k = -1, bs = "cr") +
    s(chla_lab_shallow, fx = FALSE, k = -1, bs = "cr") +
    s(l_chla_sonde_shallow, fx = FALSE, k = -1, bs = "cr") +
    s(temp_deep, fx = FALSE, k = -1, bs = "cr"), 
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m24_full_reml$gam) # everything significant n = 1892
anova(m24_full_reml$gam) # SA and temp are linear

## 3.25---
# refit model with smoothers for only non-linear predictors (chla lab, chla sonde)
m25_full_reml <- mgcv::gamm(
  ch4_total ~ l_surface_area +
    s(chla_lab_shallow, fx = FALSE, k = -1, bs = "cr") +
    s(l_chla_sonde_shallow, fx = FALSE, k = -1, bs = "cr") +
    temp_deep, 
  data = dat_trans,
  weights = vf2, 
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  control = lmc,
  method = "REML"
)
summary(m25_full_reml$gam) # everything significant n = 1892



## 3.28----
# partial dependence plots for smoothers
plot(m25_full_reml$gam,  
     select = 1, # chla lab
     ylab = "ch4_total",
     scheme = 2)


plot(m25_full_reml$gam,  
     select = 2, # l_chla_sonde_shallow
     ylab = "ch4_total",
     scheme = 2)




# alternative method for partial dependence plots
library(gratia)
# Get data for plotting the linear effect of surface area
df_plot_sa <- data_slice(m25_full_reml$gam,
                      l_surface_area = evenly(l_surface_area, n = 100))
df_plot_temp <- data_slice(m25_full_reml$gam,
                      temp_deep = evenly(temp_deep, n = 100))


fv_sa <- fitted_values(m25_full_reml$gam,
                    data = df_plot_sa,
                    scale = "linear predictor") %>%
  select(-.row, -l_chla_sonde_shallow, -chla_lab_shallow, -temp_deep) %>%
  mutate(name = "l_surface_area") %>%
  rename(value = l_surface_area)

fv_temp <- fitted_values(m25_full_reml$gam,
                             data = df_plot_temp,
                             scale = "linear predictor") %>%
  select(-.row, -l_chla_sonde_shallow, -chla_lab_shallow, -l_surface_area) %>%
  mutate(name = "temp_deep") %>%
  rename(value = temp_deep)


# Plot using ggplot2
ggplot(bind_rows(fv_sa, fv_temp),
       aes(x = value, y = .fitted)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2) +
  facet_wrap(~name, scale = "free_x")
ggsave("reports/ch4_tot_partial_dependence_plots.tiff")

## 3.29----
# refit with gamma distribution to ensure no negative values
m29_full_reml_gamma <- mgcv::gamm(
  ch4_total ~ l_surface_area +
    s(chla_lab_shallow, fx = FALSE, k = -1, bs = "cr") +
    s(l_chla_sonde_shallow, fx = FALSE, k = -1, bs = "cr") +
    temp_deep,
  data = dat_trans,
  # weights = vf2, # must be respecified to use with gamma distribution
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  family = Gamma(link = "log"), # fails to converge with identity
  control = lmc,
  method = "REML"
)
summary(m29_full_reml_gamma$gam) # everything significant n = 1892
anova(m29_full_reml_gamma$gam) # chla lab and sonde are non-linear


## 3.30----
# partial dependence plots for smoothers n gamma model
png("reports/ch4_tot_partial_dependence_plots_gamma_smooth.png")
gratia::draw(m29_full_reml_gamma, all_terms = TRUE) # only get smoothed terms
dev.off()

# alternative method for partial dependence plots
library(gratia)
# Get data for plotting the linear effect of chla_lab_shallow
df_plot_sa <- data_slice(m29_full_reml_gamma$gam,
                         l_surface_area = evenly(l_surface_area, n = 100))

df_plot_temp <- data_slice(m29_full_reml_gamma$gam,
                           temp_deep = evenly(temp_deep, n = 100))


fv_sa <- fitted_values(m29_full_reml_gamma$gam,
                       data = df_plot_sa,
                       scale = "linear predictor") %>%
  select(-.row, -l_chla_sonde_shallow, -chla_lab_shallow, -temp_deep) %>%
  mutate(name = "l_surface_area") %>%
  rename(value = l_surface_area)

fv_temp <- fitted_values(m29_full_reml_gamma$gam,
                         data = df_plot_temp,
                         scale = "linear predictor") %>%
  select(-.row, -l_chla_sonde_shallow, -chla_lab_shallow, -l_surface_area) %>%
  mutate(name = "temp_deep") %>%
  rename(value = temp_deep)


# Plot using ggplot2
ggplot(bind_rows(fv_sa, fv_temp),
       aes(x = value, y = .fitted)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2) +
  facet_wrap(~name, scale = "free_x")
ggsave("reports/ch4_tot_partial_dependence_plots_gammma_linear.tiff")

## 3.31----
# predicted vs observed
predicted_values <- predict(m29_full_reml_gamma$gam, type = "response")
observed_values <- m29_full_reml_gamma$gam$model$ch4_total

plot_data <- data.frame(observed = observed_values, 
                        predicted = predicted_values)

ggplot(plot_data, aes(x = observed, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Observed vs. Predicted Values from mgcv::gamm",
       x = "Observed Values", y = "Predicted Values") +
  theme_minimal()
ggsave("reports/ch4_tot_gamm_predicted_observed_all.tiff")

# zoom into area with most data
ggplot(plot_data, aes(x = observed, y = predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Observed vs. Predicted Values from mgcv::gamm",
       x = "Observed Values", y = "Predicted Values") +
  theme_minimal() +
  xlim(0, 5000)
ggsave("reports/ch4_tot_gamm_predicted_observed_limited_xaxis.tiff")

tic()
gam_object <- map(formula_list, ~ mgcv::gam(., data = dat_trans, family = gaussian()))
toc()

## 3.32----
# quick refit with log transformed data
m30_full_reml <- mgcv::gamm(
  log10(ch4_total) ~ l_surface_area +
    s(chla_lab_shallow, fx = FALSE, k = -1, bs = "cr") +
    s(l_chla_sonde_shallow, fx = FALSE, k = -1, bs = "cr") +
    temp_deep,
  data = dat_trans,
  weights = vf2, # must be respecified to use with gamma distribution
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit), # corSpher failed
  #family = Gamma(link = "log"), # fails to converge with identity
  control = lmc,
  method = "REML"
)
summary(m29_full_reml_gamma$gam) # everything significant n = 1892
anova(m29_full_reml_gamma$gam) # chla lab and sonde are non-linear


# 6 TOTAL GAM, LOG TRANSFORMED----
# rerun model 5 with log-transformed response variable
# start with a bunch of smoothers and inspect EDF values
# Need to simplify model (no weights, random, spatial) to achieve
# convergence
sa_e_i_log_total_formula <- as.character(formula_list$sa_e_i_total)[3] %>%
  paste0("log10(ch4_total) ~", .) %>%
  as.formula(.)
  
# this simplified model converges
# Does not converge with random lake effect
m6.1_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_trans,
  control = lmc,
  method = "REML" # 
)

summary(m6.1_full$gam)

# based on summary output, the following predictors are linear and 
# can be modeled without smoother. This should help with convergence
# issues with more complicated models.
sa_e_i_log_total_formula <- sa_e_i_log_total_formula %>%
  as.character() %>%
  pluck(3) %>% # grab predictor variables
  str_split_1(., pattern = "\\+") %>% # separate predictors
  trimws(.) %>% # trim whitespace for easier matching
  replace(.,
          . == "s(chla_lab_shallow, fx = FALSE, k = -1, bs = \"cr\")",
          "chla_lab_shallow") %>%
  replace(.,
          . == "s(l_tp_shallow, fx = FALSE, k = -1, bs = \"cr\")",
      "l_tp_shallow") %>%
  replace(.,
          . == "s(l_op_shallow, fx = FALSE, k = -1, bs = \"cr\")",
          "l_op_shallow") %>%
  replace(.,
          . == "s(chl_predicted_sample_month_visit1, fx = FALSE, k = -1, bs = \"cr\")",
          "chl_predicted_sample_month_visit1") %>%
  replace(.,
          . == "s(doc_predicted_sample_month_visit1, fx = FALSE, k = -1, bs = \"cr\")",
          "doc_predicted_sample_month_visit1") %>%
  replace(.,
          . == "s(l_precipitation_chamber, fx = FALSE, k = -1, bs = \"cr\")",
          "l_precipitation_chamber") %>%
  replace(.,
          . == "s(temp_shallow, fx = FALSE, k = -1, bs = \"cr\")",
          "temp_shallow") %>%
  replace(.,
          . == "s(do_deep, fx = FALSE, k = -1, bs = \"cr\")",
          "do_deep") %>%
  paste(., collapse = " + ") %>%
  paste0("log10(ch4_total) ~ ", .) %>%
  as.formula()

### 6.2----
# rerun with fewer smoothers + random effect
m6.2_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_trans,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge
  method = "REML" # 
)
summary(m6.2_full$gam) # many more smooth terms have DF = 1. Remove smooths

## 6.3 remove smoothers---- 
# based on summary output, the following predictors are linear and 
# can be modeled without smoother. This should help with convergence
# issues with more complicated models.
sa_e_i_log_total_formula <- sa_e_i_log_total_formula %>%
  as.character() %>%
  pluck(3) %>% # grab predictor variables
  str_split_1(., pattern = "\\+") %>% # separate predictors
  trimws(.) %>% # trim whitespace for easier matching
  gsub("\n    ", "", .) %>% # remove \n for easier matching
  replace(.,
          . == "s(temp_deep, fx = FALSE, k = -1, bs = \"cr\")",
          "temp_deep") %>%
  replace(.,
          . == "s(l_nh4_shallow, fx = FALSE, k = -1, bs = \"cr\")",
          "l_nh4_shallow") %>%
  replace(.,
          . == "s(year_completed, fx = FALSE, k = -1, bs = \"cr\")",
          "year_completed") %>%
  replace(.,
          . == "s(l_surface_area, fx = FALSE, k = -1, bs = \"cr\")",
          "l_surface_area") %>%
  replace(.,
          . == "s(l_buoyf, fx = FALSE, k = -1, bs = \"cr\")",
          "l_buoyf") %>%
  replace(.,
          . == "s(e_i, fx = FALSE, k = -1, bs = \"cr\")",
          "e_i") %>%
  replace(.,
          . == "s(l_residence_time, fx = FALSE, k = -1, bs = \"cr\")",
          "l_residence_time") %>%
  paste(., collapse = " + ") %>%
  paste0("log10(ch4_total) ~ ", .) %>%
  as.formula()


## 6.4----
# refit the model with fewer smoothers
m6.3_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_trans,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge
  method = "REML" # 
)
summary(m6.3_full$gam) # wind_speed_trap has edf = 1.002, remove smooth

## 6.5 remove smoothers----
sa_e_i_log_total_formula <- sa_e_i_log_total_formula %>%
  as.character() %>%
  pluck(3) %>% # grab predictor variables
  str_split_1(., pattern = "\\+") %>% # separate predictors
  trimws(.) %>% # trim whitespace for easier matching
  gsub("\n    ", "", .) %>% # remove \n for easier matching
  replace(.,
          . == "s(wind_speed_trap, fx = FALSE, k = -1, bs = \"cr\")",
          "wind_speed_trap") %>%
  paste(., collapse = " + ") %>%
  paste0("log10(ch4_total) ~ ", .) %>%
  as.formula()


## 6.6----
# refit with fewer smoothers
m6.4_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_trans,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge
  method = "REML" # 
)
summary(m6.4_full$gam) # 

## 6.7 tweak the model----
# add weights. did not converge
m6.4a_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_trans,
  weights = vf1, # ~lake_id
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge,
  method = "REML" # 
)

# try with vf2 weights
# yes, converged
m6.4b_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)


# can we add spatial correlation too?
# did not converge
m6.4c_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)

## 6.4----
# proceed with backward selection with random + weights model for now.
# I can come back and try to get weights spatial to converge.
summary(m6.4b_full$gam) # do_deep 0.996969


## 6.5----
# based on summary (on the REML model) do_deep
# is least significant. Remove and refit.
m6.4_drop2 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmc,
  method = "REML" 
)
summary(m6.4_drop2$gam) # l_nh4_shallow p = 0.91

## 6.6----
# based on summary (on the REML model) l_nh4_shallow
# is least significant. Remove and refit.
m6.4_drop3 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow
           ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmc,
  method = "REML" 
)
summary(m6.4_drop3$gam) # chl_predicted_sample_month_visit1 p = 0.85

## 6.7----
# based on summary (on the REML model) chl_predicted_sample_month_visit1
# is least significant. Remove and refit.
m6.4_drop4 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmc,
  method = "REML" 
)
summary(m6.4_drop4$gam) # l_op_shallow p = 0.56

## 6.8----
# based on summary (on the REML model) l_op_shallow)
# is least significant. Remove and refit.
m6.4_drop5 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmc,
  method = "REML" 
)
summary(m6.4_drop5$gam) # l_buoyf p = 0.49

## 6.9----
# based on summary (on the REML model) l_buoyf
# is least significant. Remove and refit.
# Needed more iterations to converge.
m6.4_drop6 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop6$gam) # l_precipitation_chamber p=0.42


## 6.10----
# based on summary (on the REML model) l_precipitation_chamber
# is least significant. Remove and refit.
# won't converge. remove next least significant term instead
# s(circularity) p=0.32)
m6.4_drop7 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr")
           
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop7$gam) # l_precipitation_chamber p=0.42
anova(m6.4_drop7$gam) # ecoregion significant

## 6.11----
# based on summary (on the REML model) l_precipitation_trap
# is least significant. Remove and refit.
m6.4_drop8 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr") -
           l_precipitation_chamber
         
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop8$gam) # chla_lab_shallow p = 0.40
anova(m6.4_drop8$gam) # ecoregion significant


## 6.12----
# based on summary (on the REML model) chla_lab_shallow
# is least significant. Remove and refit.
m6.4_drop9 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr") -
           l_precipitation_chamber -
           chla_lab_shallow
         
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop9$gam) # year_completed 0.33

## 6.13----
# based on summary (on the REML model) year_completed
# is least significant. Remove and refit.
m6.4_drop10 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr") -
           l_precipitation_chamber -
           chla_lab_shallow -
           year_completed
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop10$gam) # temp_deep p = 0.22

## 6.14----
# based on summary (on the REML model) temp_deep
# is least significant. Remove and refit.
m6.4_drop11 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr") -
           l_precipitation_chamber -
           chla_lab_shallow -
           year_completed -
           temp_deep
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop11$gam) # s(turbidity_deep) p = 0.27


## 6.15----
# based on summary (on the REML model) temp_deep
# is least significant. Remove and refit.
m6.4_drop12 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr") -
           l_precipitation_chamber -
           chla_lab_shallow -
           year_completed -
           temp_deep -
           s(turbidity_deep, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop12$gam) # wind_speed_trap p = 0.12

## 6.16----
# based on summary (on the REML model) wind_speed_trap
# is least significant. Remove and refit.
m6.4_drop13 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr") -
           l_precipitation_chamber -
           chla_lab_shallow -
           year_completed -
           temp_deep -
           s(turbidity_deep, fx = FALSE, k = -1, bs = "cr") -
           wind_speed_trap
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop13$gam) # s(l_wind_speed_chamber) p = 0.1

## 6.17----
# based on summary (on the REML model) s(l_wind_speed_chamber)
# is least significant. Remove and refit.
m6.4_drop14 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr") -
           l_precipitation_chamber -
           chla_lab_shallow -
           year_completed -
           temp_deep -
           s(turbidity_deep, fx = FALSE, k = -1, bs = "cr") -
           wind_speed_trap -
           s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop14$gam) # s(l_toc_shallow) p = 0.08


## 6.18----
# based on summary (on the REML model) s(l_toc_shallow)
# is least significant. Remove and refit.
m6.4_drop15 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr") -
           l_precipitation_chamber -
           chla_lab_shallow -
           year_completed -
           temp_deep -
           s(turbidity_deep, fx = FALSE, k = -1, bs = "cr") -
           wind_speed_trap -
           s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
           s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop15$gam) # doc_predicted_sample_month_visit1 p = 0.41


## 6.19----
# based on summary (on the REML model) doc_predicted_sample_month_visit1
# is least significant. Remove and refit.
m6.4_drop16 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr") -
           l_precipitation_chamber -
           chla_lab_shallow -
           year_completed -
           temp_deep -
           s(turbidity_deep, fx = FALSE, k = -1, bs = "cr") -
           wind_speed_trap -
           s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
           s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
           doc_predicted_sample_month_visit1
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop16$gam) # l_residence_time p = 0.19


## 6.20----
# based on summary (on the REML model) l_residence_time 
# is least significant. Remove and refit.
m6.4_drop17 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr") -
           l_precipitation_chamber -
           chla_lab_shallow -
           year_completed -
           temp_deep -
           s(turbidity_deep, fx = FALSE, k = -1, bs = "cr") -
           wind_speed_trap -
           s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
           s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
           doc_predicted_sample_month_visit1 -
           l_residence_time
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop17$gam) # s(tn_shallow) p = 0.055


## 6.21----
# based on summary (on the REML model) s(tn_shallow)
# is least significant. Remove and refit.
m6.4_drop18 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr") -
           l_precipitation_chamber -
           chla_lab_shallow -
           year_completed -
           temp_deep -
           s(turbidity_deep, fx = FALSE, k = -1, bs = "cr") -
           wind_speed_trap -
           s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
           s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
           doc_predicted_sample_month_visit1 -
           l_residence_time -
           s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop18$gam) # l_tp_shallow p = 0.09


## 6.22----
# based on summary (on the REML model) l_tp_shallow
# is least significant. Remove and refit.
m6.4_drop19 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr") -
           l_precipitation_chamber -
           chla_lab_shallow -
           year_completed -
           temp_deep -
           s(turbidity_deep, fx = FALSE, k = -1, bs = "cr") -
           wind_speed_trap -
           s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
           s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
           doc_predicted_sample_month_visit1 -
           l_residence_time -
           s(tn_shallow, fx = FALSE, k = -1, bs = "cr") -
           l_tp_shallow
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop19$gam) # s(l_precipitation_trap) p = 0.12


## 6.23----
# based on summary (on the REML model) s(l_precipitation_trap) 
# is least significant. Remove and refit.
m6.4_drop20 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr") -
           l_precipitation_chamber -
           chla_lab_shallow -
           year_completed -
           temp_deep -
           s(turbidity_deep, fx = FALSE, k = -1, bs = "cr") -
           wind_speed_trap -
           s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
           s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
           doc_predicted_sample_month_visit1 -
           l_residence_time -
           s(tn_shallow, fx = FALSE, k = -1, bs = "cr") -
           l_tp_shallow -
           s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop20$gam) # e_i p = 0.11

## 6.24----
# based on summary (on the REML model) e_i p = 0.11
# is least significant. Remove and refit.
m6.4_drop21 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr") -
           l_precipitation_chamber -
           chla_lab_shallow -
           year_completed -
           temp_deep -
           s(turbidity_deep, fx = FALSE, k = -1, bs = "cr") -
           wind_speed_trap -
           s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
           s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
           doc_predicted_sample_month_visit1 -
           l_residence_time -
           s(tn_shallow, fx = FALSE, k = -1, bs = "cr") -
           l_tp_shallow -
           s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr") -
           e_i
  ),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop21$gam) # s(l_so4_shallow) is p=0.0537
                         # mean_depth has edf = 1.001, fit as linear


## 6.25----
# based on summary (on the REML model) mean depth is linear. 
# Replace smoother with linear term
m6.4_drop22 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr") -
           l_precipitation_chamber -
           chla_lab_shallow -
           year_completed -
           temp_deep -
           s(turbidity_deep, fx = FALSE, k = -1, bs = "cr") -
           wind_speed_trap -
           s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
           s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
           doc_predicted_sample_month_visit1 -
           l_residence_time -
           s(tn_shallow, fx = FALSE, k = -1, bs = "cr") -
           l_tp_shallow -
           s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr") -
           e_i
  ) %>%
    # this bit replaces the mean_depth smoother
    as.character() %>%
    pluck(3) %>% # grab predictor variables
    str_split_1(., pattern = "\\+") %>% # separate predictors
    trimws(.) %>% # trim whitespace for easier matching
    gsub("\n    ", "", .) %>% # remove \n for easier matching
    replace(.,
            . == "s(mean_depth, fx = FALSE, k = -1, bs = \"cr\")",
            "mean_depth") %>%
    paste(., collapse = " + ") %>%
    paste0("log10(ch4_total) ~ ", .) %>%
    as.formula(),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop22$gam) # s(l_so4_shallow) is p=0.0537, but I want to keep it


## 6.26 ----
# refit with spatial
# based on summary (on the REML model) mean depth is linear. 
# Replace smoother with linear term
m6.4_drop23 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -do_deep - l_nh4_shallow -
           chl_predicted_sample_month_visit1 -
           l_op_shallow -
           l_buoyf -
           s(circularity, fx = FALSE, k = -1, bs = "cr") -
           l_precipitation_chamber -
           chla_lab_shallow -
           year_completed -
           temp_deep -
           s(turbidity_deep, fx = FALSE, k = -1, bs = "cr") -
           wind_speed_trap -
           s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = "cr") -
           s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr") -
           doc_predicted_sample_month_visit1 -
           l_residence_time -
           s(tn_shallow, fx = FALSE, k = -1, bs = "cr") -
           l_tp_shallow -
           s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr") -
           e_i
  ) %>%
    # this bit replaces the mean_depth smoother
    as.character() %>%
    pluck(3) %>% # grab predictor variables
    str_split_1(., pattern = "\\+") %>% # separate predictors
    trimws(.) %>% # trim whitespace for easier matching
    gsub("\n    ", "", .) %>% # remove \n for easier matching
    replace(.,
            . == "s(mean_depth, fx = FALSE, k = -1, bs = \"cr\")",
            "mean_depth") %>%
    paste(., collapse = " + ") %>%
    paste0("log10(ch4_total) ~ ", .) %>%
    as.formula(),
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_drop23$gam) # s(l_so4_shallow) is p = 0.057
anova(m6.4_drop23$gam) # ecoregion p < 0.001

## 6.27 rewrite model----
m6.4_final <- mgcv::gamm(
  log10(ch4_total) ~ s(l_chla_sonde_shallow, fx = FALSE, k = -1,bs = "cr") + 
    s(l_so4_shallow, fx = FALSE, k = -1, bs = "cr") +
    s(site_depth, fx = FALSE, k = -1, bs = "cr") +
    temp_shallow +  ag_eco9_nm + l_surface_area + mean_depth,
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_final$gam) # s(l_so4_shallow) is p = 0.057
anova(m6.4_final$gam) # ecoregion p < 0.001

## 6.28 inspect model----  
plot(m6.4_final$gam)

# partial dependence plots 
# library(gratia)
# gratia::draw for smoothers only
png("reports/ch4_tot_partial_dependence_plots_smooths.png")
gratia::draw(m6.4_final$gam, all_terms = TRUE) # only get smoothed terms
dev.off()

# Get data for plotting the linear effect of temp_shallow
df_plot_temp <- data_slice(m6.4_final$gam,
                         temp_shallow = evenly(temp_shallow, n = 100))

df_plot_sa <- data_slice(m6.4_final$gam,
                         l_surface_area = evenly(l_surface_area, n = 100))

df_plot_depth <- data_slice(m6.4_final$gam,
                         mean_depth = evenly(mean_depth, n = 100))

fv_temp <- fitted_values(m6.4_final$gam,
                         data = df_plot_temp,
                         scale = "linear predictor") %>%
  select(temp_shallow, .fitted, .lower_ci, .upper_ci) %>%
  mutate(name = "temp_shallow") %>%
  rename(value = temp_shallow)

fv_sa <- fitted_values(m6.4_final$gam,
                       data = df_plot_sa,
                       scale = "linear predictor") %>%
  select(l_surface_area, .fitted, .lower_ci, .upper_ci) %>%
  mutate(name = "l_surface_area") %>%
  rename(value = l_surface_area)

fv_depth <- fitted_values(m6.4_final$gam,
                         data = df_plot_depth,
                         scale = "linear predictor") %>%
  select(mean_depth, .fitted, .lower_ci, .upper_ci) %>%
  mutate(name = "mean_depth") %>%
  rename(value = mean_depth)


# Plot using ggplot2
ggplot(bind_rows(fv_sa, fv_temp, fv_depth),
       aes(x = value, y = .fitted)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2) +
  facet_wrap(~name, scale = "free_x")
ggsave("reports/ch4_tot_partial_dependence_plots_linear.tiff")


## 6.29 interactions----
# Does deep_temp interact with site_depth? Cold adaptation like in Acton?
# must pull site_depth out of smoother to include in linear interaction
m6.4_final_int1 <- mgcv::gamm(
  log10(ch4_total) ~ s(l_chla_sonde_shallow, fx = FALSE, k = -1,bs = "cr") + 
    s(l_so4_shallow, fx = FALSE, k = -1, bs = "cr") +
    temp_shallow +  ag_eco9_nm + l_surface_area + mean_depth +
     temp_deep*site_depth, # interaction
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m6.4_final_int1$gam) # interaction not significant

# what if we eliminate temp_shallow?
m6.4_final_int2 <- mgcv::gamm(
  log10(ch4_total) ~ s(l_chla_sonde_shallow, fx = FALSE, k = -1,bs = "cr") + 
    s(l_so4_shallow, fx = FALSE, k = -1, bs = "cr") +
    ag_eco9_nm + l_surface_area + mean_depth +
    temp_deep * site_depth, # interaction
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
) 
summary(m6.4_final_int2$gam) # interaction not significant


## 6.30 doc / conductivity----
# per Soued et al. 
doc_so4 <- mgcv::gamm(
  log10(ch4_total) ~ s(l_chla_sonde_shallow, fx = FALSE, k = -1,bs = "cr") + 
    s(l_so4_shallow, fx = FALSE, k = -1, bs = "cr") +
    ag_eco9_nm + l_surface_area + mean_depth +
    temp_deep + site_depth +
    log10(toc_shallow / sp_cond_shallow), # 
  data = dat_trans,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
) 
summary(doc_so4$gam) # toc not significant

# significant in a bivariate analysis
lm(log10(ch4_total) ~ log10(toc_shallow / sp_cond_shallow),
   data = dat_trans) %>%
  summary()

# not very convincing
ggplot(dat_trans, aes(ch4_total, (toc_shallow / sp_cond_shallow))) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10()


# 7 TOTAL GAM, TRANSFORMED, ALL PREDICTORS----
# Here I rerun the gamm with log transformed CH4 total, but restrict the 
# model to a single set of observations, rather than allowing n to change
# as predictors are removed during the variable selection process.

## 7.1 define data----
# Only 7 of the 32 sites from 2016 have NLA data. The others will be
# filtered out due to missingness.
dat_7 <- dat_trans %>% 
  select(
    ch4_total, # response
    modeling_vars_final$sa_e_i, # predictor variables
    lake_id, # random effect
    long, lat, visit # spatial correlation
  ) %>%
  drop_na() # remove all rows with any NA

dim(dat_7) # 1399 observations

## 7.2 predictors----
# initial set of predictors are wrapped in smoothers.
sa_e_i_log_total_formula <- as.character(formula_list$sa_e_i_total)[3] %>%
  paste0("log10(ch4_total) ~", .) %>%
  as.formula(.)

## 7.3 basic full model----
# Won't converge, even with more iterations and simplified structure 
# (no weights, random, spatial)
m7.1_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge,
  method = "REML" # 
)

## 7.4 remove smoothers 1----
# based on summary output of model 6, the following predictors are linear and 
# can be modeled without smoother. This should help with convergence
# issues with more complicated models.
sa_e_i_log_total_formula <- sa_e_i_log_total_formula %>%
  as.character() %>%
  pluck(3) %>% # grab predictor variables
  str_split_1(., pattern = "\\+") %>% # separate predictors
  trimws(.) %>% # trim whitespace for easier matching
  replace(.,
          . == "s(chla_lab_shallow, fx = FALSE, k = -1, bs = \"cr\")",
          "chla_lab_shallow") %>%
  replace(.,
          . == "s(l_tp_shallow, fx = FALSE, k = -1, bs = \"cr\")",
          "l_tp_shallow") %>%
  replace(.,
          . == "s(l_op_shallow, fx = FALSE, k = -1, bs = \"cr\")",
          "l_op_shallow") %>%
  replace(.,
          . == "s(chl_predicted_sample_month_visit1, fx = FALSE, k = -1, bs = \"cr\")",
          "chl_predicted_sample_month_visit1") %>%
  replace(.,
          . == "s(doc_predicted_sample_month_visit1, fx = FALSE, k = -1, bs = \"cr\")",
          "doc_predicted_sample_month_visit1") %>%
  replace(.,
          . == "s(l_precipitation_chamber, fx = FALSE, k = -1, bs = \"cr\")",
          "l_precipitation_chamber") %>%
  replace(.,
          . == "s(temp_shallow, fx = FALSE, k = -1, bs = \"cr\")",
          "temp_shallow") %>%
  replace(.,
          . == "s(do_deep, fx = FALSE, k = -1, bs = \"cr\")",
          "do_deep") %>%
  paste(., collapse = " + ") %>%
  paste0("log10(ch4_total) ~ ", .) %>%
  as.formula()

## 7.5 refit with fewer smoothers----
# rerun with fewer smoothers + random effect
m7.1_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge
  method = "REML" # 
)
summary(m7.1_full$gam) # many more smooth terms have DF = 1. Remove smooths

## 7.6 remove smoothers 2---- 
# based on summary output, the following predictors are linear and 
# can be modeled without smoother. This should help with convergence
# issues with more complicated models.
sa_e_i_log_total_formula <- sa_e_i_log_total_formula %>%
  as.character() %>%
  pluck(3) %>% # grab predictor variables
  str_split_1(., pattern = "\\+") %>% # separate predictors
  trimws(.) %>% # trim whitespace for easier matching
  gsub("\n    ", "", .) %>% # remove \n for easier matching
  replace(.,
          . == "s(delta_bp_ebu, fx = FALSE, k = -1, bs = \"cr\")",
          "delta_bp_ebu") %>%
  replace(.,
          . == "s(temp_deep, fx = FALSE, k = -1, bs = \"cr\")",
          "temp_deep") %>%
  replace(.,
          . == "s(l_nh4_shallow, fx = FALSE, k = -1, bs = \"cr\")",
          "l_nh4_shallow") %>%
  replace(.,
          . == "s(l_surface_area, fx = FALSE, k = -1, bs = \"cr\")",
          "l_surface_area") %>%
  replace(.,
          . == "s(l_buoyf, fx = FALSE, k = -1, bs = \"cr\")",
          "l_buoyf") %>%
  replace(.,
          . == "s(l_residence_time, fx = FALSE, k = -1, bs = \"cr\")",
          "l_residence_time") %>%
  paste(., collapse = " + ") %>%
  paste0("log10(ch4_total) ~ ", .) %>%
  as.formula()


## 7.7 refit the model with fewer smoothers----
# 
m7.3_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge
  method = "REML" # 
)
summary(m7.3_full$gam) # several smooths with EDF ~ 1, remove smooth

## 7.8 add interactions----
# temp_deep : site_depth
# chla_lab_shallow : l_residence_time
sa_e_i_log_total_formula <- sa_e_i_log_total_formula %>%
  as.character() %>%
  pluck(3) %>% # grab predictor variables
  str_split_1(., pattern = "\\+") %>% # separate predictors
  trimws(.) %>% # trim whitespace for easier matching
  gsub("\n    ", "", .) %>% # remove \n for easier matching
  c(., 
    "temp_deep : site_depth", # s(site_depth) in model, but this works
    "chla_lab_shallow : l_residence_time" # s(e_i) can't be used in interaction
  ) %>%
  paste(., collapse = " + ") %>%
  paste0("log10(ch4_total) ~ ", .) %>%
  as.formula()

## 7.9 refit with interactions----
# doesn't converge with interactions
m7.4_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge
  method = "REML" # 
)

# crap, maybe remove smoother from all terms with edf < 2  and p >> 0.05 to 
# facilitate convergence

## 7.10 remove smoothers 3----
# remove smoother from all terms with edf < 2  and p >> 0.05 
sa_e_i_log_total_formula <- sa_e_i_log_total_formula %>%
  as.character() %>%
  pluck(3) %>% # grab predictor variables
  str_split_1(., pattern = "\\+") %>% # separate predictors
  trimws(.) %>% # trim whitespace for easier matching
  gsub("\n    ", "", .) %>% # remove \n for easier matching
  replace(.,
          . == "s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = \"cr\")",
          "l_wind_speed_chamber") %>%
  replace(.,
          . == "s(year_completed, fx = FALSE, k = -1, bs = \"cr\")",
          "year_completed") %>%
  paste(., collapse = " + ") %>%
  paste0("log10(ch4_total) ~ ", .) %>%
  as.formula()

## 7.11 refit with fewer smoothers + interaction----
# converged, yay
m7.5_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge
  method = "REML" # 
)
summary(m7.5_full$gam) # s(wind_speed_trap edf = 1.006). remove smooth

## 7.12 remove smoothers 4----
sa_e_i_log_total_formula <- sa_e_i_log_total_formula %>%
  as.character() %>%
  pluck(3) %>% # grab predictor variables
  str_split_1(., pattern = "\\+") %>% # separate predictors
  trimws(.) %>% # trim whitespace for easier matching
  gsub("\n    ", "", .) %>% # remove \n for easier matching
  replace(.,
          . == "s(wind_speed_trap, fx = FALSE, k = -1, bs = \"cr\")",
          "wind_speed_trap") %>%
  paste(., collapse = " + ") %>%
  paste0("log10(ch4_total) ~ ", .) %>%
  as.formula()

## 7.13 refit with fewer smoothers + interaction----
m7.6_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge
  method = "REML" # 
)
summary(m7.6_full$gam) #

## 7.8 tweak the model----
# add weights. did not converge
m7.6a_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  weights = vf1, # ~lake_id
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge,
  method = "REML" # 
)

# try with vf2 weights
# did not converge
m7.6b_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)

# can we add spatial correlation too?
# did not converge
m7.6c_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)

# vf2 weights model previously converged, main difference is that I added
# the interactions. Although variable selection shouldn't proceed until after
# the random components have been specified, I'm going to evaluate significance
# of interactions now. If not significant, remove and try fitting weights.

## 7.9 evaluate interactions----
m7.7_full <-  mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge
  method = "ML" # ML for variable selection via liklihood ratio test
)

m7.7_drop1 <-  mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -temp_deep:site_depth),
  data = dat_7,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge
  method = "ML" # ML for variable selection via liklihood ratio test
)

m7.7_drop2 <-  mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -chla_lab_shallow:l_residence_time),
  data = dat_7,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge
  method = "ML" # ML for variable selection via liklihood ratio test
)

anova(m7.7_full$lme, m7.7_drop1$lme) # p =0.79
anova(m7.7_full$lme, m7.7_drop2$lme) # p = 0.53

# neither 2-way interaction is significant.
# omit and refit.

## 7.10 omit interactions and refit----
# remove interactions from formula
sa_e_i_log_total_formula <- update(
  sa_e_i_log_total_formula, ~ . # formula with interactions
  -temp_deep : site_depth - # remove this interaction
    chla_lab_shallow : l_residence_time # remove this interaction
  )

# refit
m7.8_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge
  method = "REML" # 
)
summary(m7.8_full$gam)

## 7.11 tweak the model----
# add weights. did not converge
m7.8a_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  weights = vf1, # ~lake_id
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge,
  method = "REML" # 
)

# try with vf2 weights
# did not converge. What? converged earlier (see )
m7.8b_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)

# can we add spatial correlation too?
# did not converge
m7.8c_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)

# crap, as above, did not converge. Lets bend the rules and remove some
# non-significant predictors.

## 7.12 remove predictors and refit----
# chl_predicted_sample_month_visit1 p = 0.99
# do_deep p = 0.88
sa_e_i_log_total_formula <- update(
  sa_e_i_log_total_formula, ~ . # formula with interactions
  -chl_predicted_sample_month_visit1  # remove 
  - do_deep
)
# refit
m7.9_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge
  method = "REML" # 
)
summary(m7.9_full$gam)

## 7.13 tweak the model----
# add weights. did not converge
m7.9a_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  weights = vf1, # ~lake_id
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge,
  method = "REML" # 
)

# try with vf2 weights
# converged, finally
m7.9b_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)
summary(m7.9b_full$gam) # delta_bp_ebu p = 0.83

# can we add spatial correlation too?
# did not converge
m7.9c_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)

## 7.14 start variable selection----

# based on summary (on the REML model) delta_bp_ebu 
# is least significant. Remove and refit.
m7.9_drop1 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ . -
           delta_bp_ebu),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)
summary(m7.9_drop1$gam) # l_nh4_shallow p=0.56

## 7.15----
# based on summary (on the REML model) l_nh4_shallow
# is least significant. Remove and refit.
m7.9_drop2 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ .
          - delta_bp_ebu 
         - l_nh4_shallow),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)

summary(m7.9_drop2$gam) # l_wind_speed_chamber p=0.49

## 7.16----
# based on summary (on the REML model) l_wind_speed_chamber
# is least significant. Remove and refit.
# Doesn't converge. Remove next least signifcant variable too (l_precipitation_chamber )
m7.9_drop3 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ .
         - delta_bp_ebu 
         - l_nh4_shallow
         - l_wind_speed_chamber
         - l_precipitation_chamber),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)

summary(m7.9_drop3$gam) # l_buoyf p=0.3

## 7.17----
# based on summary (on the REML model) l_buoyf
# is least significant. Remove and refit.
m7.9_drop4 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ .
         - delta_bp_ebu 
         - l_nh4_shallow
         - l_wind_speed_chamber
         - l_buoyf),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)

summary(m7.9_drop4$gam) # l_precipitation_chamber p=0.51


## 7.18----
# based on summary (on the REML model) l_precipitation_chamber 
# is least significant. Remove and refit.
# doesn't converge. Remove next least significant term too (temp_deep)
m7.9_drop5 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ .
         - delta_bp_ebu 
         - l_nh4_shallow
         - l_wind_speed_chamber
         - l_buoyf
         - l_precipitation_chamber
         - temp_deep),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)

summary(m7.9_drop5$gam) # l_op_shallow p=0.36


## 7.19----
# based on summary (on the REML model) l_op_shallow 
# is least significant. Remove and refit.
# doesn't converge, remove next least significant (year_completed)
m7.9_drop6 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ .
         - delta_bp_ebu 
         - l_nh4_shallow
         - l_wind_speed_chamber
         - l_buoyf
         - l_precipitation_chamber
         - temp_deep 
         - l_op_shallow
         - year_completed),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)

summary(m7.9_drop6$gam) # s(circularity) 0.33



## 7.20----
# based on summary (on the REML model) s(circularity)
# is least significant. Remove and refit.
m7.9_drop7 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ .
         - delta_bp_ebu 
         - l_nh4_shallow
         - l_wind_speed_chamber
         - l_buoyf
         - l_precipitation_chamber
         - temp_deep 
         - l_op_shallow
         - year_completed
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         ),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)

summary(m7.9_drop7$gam) # chla_lab_shallow 0.32



## 7.21----
# based on summary (on the REML model) chla_lab_shallow 
# is least significant. Remove and refit.
m7.9_drop8 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ .
         - delta_bp_ebu 
         - l_nh4_shallow
         - l_wind_speed_chamber
         - l_buoyf
         - l_precipitation_chamber
         - temp_deep 
         - l_op_shallow
         - year_completed
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - chla_lab_shallow),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)

summary(m7.9_drop8$gam) # wind_speed_trap 0.15


## 7.22----
# based on summary (on the REML model) wind_speed_trap 
# is least significant. Remove and refit.
m7.9_drop9 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ .
         - delta_bp_ebu 
         - l_nh4_shallow
         - l_wind_speed_chamber
         - l_buoyf
         - l_precipitation_chamber
         - temp_deep 
         - l_op_shallow
         - year_completed
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - chla_lab_shallow
         - wind_speed_trap),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)

summary(m7.9_drop9$gam) #s(l_toc_shallow) p =0.06


## 7.23----
# based on summary (on the REML model) s(l_toc_shallow)
# is least significant. Remove and refit.
m7.9_drop10 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ .
         - delta_bp_ebu 
         - l_nh4_shallow
         - l_wind_speed_chamber
         - l_buoyf
         - l_precipitation_chamber
         - temp_deep 
         - l_op_shallow
         - year_completed
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - chla_lab_shallow
         - wind_speed_trap
         - s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr")
         ),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)

summary(m7.9_drop10$gam) # doc_predicted_sample_month_visit1 0.38

## 7.24----
# based on summary (on the REML model) doc_predicted_sample_month_visit1
# is least significant. Remove and refit.
m7.9_drop11 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ .
         - delta_bp_ebu 
         - l_nh4_shallow
         - l_wind_speed_chamber
         - l_buoyf
         - l_precipitation_chamber
         - temp_deep 
         - l_op_shallow
         - year_completed
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - chla_lab_shallow
         - wind_speed_trap
         - s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr")
         - doc_predicted_sample_month_visit1
  ),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)

summary(m7.9_drop11$gam) # l_residence_time 0.18


## 7.25----
# based on summary (on the REML model) l_residence_time 
# is least significant. Remove and refit.
m7.9_drop12 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ .
         - delta_bp_ebu 
         - l_nh4_shallow
         - l_wind_speed_chamber
         - l_buoyf
         - l_precipitation_chamber
         - temp_deep 
         - l_op_shallow
         - year_completed
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - chla_lab_shallow
         - wind_speed_trap
         - s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr")
         - doc_predicted_sample_month_visit1
         - l_residence_time 
  ),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)

summary(m7.9_drop12$gam) #s(tn_shallow) 0.087

## 7.26----
# based on summary (on the REML model) s(tn_shallow)
# is least significant. Remove and refit.
m7.9_drop13 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ .
         - delta_bp_ebu 
         - l_nh4_shallow
         - l_wind_speed_chamber
         - l_buoyf
         - l_precipitation_chamber
         - temp_deep 
         - l_op_shallow
         - year_completed
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - chla_lab_shallow
         - wind_speed_trap
         - s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr")
         - doc_predicted_sample_month_visit1
         - l_residence_time 
         - s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)

summary(m7.9_drop13$gam) # l_tp_shallow 0.15


## 7.27----
# based on summary (on the REML model) l_tp_shallow 
# is least significant. Remove and refit.
m7.9_drop14 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ .
         - delta_bp_ebu 
         - l_nh4_shallow
         - l_wind_speed_chamber
         - l_buoyf
         - l_precipitation_chamber
         - temp_deep 
         - l_op_shallow
         - year_completed
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - chla_lab_shallow
         - wind_speed_trap
         - s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr")
         - doc_predicted_sample_month_visit1
         - l_residence_time 
         - s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
         - l_tp_shallow 
  ),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)

summary(m7.9_drop14$gam) # s(e_i) 0.06


## 7.28----
# based on summary (on the REML model) s(e_i)
# is least significant. Remove and refit.
m7.9_drop15 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ .
         - delta_bp_ebu 
         - l_nh4_shallow
         - l_wind_speed_chamber
         - l_buoyf
         - l_precipitation_chamber
         - temp_deep 
         - l_op_shallow
         - year_completed
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - chla_lab_shallow
         - wind_speed_trap
         - s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr")
         - doc_predicted_sample_month_visit1
         - l_residence_time 
         - s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
         - l_tp_shallow 
         - s(e_i, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)

summary(m7.9_drop15$gam) # s(mean_depth) 0.053


## 7.29----
# based on summary (on the REML model) s(mean_depth)
# is least significant. Remove and refit.
m7.9_drop16 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ .
         - delta_bp_ebu 
         - l_nh4_shallow
         - l_wind_speed_chamber
         - l_buoyf
         - l_precipitation_chamber
         - temp_deep 
         - l_op_shallow
         - year_completed
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - chla_lab_shallow
         - wind_speed_trap
         - s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr")
         - doc_predicted_sample_month_visit1
         - l_residence_time 
         - s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
         - l_tp_shallow 
         - s(e_i, fx = FALSE, k = -1, bs = "cr")
         - s(mean_depth, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)

summary(m7.9_drop16$gam) # all significant so4 = 0.050171
anova(m7.9_drop16$gam) # ecoregion significant

## 7.30 refit with spatial----
m7.9_final <- mgcv::gamm(
  update(sa_e_i_log_total_formula, ~ .
         - delta_bp_ebu 
         - l_nh4_shallow
         - l_wind_speed_chamber
         - l_buoyf
         - l_precipitation_chamber
         - temp_deep 
         - l_op_shallow
         - year_completed
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - chla_lab_shallow
         - wind_speed_trap
         - s(l_toc_shallow, fx = FALSE, k = -1, bs = "cr")
         - doc_predicted_sample_month_visit1
         - l_residence_time 
         - s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
         - l_tp_shallow 
         - s(e_i, fx = FALSE, k = -1, bs = "cr")
         - s(mean_depth, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_7,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" 
)

summary(m7.9_final$gam) # all significant so4 = 0.050171
anova(m7.9_final$gam) # ecoregion significant

anova(m7.9_drop16$lme, m7.9_final$lme) # model with spatial is preferred

## 7.31 visualize results----
plot(m7.9_final)

# partial dependence plots 
# library(gratia)
# gratia::draw for smoothers only
png("reports/log_ch4_tot_partial_dependence_plots_smooths_no_2016.png")
gratia::draw(m7.9_final$gam, all_terms = TRUE) # only get smoothed terms
dev.off()

# Get data for plotting the linear effect of temp_shallow
df_plot_temp <- data_slice(m7.9_final$gam,
                           temp_shallow = evenly(temp_shallow, n = 100))

df_plot_sa <- data_slice(m7.9_final$gam,
                         l_surface_area = evenly(l_surface_area, n = 100))

df_plot_eco <- data_slice(m7.9_final$gam,
                           ag_eco9_nm = evenly(ag_eco9_nm))


fv_temp <- fitted_values(m7.9_final$gam,
                         data = df_plot_temp,
                         scale = "linear predictor") %>%
  select(temp_shallow, .fitted, .lower_ci, .upper_ci) %>%
  mutate(name = "temp_shallow") %>%
  rename(value = temp_shallow)

fv_sa <- fitted_values(m7.9_final$gam,
                       data = df_plot_sa,
                       scale = "linear predictor") %>%
  select(l_surface_area, .fitted, .lower_ci, .upper_ci) %>%
  mutate(name = "l_surface_area") %>%
  rename(value = l_surface_area)

fv_eco <- fitted_values(m7.9_final$gam,
                          data = df_plot_eco,
                          scale = "linear predictor") %>%
  select(ag_eco9_nm, .fitted, .lower_ci, .upper_ci) 


# Plot using ggplot2
p1 <- ggplot(bind_rows(fv_sa, fv_temp),
             aes(x = value, y = .fitted)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2) +
  facet_wrap(~name, scale = "free_x")

p2 <- ggplot(fv_eco) +
  geom_bar(aes(ag_eco9_nm, .fitted), stat = "identity") +
  geom_errorbar(aes(x=ag_eco9_nm, ymin=.lower_ci, ymax=.upper_ci),
                width = 0.4) +
  xlab(NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

library(gridExtra)
p1_p2 <- grid.arrange(p1, p2, nrow =1, widths = c(2/3, 1/3))

ggsave("reports/log_ch4_tot_partial_dependence_plots_linear_no_2016.tiff", 
       plot = p1_p2)


# 8 TOTAL GAM, TRANSFORMED, NO NLA PREDICTORS----
# Eliminate NLA predictors to increase n. Also eliminate other
# predictors that have lots of missingness, especially if not
# important in previous model.

## 8.1 predictors----
# NLA predictors to eliminate are those related to residence time,
# although others will be added later [1/15/2025].
# LAGOS variables missing from 8 lakes. These are not important predictors.
sa_e_i_log_total_formula <- as.character(formula_list$sa_e_i_total)[3] %>%
  paste0("log10(ch4_total) ~", .) %>%
  as.formula(.) %>%
  update(., .~.
         # exclude NLA predictors
         -s(e_i, fx = FALSE, k = -1, bs = "cr") 
         - s(l_residence_time, fx = FALSE, k = -1, bs = "cr")
         # exclude LAGOS predictors
         - s(doc_predicted_sample_month_visit1, fx = FALSE, k = -1, bs = "cr")
         - s(chl_predicted_sample_month_visit1, fx = FALSE, k = -1, bs = "cr")
         # full model won't converge with interactions. Try after smoothers
         # have been minimized
         # # interactions. Main terms are smoothers, so not sure if this is legit
         # + temp_deep:site_depth
         # + l_toc_shallow:l_so4_shallow
         )

## 8.2 define data----
dat_8 <- dat_trans %>% 
  select(
    ch4_total, # response
    modeling_vars_final$sa_e_i, # predictor variables
    lake_id, # random effect
    long, lat, visit # spatial correlation
  ) %>%
  select(-e_i, -l_residence_time, # NLA to exclude
         -doc_predicted_sample_month_visit1, # lagos to exclude
         -chl_predicted_sample_month_visit1) %>% # lagos to exclude 
  drop_na() # remove all rows with any NA

dim(dat_8) # 1849 observations, we have 2030 ch4_tot observations

## 8.3 basic full model----
# won't converge with interactions
m8.1_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_8,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge,
  method = "REML" # 
)
summary(m8.1_full$gam) # lots of smoothers with edf ~ 1, remove smooth

## 8.4 remove smoothers 1----
# based on summary output of model m8.1_full, the following predictors are linear and 
# can be modeled without smoother. This should help with convergence
# issues with more complicated models.
sa_e_i_log_total_formula <- sa_e_i_log_total_formula %>%
  as.character() %>%
  pluck(3) %>% # grab predictor variables
  str_split_1(., pattern = "\\+") %>% # separate predictors
  trimws(.) %>% # trim whitespace for easier matching
  gsub("\n    ", "", .) %>% # remove \n for easier matching
  replace(.,
          . == "s(chla_lab_shallow, fx = FALSE, k = -1, bs = \"cr\")",
          "chla_lab_shallow") %>%
  replace(.,
          . == "s(l_op_shallow, fx = FALSE, k = -1, bs = \"cr\")",
          "l_op_shallow") %>%
  replace(.,
          . == "s(l_toc_shallow, fx = FALSE, k = -1, bs = \"cr\")",
          "l_toc_shallow") %>%
  replace(.,
          . == "s(l_precipitation_chamber, fx = FALSE, k = -1, bs = \"cr\")",
          "l_precipitation_chamber") %>%
  replace(.,
          . == "s(l_wind_speed_chamber, fx = FALSE, k = -1, bs = \"cr\")",
          "l_wind_speed_chamber") %>%
  replace(.,
          . == "s(wind_speed_trap, fx = FALSE, k = -1, bs = \"cr\")",
          "wind_speed_trap") %>%
  replace(.,
          . == "s(delta_bp_ebu, fx = FALSE, k = -1, bs = \"cr\")",
          "delta_bp_ebu") %>%
  replace(.,
          . == "s(l_nh4_shallow, fx = FALSE, k = -1, bs = \"cr\")",
          "l_nh4_shallow") %>%
  replace(.,
          . == "s(l_surface_area, fx = FALSE, k = -1, bs = \"cr\")",
          "l_surface_area") %>%
  replace(.,
          . == "s(mean_depth, fx = FALSE, k = -1, bs = \"cr\")",
          "mean_depth") %>%
  replace(.,
          . == "s(l_buoyf, fx = FALSE, k = -1, bs = \"cr\")",
          "l_buoyf") %>%
  paste(., collapse = " + ") %>%
  paste0("log10(ch4_total) ~ ", .) %>%
  as.formula()

## 8.5 refit with fewer smoothers----
m8.2_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_8,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge,
  method = "REML" # 
)

# s(temp_deep) and s(site_depth) have edf ~ 3.1 and 2.4,
# but I want to test their interaction. Convert to linear
# and add interaction.
summary(m8.2_full$gam) 

## 8.6 add interactions----
sa_e_i_log_total_formula <- sa_e_i_log_total_formula %>%
  as.character() %>%
  pluck(3) %>% # grab predictor variables
  str_split_1(., pattern = "\\+") %>% # separate predictors
  trimws(.) %>% # trim whitespace for easier matching
  replace(.,
          . == "s(temp_deep, fx = FALSE, k = -1, bs = \"cr\")",
          "temp_deep") %>%
  replace(.,
          . == "s(site_depth, fx = FALSE, k = -1, bs = \"cr\")",
          "site_depth") %>%
  paste(., collapse = " + ") %>%
  paste0("log10(ch4_total) ~ ", .) %>%
  as.formula() %>%
  update(
    ., .~.
    + temp_deep:site_depth
  )

## 8.7 refit----
m8.3_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_8,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge,
  method = "REML" # 
)
summary(m8.3_full$gam) 
anova(m8.3_full$gam) # temp_deep:site_depth p = 0.9!


## 8.9 remove interaction----
# temp_deep:site_depth is not significant. Remove interaction and add
# smoother to main terms
sa_e_i_log_total_formula <- update(
  sa_e_i_log_total_formula, .~.
  - temp_deep:site_depth
  - temp_deep
  - site_depth
  + s(temp_deep, fx = FALSE, k = -1, bs = "cr")
  + s(site_depth, fx = FALSE, k = -1, bs = "cr")
)

## 8.10 refit----
m8.4_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_8,
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge,
  method = "REML" # 
)
summary(m8.4_full$gam) # 

## 8.11 tweak model----
# add weights. did not converge
m8.4a_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_8,
  weights = vf1, # ~lake_id
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200), # required to converge,
  method = "REML" # 
)

# try with vf2 weights
# converged.
m8.4b_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)
summary(m8.4b_full$gam) # 

# can we add spatial correlation too?
# holy crap this converged!
m8.4c_full <- mgcv::gamm(
  sa_e_i_log_total_formula,
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)
summary(m8.4c_full$gam) # l_precipitation_chamber 0.98

## 8.12 start variable selection----
# based on summary (on the REML model) l_precipitation_chamber
# is least significant. Remove and refit.
m8.5_drop1 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, .~.
         - l_precipitation_chamber),
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)

summary(m8.5_drop1$gam) # l_toc_shallow 0.34

## 8.13----
# based on summary (on the REML model) l_toc_shallow
# is least significant. Remove and refit.
m8.5_drop2 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, .~.
         - l_precipitation_chamber
         - l_toc_shallow),
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)
summary(m8.5_drop2$gam) # delta_bp_ebu 0.59

## 8.14----
# based on summary (on the REML model) delta_bp_ebu
# is least significant. Remove and refit.
# did not converge. remove next least significant too: l_nh4_shallow 0.54
m8.5_drop3 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, .~.
         - l_precipitation_chamber
         - l_toc_shallow
         - delta_bp_ebu
         - l_nh4_shallow),
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)
summary(m8.5_drop3$gam) # s(l_precipitation_trap)

## 8.15----
# based on summary (on the REML model) s(l_precipitation_trap)
# is least significant. Remove and refit.
m8.5_drop4 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, .~.
         - l_precipitation_chamber
         - l_toc_shallow
         - delta_bp_ebu
         - s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)
summary(m8.5_drop4$gam) # l_nh4_shallow 0.75


## 8.16----
# based on summary (on the REML model) s(l_precipitation_trap)
# is least significant. Remove and refit.
# did not converge. Remove next least significant predictor too.s(l_tp_shallow)
m8.5_drop5 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, .~.
         - l_precipitation_chamber
         - l_toc_shallow
         - delta_bp_ebu
         - s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr")
         - l_nh4_shallow
         -  s(l_tp_shallow, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)
summary(m8.5_drop5$gam) # s(tn_shallow)           


## 8.17----
# based on summary (on the REML model)  s(tn_shallow)          
# is least significant. Remove and refit.
m8.5_drop6 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, .~.
         - l_precipitation_chamber
         - l_toc_shallow
         - delta_bp_ebu
         - s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr")
         - l_nh4_shallow
         - s(l_tp_shallow, fx = FALSE, k = -1, bs = "cr")
         - s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)
summary(m8.5_drop6$gam) # wind_speed_trap 0.28


## 8.18----
# based on summary (on the REML model)  wind_speed_trap         
# is least significant. Remove and refit.
m8.5_drop7 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, .~.
         - l_precipitation_chamber
         - l_toc_shallow
         - delta_bp_ebu
         - s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr")
         - l_nh4_shallow
         - s(l_tp_shallow, fx = FALSE, k = -1, bs = "cr")
         - s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
         - wind_speed_trap
  ),
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)
summary(m8.5_drop7$gam) # s(circularity) 0.34


## 8.19----
# based on summary (on the REML model) s(circularity)       
# is least significant. Remove and refit.
m8.5_drop8 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, .~.
         - l_precipitation_chamber
         - l_toc_shallow
         - delta_bp_ebu
         - s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr")
         - l_nh4_shallow
         - s(l_tp_shallow, fx = FALSE, k = -1, bs = "cr")
         - s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
         - wind_speed_trap
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)
summary(m8.5_drop8$gam) #  l_wind_speed_chamber 0.26


## 8.20----
# based on summary (on the REML model) l_wind_speed_chamber        
# is least significant. Remove and refit.
m8.5_drop9 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, .~.
         - l_precipitation_chamber
         - l_toc_shallow
         - delta_bp_ebu
         - s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr")
         - l_nh4_shallow
         - s(l_tp_shallow, fx = FALSE, k = -1, bs = "cr")
         - s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
         - wind_speed_trap
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - l_wind_speed_chamber
  ),
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)
summary(m8.5_drop9$gam) # chla_lab_shallow 0.25


## 8.21----
# based on summary (on the REML model) chla_lab_shallow  
# is least significant. Remove and refit.
m8.5_drop10 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, .~.
         - l_precipitation_chamber
         - l_toc_shallow
         - delta_bp_ebu
         - s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr")
         - l_nh4_shallow
         - s(l_tp_shallow, fx = FALSE, k = -1, bs = "cr")
         - s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
         - wind_speed_trap
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - l_wind_speed_chamber
         - chla_lab_shallow 
  ),
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)
summary(m8.5_drop10$gam) # mean_depth 0.198

## 8.22----
# based on summary (on the REML model) mean_depth
# is least significant. Remove and refit.
m8.5_drop11 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, .~.
         - l_precipitation_chamber
         - l_toc_shallow
         - delta_bp_ebu
         - s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr")
         - l_nh4_shallow
         - s(l_tp_shallow, fx = FALSE, k = -1, bs = "cr")
         - s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
         - wind_speed_trap
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - l_wind_speed_chamber
         - chla_lab_shallow 
         - mean_depth
  ),
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)

summary(m8.5_drop11$gam) # s(year_completed) 0.156


## 8.23----
# based on summary (on the REML model) s(year_completed)
# is least significant. Remove and refit.
m8.5_drop12 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, .~.
         - l_precipitation_chamber
         - l_toc_shallow
         - delta_bp_ebu
         - s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr")
         - l_nh4_shallow
         - s(l_tp_shallow, fx = FALSE, k = -1, bs = "cr")
         - s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
         - wind_speed_trap
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - l_wind_speed_chamber
         - chla_lab_shallow 
         - mean_depth
         - s(year_completed, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)

summary(m8.5_drop12$gam) # l_buoyf 0.12


## 8.24----
# based on summary (on the REML model) l_buoyf
# is least significant. Remove and refit.
m8.5_drop13 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, .~.
         - l_precipitation_chamber
         - l_toc_shallow
         - delta_bp_ebu
         - s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr")
         - l_nh4_shallow
         - s(l_tp_shallow, fx = FALSE, k = -1, bs = "cr")
         - s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
         - wind_speed_trap
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - l_wind_speed_chamber
         - chla_lab_shallow 
         - mean_depth
         - s(year_completed, fx = FALSE, k = -1, bs = "cr")
         - l_buoyf
  ),
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)

summary(m8.5_drop13$gam) # s(l_so4_shallow) p=0.17


## 8.25----
# based on summary (on the REML model) s(l_so4_shallow) 
# is least significant. Remove and refit.
m8.5_drop14 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, .~.
         - l_precipitation_chamber
         - l_toc_shallow
         - delta_bp_ebu
         - s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr")
         - l_nh4_shallow
         - s(l_tp_shallow, fx = FALSE, k = -1, bs = "cr")
         - s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
         - wind_speed_trap
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - l_wind_speed_chamber
         - chla_lab_shallow 
         - mean_depth
         - s(year_completed, fx = FALSE, k = -1, bs = "cr")
         - l_buoyf
         - s(l_so4_shallow, fx = FALSE, k = -1, bs = "cr")
  ),
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)

summary(m8.5_drop14$gam) # l_op_shallow 0.07 



## 8.25----
# based on summary (on the REML model) l_op_shallow
# is least significant. Remove and refit.
m8.5_drop15 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, .~.
         - l_precipitation_chamber
         - l_toc_shallow
         - delta_bp_ebu
         - s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr")
         - l_nh4_shallow
         - s(l_tp_shallow, fx = FALSE, k = -1, bs = "cr")
         - s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
         - wind_speed_trap
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - l_wind_speed_chamber
         - chla_lab_shallow 
         - mean_depth
         - s(year_completed, fx = FALSE, k = -1, bs = "cr")
         - l_buoyf
         - s(l_so4_shallow, fx = FALSE, k = -1, bs = "cr")
         - l_op_shallow
  ),
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)

summary(m8.5_drop15$gam) # s(temp_shallow) 0.067


## 8.26----
# based on summary (on the REML model) s(temp_shallow) 
# is least significant. Remove and refit.
m8.5_drop16 <- mgcv::gamm(
  update(sa_e_i_log_total_formula, .~.
         - l_precipitation_chamber
         - l_toc_shallow
         - delta_bp_ebu
         - s(l_precipitation_trap, fx = FALSE, k = -1, bs = "cr")
         - l_nh4_shallow
         - s(l_tp_shallow, fx = FALSE, k = -1, bs = "cr")
         - s(tn_shallow, fx = FALSE, k = -1, bs = "cr")
         - wind_speed_trap
         - s(circularity, fx = FALSE, k = -1, bs = "cr")
         - l_wind_speed_chamber
         - chla_lab_shallow 
         - mean_depth
         - s(year_completed, fx = FALSE, k = -1, bs = "cr")
         - l_buoyf
         - s(l_so4_shallow, fx = FALSE, k = -1, bs = "cr")
         - l_op_shallow
         - s(temp_shallow, fx = FALSE, k = -1, bs = "cr") 
  ),
  data = dat_8,
  weights = vf2, # ~ecoregion
  random = list(lake_id =~1),
  correlation = corGaus(form = ~long + lat|visit),
  control = lmeControl(niterEM = 5200, msMaxIter = 5200),
  method = "REML" # 
)

summary(m8.5_drop16$gam) # all significant
anova(m8.5_drop16$gam) # all significant

m8.final <- m8.5_drop16

## 8.26 visualize results----
plot(m8.final$gam)

# partial dependence plots 
# library(gratia)
# gratia::draw for smoothers only
png("reports/log_ch4_tot_partial_dependence_plots_smooths_with_2016.png")
gratia::draw(m8.final$gam, all_terms = TRUE) # only get smoothed terms
dev.off()

# Get data for plotting the linear effect of temp_shallow
df_plot_sa <- data_slice(m8.final$gam,
                         l_surface_area = evenly(l_surface_area, n = 100))

df_plot_eco <- data_slice(m8.final$gam,
                          ag_eco9_nm = evenly(ag_eco9_nm))


fv_sa <- fitted_values(m8.final$gam,
                       data = df_plot_sa,
                       scale = "linear predictor") %>%
  select(l_surface_area, .fitted, .lower_ci, .upper_ci) %>%
  mutate(name = "l_surface_area") %>%
  rename(value = l_surface_area)

fv_eco <- fitted_values(m8.final$gam,
                        data = df_plot_eco,
                        scale = "linear predictor") %>%
  select(ag_eco9_nm, .fitted, .lower_ci, .upper_ci) 


# Plot using ggplot2
p1 <- ggplot(fv_sa,
             aes(x = value, y = .fitted)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.2)

p2 <- ggplot(fv_eco) +
  geom_bar(aes(ag_eco9_nm, .fitted), stat = "identity") +
  geom_errorbar(aes(x=ag_eco9_nm, ymin=.lower_ci, ymax=.upper_ci),
                width = 0.4) +
  xlab(NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

library(gridExtra)
p1_p2 <- grid.arrange(p1, p2, nrow =1)

ggsave("reports/log_ch4_tot_partial_dependence_plots_linear_with_2016.tiff", 
       plot = p1_p2)


return(gam_object)
}
























