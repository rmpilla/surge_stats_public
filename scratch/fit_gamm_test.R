# TEST
# FIT SPATIAL NESTED GAMM FROM FINAL MODELING VARS

library(mgcv)

fit_gamm_test <- function(dat_trans,
                          modeling_vars_final){
  
  dat_model <- dat_trans %>%
    
    mutate(lake_id_crc = as.character(lake_id),
           log_ch4_diffusion = log10(ch4_diffusion)) %>%
    
    select(lake_id_crc,
           lat,
           long,
           visit,
           log_ch4_diffusion,
           as.vector(unlist(modeling_vars_final))) %>%
    
    mutate(chl_predicted_sample_season = case_when(visit == 1 ~ chl_predicted_sample_season_visit1,
                                                   visit == 2 ~ chl_predicted_sample_season_visit2),
           
           chl_predicted_sample_month = case_when(visit == 1 ~ chl_predicted_sample_month_visit1,
                                                  visit == 2 ~ chl_predicted_sample_month_visit2),
           
           doc_predicted_sample_season = case_when(visit == 1 ~ doc_predicted_sample_season_visit1,
                                                   visit == 2 ~ doc_predicted_sample_season_visit2),
           
           doc_predicted_sample_month = case_when(visit == 1 ~ doc_predicted_sample_month_visit1,
                                                  visit == 2 ~ doc_predicted_sample_month_visit2)) %>%
    
    select(-chl_predicted_sample_season_visit1, -chl_predicted_sample_season_visit2,
           -chl_predicted_sample_month_visit1, -chl_predicted_sample_month_visit2,
           -doc_predicted_sample_season_visit1, -doc_predicted_sample_season_visit2,
           -doc_predicted_sample_month_visit1, -doc_predicted_sample_month_visit2)
  
  
  explanatory_formula = paste0(
    paste0("s(", colnames(dat_model %>%
                            select(-lake_id_crc,
                                   -lat,
                                   -long,
                                   -visit,
                                   -log_ch4_diffusion,
                                   
                                   ## has -Inf values, probably from log-transformation
                                   -l_chla_sonde_shallow,
                                   
                                   # both are categorical -- incorporate with additional s() argument below
                                   -ag_eco9_nm,
                                   -e_i_type)), 
           ")", collapse = " + "),
    " + ",
    paste0(colnames(dat_model %>%
                            select(ag_eco9_nm,
                                   e_i_type)), 
           collapse = " + "))
  
  
  full_formula = as.formula(paste("log_ch4_diffusion ~", explanatory_formula))
  
  
  sp_gamm_full <- gamm(full_formula,
                       random = list(lake_id_crc = ~1),
                       correlation = corSpatial(form = ~long + lat, type = "exponential"),
                       data = dat_model)
  
  
  ## check/compare correlation structure type
  ## explanatory variable selection
  
  
  return(sp_gamm_full)
  
}