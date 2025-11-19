# TEST
# FIT SPATIAL NESTED RANDOM FOREST FROM FINAL MODELING VARS

library(spmodel)

fit_sprf_test <- function(dat_trans,
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
           -doc_predicted_sample_month_visit1, -doc_predicted_sample_month_visit2,
           -visit)
  
  
  ## will want to set seed if use random forest for reproducibility

  sp_random_forest <- splmRF(log_ch4_diffusion ~ .,
                             data = dat_model,
                             random = ~ (1 | lake_id_crc),
                             xcoord = long, ycoord = lat,
                             spcov_type = "exponential",
                             importance = "impurity")
  
  sp_rf_varImp <- data.frame("Importance" = sp_random_forest$ranger$variable.importance) %>%
    arrange(desc(Importance)) %>%
    mutate("Variable" = factor(row.names(.), levels = row.names(.)),
           "Relative_Importance" = Importance / max(Importance))
  
  ggplot() +
    geom_point(data = sp_rf_varImp, aes(x = Relative_Importance, y = Variable), size = 3, stroke = 1.5) +
    scale_y_discrete(limits = rev) +
    theme_bw()
  
  partial_all = c()
  for(i in 1:6){
    print(i)
    partial_1 = pdp::partial(object = sp_random_forest, 
                             pred.var = as.character(sp_rf_varImp$Variable[i]), 
                             type = "regression") %>%
      as.data.frame() %>%
      rename(x_value = 1) %>%
      mutate(yhat_untrans = 10^(yhat),
             x_var_name = sp_rf_varImp$Variable[i])
    
    partial_all = rbind(partial_all, partial_1)
  }
  
  ggplot() +
    geom_point(data = dat_model %>% select(unique(partial_all$x_var_name)) %>%
                 pivot_longer(values_to = "x_value", names_to = "x_var_name", cols = 1:ncol(.)), 
               aes(x = x_value, y = -Inf), shape = 3, size = 3, alpha = 0.3) +
    geom_line(data = partial_all, aes(x = x_value, y = yhat_untrans, color = x_var_name)) +
    geom_smooth(data = partial_all, aes(x = x_value, y = yhat_untrans, color = x_var_name), 
                linewidth = 1) +
    facet_wrap(~ x_var_name, scales = "free_x") +
    #scale_y_continuous(trans = "log10") +
    theme_bw() +
    theme(legend.position = "none")

  
  return(sp_random_forest)
  
}