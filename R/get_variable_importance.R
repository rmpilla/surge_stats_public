get_variable_importance <- function(random_forest_object){
  
  rf_varimp <- c()
  for(i in 1:length(random_forest_object)){
    var_imp = random_forest_object[[i]]$ranger$variable.importance %>%
      as.data.frame() %>%
      rename(importance = 1) %>%
      mutate(model = names(random_forest_object)[i],
             variable = rownames(.),
             relative_importance = importance / max(importance),
             variable_ordered = factor(variable, levels = variable[order(relative_importance, decreasing = T)]),
             variable_rank = as.numeric(variable_ordered)) %>%
      separate_wider_delim(cols = model, delim = ".", names = c("response", "var_group"), cols_remove = F)
    
    rf_varimp <- rbind(rf_varimp, var_imp)
  }
  
  rf_varimp$response_print = factor(case_when(rf_varimp$response == "ch4_total" ~ "CH[4]~total",
                                              rf_varimp$response == "ch4_diffusion" ~ "CH[4]~diffusion",
                                              rf_varimp$response == "ch4_ebullition" ~ "CH[4]~ebullition"),
                                    levels = c("CH[4]~total",
                                               "CH[4]~diffusion",
                                               "CH[4]~ebullition"))
  
  return(rf_varimp)
}