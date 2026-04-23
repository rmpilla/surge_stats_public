get_interaction_results <- function(random_forest_object,
                                    dat_trans){
  
  
  int_var_combos = data.frame("model_name" = c(#"ch4_total.local_vars_all",
    "ch4_total.local_vars_all")
    #"ch4_total.local_vars_all")
    ,
    
    "int_var_1" = c(#"nla17_sed_c",
      "nla17_sed_c")
    #"mean_depth"
    ,
    
    "int_var_2" = c(#"littoral_fraction",
      "mean_depth")
    #"littoral_fraction"
  )
  

  partial_interaction_all = c()
  for(i in 1:nrow(int_var_combos)){
    pdp_data = pdp::partial(object = #random_forest_object_rangerOnly[[int_var_combos$model_name[i]]],
                              random_forest_object[[int_var_combos$model_name[i]]]$ranger,
                            pred.var = c(int_var_combos$int_var_1[i], int_var_combos$int_var_2[i]),
                            type = "regression", train = dat_trans, grid.resolution = 50)
    
    partial_interaction_spatial = pdp_data %>%
      rename(var1 = 1,
             var2 = 2) %>%
      mutate(var1_name = int_var_combos$int_var_1[i],
             var2_name = int_var_combos$int_var_2[i],
             model_name = int_var_combos$model_name[i],
             pair_num = i)
    
    partial_interaction_all = rbind(partial_interaction_all,
                                    partial_interaction_spatial)
  }
  
  
  partial_interaction_spatial_df_g1 = partial_interaction_all %>%
    as.data.frame() %>%
    group_by(pair_num, var1_name, var2_name, model_name) %>%
    mutate(pair = paste(var1_name, var2_name, sep = "__")) %>%
    mutate(group = as.numeric(factor(var1))) %>%
    select(pair, pair_num, var1_name, var2_name, model_name, yhat, group, var1, var2) %>%
    ungroup()
  
  
  
  partial_interaction_spatial_df_g2 = partial_interaction_all %>%
    as.data.frame() %>%
    group_by(pair_num, var1_name, var2_name, model_name) %>%
    mutate(pair = paste(var1_name, var2_name, sep = "__")) %>%
    mutate(group = as.numeric(factor(var2))) %>%
    select(pair, pair_num, var1_name, var2_name, model_name, yhat, group, var1, var2) %>%
    ungroup()
  
  
  return(list(partial_interaction_spatial_df_g1, 
              partial_interaction_spatial_df_g2))
}