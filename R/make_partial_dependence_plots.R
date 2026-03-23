make_partial_dependence_plots <- function(random_forest_object,
                                          dat_trans,
                                          variable_importance){

  models = c("ch4_total.local_vars_all",
             "ch4_diffusion.local_vars_all",
             "ch4_ebullition.local_vars_all"
             #"ch4_total.global_vars",
             #"ch4_diffusion.global_vars",
             #"ch4_ebullition.global_vars"
             )
  
  library(ranger)
  partial_df_all = c()
  for(m in models){
    #print(m)
    for(i in 1:3){
      #print(i)
      
      var_use = as.character(variable_importance %>%
                               filter(model == m) %>%
                               arrange(desc(relative_importance)) %>%
                               select(variable) %>%
                               slice(i))
      
      partial_df = pdp::partial(object = random_forest_object[[m]]$ranger, 
                                pred.var = var_use, train = dat_trans, type = "regression")
      
      partial_df_all = rbind(partial_df_all,
                             partial_df %>%
                               rename(var_value = 1) %>%
                               mutate("untrans_bin_mean" = 10^var_value,
                                      "untrans_resp" = expm1(yhat),
                                      "variable" = var_use,
                                      "model" = m,
                                      "varimp_rank" = i))
    }
  }
  
  
  partial_df_final_all = partial_df_all %>%
    separate_wider_delim(cols = model, delim = ".", names = c("response", "var_group"), cols_remove = F) %>%
    mutate(use_untrans_bin_mean = case_when(grepl(pattern = "^l_", variable) ~ untrans_bin_mean,
                                            variable == "e_i_type" ~ untrans_bin_mean,
                                            TRUE ~ yhat),
           response = factor(response, levels = c("ch4_total", "ch4_diffusion", "ch4_ebullition")),
           response_print = case_when(response == "ch4_total" ~ "CH[4]~total",
                                             response == "ch4_diffusion" ~ "CH[4]~diffusion",
                                             response == "ch4_ebullition" ~ "CH[4]~ebullition")) %>%
    filter(!(response == "ch4_ebullition" & variable == "site_depth"))
  
  
  partial_dependence_plots <- ggplot() +
    geom_line(data = partial_df_final_all, 
              aes(x = var_value, y = untrans_resp, color = response_print), linewidth = 1) +
    geom_rug(data = dat_trans %>% select(ch4_total,
                                         all_of(partial_df_final_all %>%
                                                  select(variable) %>%
                                                  distinct() %>%
                                                  as.matrix() %>%
                                                  as.vector())) %>%
               pivot_longer(values_to = "untrans_resp", names_to = "variable",
                            cols = 2:ncol(.)) %>% select(-ch4_total) %>% distinct(),
             aes(x = untrans_resp), alpha = 0.3) +
    facet_wrap(~ variable, scales = "free_x") +
    scale_y_continuous(trans = "log1p") +
    scale_color_brewer(palette = "Dark2", name = "flux pathway:", labels = scales::label_parse()) +
    labs(x = "explanatory variable value", y = bquote(mean~predicted~CH[4]~flux)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "top")
  
  
  return(partial_dependence_plots)
  
}