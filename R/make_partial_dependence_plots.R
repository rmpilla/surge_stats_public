make_partial_dependence_plots <- function(random_forest_object,
                                          dat_trans,
                                          variable_importance,
                                          clean_predictor_names_crosswalk,
                                          models,
                                          explanatory_vars){
  

  library(ranger)
  partial_df_all = c()
  for(m in models){
    #print(m)
    for(i in explanatory_vars){
      #print(i)
      
      var_use = i
      
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
           response_print = factor(case_when(response == "ch4_total" ~ "CH[4]~total",
                                             response == "ch4_diffusion" ~ "CH[4]~diffusion",
                                             response == "ch4_ebullition" ~ "CH[4]~ebullition"),
                                   levels = c("CH[4]~total",
                                              "CH[4]~diffusion",
                                              "CH[4]~ebullition")),
           var_value_SAtrans = case_when(variable == "surface_area" ~ log10(var_value),
                                         TRUE ~ var_value)) %>%
    left_join(clean_predictor_names_crosswalk) %>%
    mutate(variable_clean_units = case_when(units == "" ~ variable_clean,
                                            endsWith(")", x = variable_clean) ~ 
                                              paste0(str_sub(variable_clean, end = -2), "*';'~", units, ")"),
                                            TRUE ~ paste0(variable_clean, "~(", units, ")")))
  
  
  
  partial_dependence_plots <- ggplot() +
    geom_line(data = partial_df_final_all, 
              aes(x = var_value_SAtrans, y = untrans_resp, color = response_print), linewidth = 1) +
    geom_rug(data = dat_trans %>% select(ch4_total, all_of(explanatory_vars)) %>%
               pivot_longer(values_to = "untrans_resp", names_to = "variable",
                            cols = 2:ncol(.)) %>% select(-ch4_total) %>% distinct() %>%
               mutate(use_resp = case_when(variable == "surface_area" ~ log10(untrans_resp),
                                           TRUE ~ untrans_resp)) %>%
               left_join(clean_predictor_names_crosswalk) %>%
               mutate(variable_clean_units = case_when(units == "" ~ variable_clean,
                                                       endsWith(")", x = variable_clean) ~ 
                                                         paste0(str_sub(variable_clean, end = -2), "*';'~", units, ")"),
                                                       TRUE ~ paste0(variable_clean, "~(", units, ")"))),
             aes(x = use_resp), alpha = 0.3) +
    facet_wrap(~ variable_clean_units, scales = "free_x", labeller = label_parsed) +
    scale_y_continuous(trans = "log1p") +
    scale_color_brewer(palette = "Dark2", name = "emission mechanism:", labels = scales::label_parse()) +
    labs(x = "explanatory variable value", 
         y = bquote(atop(mean~predicted~total~CH[4]~emission~rate, (mg~CH[4]~m^-2~d^-1)))) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "top")
  
  
  return(partial_dependence_plots)
  
}
