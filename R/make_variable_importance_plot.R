make_variable_importance_plot <- function(variable_importance,
                                          clean_predictor_names_crosswalk,
                                          var_group_use){

  var_imp_subset <- variable_importance %>% 
    filter(var_group == var_group_use) %>%
    left_join(clean_predictor_names_crosswalk) %>%
    mutate(variable_clean_superscript = fct_reorder(paste0(variable_clean, superscript), 
                                                    as.numeric(variable_ordered)))
  
  variable_importance_plot <- ggplot() +
    geom_hline(yintercept = seq(2, length(unique(var_imp_subset$variable)), by = 2), linetype = 2) +
    geom_hline(yintercept = seq(1, length(unique(var_imp_subset$variable)), by = 2), color = "grey80") +
    geom_point(data = var_imp_subset, 
               aes(x = relative_importance, y = variable_clean_superscript, color = response_print), 
               size = 3) +
    facet_wrap(~ response_print, nrow = 1, labeller = label_parsed) +
    scale_y_discrete(limits = rev, labels = scales::label_parse()) +
    scale_color_brewer(palette = "Dark2", guide = "none") +
    labs(x = "relative importance", y = NULL) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "none")
  
  return(variable_importance_plot)
  
}