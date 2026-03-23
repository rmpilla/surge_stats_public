make_variable_importance_plot <- function(variable_importance){

  
  variable_importance_plot <- ggplot() +
    geom_hline(yintercept = seq(2, 42, by = 2), linetype = 2) +
    geom_hline(yintercept = seq(1, 42, by = 2), color = "grey80") +
    geom_point(data = variable_importance %>% filter(var_group %in% c("local_vars_all")), 
               aes(x = relative_importance, y = variable_ordered), 
               size = 3) +
    facet_wrap(~ response_print, nrow = 1, labeller = label_parsed) +
    scale_y_discrete(limits = rev) +
    labs(x = "relative importance", y = NULL) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "none")
  
  
  return(variable_importance_plot)
  
}