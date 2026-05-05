make_predicted_observed_plot_main_text <- function(random_forest_object, dat_trans) {
  
  # get OOB predictions and training data
  # this code gathers data for all response variables and models
  # (global, local_vars_all, local_vars_no_nla)
  rf_dat <- c()
  for(i in 1:length(random_forest_object)){
    # extract OOB predictions from model
    predicted_log = random_forest_object[[i]]$ranger$predictions
    
    # extract response variable
    ## \\.: Matches a literal period (.). The backslash is escaped with another backslash for R.
    ## .*: Matches any character (except a newline) zero or more times.
    ## "": Replaces the matched pattern (the first . and everything after) with an empty string. 
    response = sub("\\..*", "", names(random_forest_object)[i])
    
    # extract model_var name
    ## Matches the start (^) followed by the fewest characters (.*?) 
    ## and a literal dot (\\.) and replaces them with an empty string ("")
    model_var_set = sub("^.*?\\.", "", names(random_forest_object)[i])
    
    # extract predictor variables
    predictor_vars = names(random_forest_object[[i]]$ranger$variable.importance)
    
    # use response and predictor names to extract the training data
    observed_linear = dat_trans %>%
      select(
        all_of(response), all_of(predictor_vars) # vars in model
      ) %>%
      drop_na() %>% # omit any rows with NA
      pull(response)
    
    rf_dat_i <- tibble(
      predicted_log,
      observed_linear,
      response,
      model_var_set
    ) %>%
      mutate(predicted_linear = expm1(predicted_log),
             observed_log = log1p(observed_linear))
    
    rf_dat <- bind_rows(rf_dat_i, rf_dat)
  }
  
  
  # plot predicted vs observed
  # log-scale
  predicted_observed_plot_log <- rf_dat %>%
    filter(
      #observed_log < 20000, # omit Comins lake outlier
      model_var_set == "local_vars_all", # only plot local_all model
      response == "ch4_diffusion" # one response to simplify plot
    ) %>%
    ggplot(aes(observed_log, predicted_log)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    labs(y = expression(atop("predicted diffusive emission rate", paste("1og10(mg C", H[4], " ", m^{-2}, " ", h^{-1}, ")"))),
         x = expression(atop("observed diffusive emission rate", paste("log10(mg C", H[4], " ", m^{-2}, " ", h^{-1}, ")")))) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) 
  
  # filter data down to plot we intend to share
  predicted_observed_plot_linear <- rf_dat %>%
    filter(
      observed_linear < 20000, # omit Comins lake outlier
      model_var_set == "local_vars_all", # only plot local_all model
      response == "ch4_diffusion" # one response to simplify plot
    ) %>%
    ggplot(aes(observed_linear, predicted_linear)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    labs(y = expression(paste("mg C", H[4], " ", m^{-2}, " ", h^{-1})),
         x = expression(atop("observed diffusive emission rate", paste("mg C", H[4], " ", m^{-2}, " ", h^{-1})))) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  
  predicted_observed_plot_linear_limited_range <- rf_dat %>%
    filter(
      observed_linear < 20000, # omit Comins lake outlier
      model_var_set == "local_vars_all", # only plot local_all model
      response == "ch4_diffusion" # one response to simplify plot
    ) %>%
    ggplot(aes(observed_linear, predicted_linear)) +
    geom_point(size = 0.5) +
    geom_abline(slope = 1, intercept = 0) +
    scale_x_continuous(breaks = c(0, 100, 200), limits = c(0, 250)) +
    scale_y_continuous(breaks = c(0,200, 400), limits = c(0, 400)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text = element_text(size = 5))
  
  
  predicted_observed_plot <- predicted_observed_plot_log +  
    predicted_observed_plot_linear +
    inset_element(
      predicted_observed_plot_linear_limited_range, 
      left = 0.35, bottom = 0.45, right = 0.98, top = 0.98,
      # make sure inset panel is ignored with adding panel
      # labels via plot_annotation(tag_levels = "A") when
      # the combined plot is created
      ignore_tag = TRUE 
    ) +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag.position = c(0.08, 1)) #c(0.18, 1)
  
  
    
  
  return(predicted_observed_plot)
}