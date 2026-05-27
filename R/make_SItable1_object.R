make_SItable1_object <- function(random_forest_object,
                                 variable_importance_variable_group,
                                 dat_trans,
                                 table2_object,
                                 variable_importance,
                                 clean_predictor_names_crosswalk){
  
  
  rf_surfacearea_tableSI1 <- table2_object$table2_dat %>%
    mutate(variable_group = "surface area") %>%
    select(response, model_var_set, variable_group, nobs:r2_log) %>%
    
    full_join(
      variable_importance %>% 
        filter(variable_rank <= 3,
               !var_group == "local_vars_no_nla") %>%
        left_join(clean_predictor_names_crosswalk) %>%
        arrange(response, var_group, variable_rank) %>%
        mutate(variable_group = "surface area") %>%
        group_by(response, var_group, variable_group) %>%
        summarize(top_vars = paste(variable_clean, collapse = ",\n")) %>%
        rename(model_var_set = var_group)
    )
  
  
  # GET PREDICTED VS OBSERVED AND ERROR METRICS for other variable groups----
  # get OOB predictions and training data
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
    
    # extract model variance for use in Miller's correction to log -> linear
    # transformation
    resid_var <- random_forest_object[[i]]$splm$obdata$.ranger_resid %>%
      var
    
    rf_dat_i <- tibble(
      predicted_log,
      observed_linear,
      response,
      model_var_set
    ) %>%
      mutate(predicted_linear = exp(predicted_log + (resid_var/2)) - 1,
             observed_log = log1p(observed_linear))
    
    rf_dat <- bind_rows(rf_dat_i, rf_dat)
  }
  
  # now get model OOB r2
  rf_r2_ranger <- c()
  for(i in 1:length(random_forest_object)){
    # extract response variable
    ## \\.: Matches a literal period (.). The backslash is escaped with another backslash for R.
    ## .*: Matches any character (except a newline) zero or more times.
    ## "": Replaces the matched pattern (the first . and everything after) with an empty string. 
    response = sub("\\..*", "", names(random_forest_object)[i])
    
    # extract model_var name
    ## Matches the start (^) followed by the fewest characters (.*?) 
    ## and a literal dot (\\.) and replaces them with an empty string ("")
    model_var_set = sub("^.*?\\.", "", names(random_forest_object)[i])
    
    # extract r2
    ranger_r2_log = random_forest_object[[i]]$ranger$r.squared
    
    rf_r2_i <- tibble(
      response,
      model_var_set,
      ranger_r2_log
    ) 
    rf_r2_ranger <- bind_rows(rf_r2_ranger, rf_r2_i)
  }
  # now calculate OOB r2 for linear and log
  # log should be same as extracted from ranger
  rf_r2_manual <- 
    rf_dat %>% 
    group_by(response, model_var_set) %>%
    summarize(
      manual_r2_log = glance(lm(predicted_log ~ observed_log))$r.squared,
      manual_r2_linear = glance(lm(predicted_linear ~ observed_linear))$r.squared,
      nobs = glance(lm(predicted_log ~ observed_log))$nobs  
    ) %>%
    ungroup()
  
  
  
  
  # Calculate and merge performance metrics
  tableSI1_dat <- rf_dat %>% 
    group_by(response, model_var_set) %>%
    summarize(
      # smape Formula: 100 * mean( 2 * |A-F| / (|A|+|F|) )
      smape_log = mean(200 * abs(observed_log - predicted_log) / (abs(observed_log) + abs(predicted_log))),
      mae_log = abs(observed_log - predicted_log) %>% mean(.),
      mse_log = (observed_log - predicted_log)^2 %>% mean(.),
      rmse_log = sqrt(mse_log),
      # smape_linear = mean(200 * abs(observed_linear - predicted_linear) / (abs(observed_linear) + abs(predicted_linear))),
      # mae_linear = abs(observed_linear - predicted_linear) %>% mean(.),
      # mse_linear = (observed_linear - predicted_linear)^2 %>% mean(.),
      # rmse_linear = sqrt(mse_linear)
    ) %>%
    full_join(
      rf_r2_ranger) %>%
    full_join(
      rf_r2_manual
    ) %>%
    
    full_join(
      variable_importance_variable_group  %>% 
        filter(variable_rank <= 3,
               !var_group == "local_vars_no_nla") %>%
        left_join(clean_predictor_names_crosswalk %>%
                    full_join(data.frame(variable = "shoreline_development",
                                         variable_clean = "shoreline~development"))) %>%
        arrange(response, var_group, variable_rank) %>%
        group_by(response, var_group) %>%
        summarize(top_vars = paste(variable_clean, collapse = ",\n")) %>%
        rename(model_var_set = var_group)
    ) %>%
    
    separate(col = model_var_set, into = c("model_var_set", "variable_group") ,sep = "__") %>%
    select(
      response, model_var_set, variable_group, nobs,
      rmse_log, mae_log, smape_log,  r2_log = ranger_r2_log, top_vars
      #rmse_linear, mae_linear, smape_linear, r2_linear = manual_r2_linear
      
    ) %>%
    filter(model_var_set != "local_vars_no_nla") %>%
    ungroup() %>%
    full_join(rf_surfacearea_tableSI1) %>%
    mutate(variable_group = str_replace(variable_group, pattern = "_", replacement =  " "),
           top_vars = str_replace_all(top_vars, pattern = "~", replacement = " ")) %>%
    arrange(response, model_var_set, desc(variable_group))
  
  
  
  
  tableSI1_object <- tableSI1_dat %>%
    mutate(
      placeholder = "", # add empty column to be between log and linear metrics
      model_var_set = case_when(
        model_var_set == "global_vars" ~ "national",
        model_var_set == "local_vars_all" ~ "local",
        TRUE ~ "FLY YOU FOOLS!"
      ),
      response = case_when(
        response == "ch4_diffusion" ~ "diffusion",
        response == "ch4_ebullition" ~ "ebullition",
        response == "ch4_total" ~ "total",
        TRUE ~ "FLY YOU FOOLS!"
      )
    ) %>%
    rename(predictors = model_var_set) %>%
    # move empty column between log and linear metrics
    relocate(placeholder, .after = "r2_log") %>% 
    # convert to flextable
    flextable(.) %>%
    theme_booktabs() %>%
    fontsize(size = 8, part = "all") %>%
    padding(padding = 0, part = "all") %>% # Set all padding to 0 points
    line_spacing(space = 1, part = "all") %>%   # Set line spacing to single space (1)
    merge_v(j = ~response) %>%
    merge_v(j = ~predictors) %>%
    fix_border_issues() %>%
    colformat_double(j = c("rmse_log", "mae_log"), digits = 3) %>%
    colformat_double(j = c("smape_log"), digits = 0) %>%
    colformat_double(j = c("r2_log"), digits = 2) %>%
    #colformat_double(j = c("rmse_linear", "smape_linear"), digits = 0) %>%
    #colformat_double(j = c("mae_linear"), digits = 1) %>%
    set_header_labels(
      variable_group = "variable group",
      nobs = "n",
      rmse_log = "RMSE",
      mae_log = "MAE",
      smape_log = "sMAPE",
      placeholder = "",
      top_vars = "top 3 predictor variables"
      #rmse_linear = "RMSE",
      #mae_linear = "MAE",
      #smape_linear = "sMAPE"
    ) %>%
    # center align numeric column titles and data
    align(
      j = 4:8,
      align = "center",
      part = "all"
    ) %>%
    compose(
      part = "header",
      j = "r2_log",
      value = as_paragraph("OOB r", as_sup("2"))
    ) %>%
    # compose(
    #   part = "header",
    #   j = "r2_linear",
    #   value = as_paragraph("OOB r", as_sup("2"))
    # ) %>%
    # add headers to group the log and linear scale metrics
    # Don't add grouping variable to first 3 and 8th columns
    # add_header_row(values = c("", "log", "", "linear"),
    #                colwidths = c(3,4,1,4)) %>%
    align(i = 1, j = 4:8, align = "center", part = "header") %>% # center "log"
    #align(i = 1, j = 9:12, align = "center", part = "header") %>% # center "linear"
    # Remove bottom border for the first 3  and 8th columns of the new top header row
    #hline(i = 1, j = 1:3, border = fp_border(width = 0), part = "header") %>%
    #hline(i = 1, j = 8, border = fp_border(width = 0), part = "header") %>%
    hline(
      i = 1:18,#c(3, 6, 9, 12, 15, 18),
      part = "body"
    ) %>%
    # remove border line from top of table
    hline_top(
      border = fp_border_default(width = 0), 
      part = "header"
    ) %>%
    autofit()
  
  return(list(
    tableSI1_object = tableSI1_object,
    tableSI1_dat = tableSI1_dat)
  )
  
}
