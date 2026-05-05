get_table2_dat <- function(random_forest_object, dat_trans){

# GET PREDICTED VS OBSERVED AND ERROR METRICS----
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
table2_dat <- rf_dat %>% 
  group_by(response, model_var_set) %>%
  summarize(
    mae_log = abs(observed_log - predicted_log) %>% mean(.),
    mse_log = (observed_log - predicted_log)^2 %>% mean(.),
    rmse_log = sqrt(mse_log),
    mae_linear = (observed_linear - predicted_linear) %>% mean(.),
    mse_linear = (observed_linear - predicted_linear)^2 %>% mean(.),
    rmse_linear = sqrt(mse_linear)
  ) %>%
  full_join(
    rf_r2_ranger) %>%
  full_join(
    rf_r2_manual
  ) %>%
  select(
    response, model_var_set,
    mse_log, rmse_log, mae_log, r2_log = ranger_r2_log,
    mse_linear, rmse_linear, mae_linear, r2_linear = manual_r2_linear,
    nobs
  ) %>%
  filter(model_var_set != "local_vars_no_nla") %>%
  ungroup()

return(table2_dat)

}