make_table2_object <- function(random_forest_object, dat_trans){

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
# Fractional Bias function
calc_fb <- function(obs, pred) {
  fb <- mean(((pred - obs) / ((pred + obs) / 2))) * 100
  return(fb)
}

table2_dat <- rf_dat %>% 
  group_by(response, model_var_set) %>%
  summarize(
    # smape Formula: 100 * mean( 2 * |A-F| / (|A|+|F|) )
    smape_log = mean(200 * abs(observed_log - predicted_log) / (abs(observed_log) + abs(predicted_log))),
    mae_log = abs(observed_log - predicted_log) %>% mean(.),
    mse_log = (observed_log - predicted_log)^2 %>% mean(.),
    rmse_log = sqrt(mse_log),
    fbias_log = calc_fb(observed_log, predicted_log),
    smape_linear = mean(200 * abs(observed_linear - predicted_linear) / (abs(observed_linear) + abs(predicted_linear))),
    mae_linear = abs(observed_linear - predicted_linear) %>% mean(.),
    mse_linear = (observed_linear - predicted_linear)^2 %>% mean(.),
    rmse_linear = sqrt(mse_linear),
    fbias_linear = calc_fb(observed_linear, predicted_linear),
  ) %>%
  full_join(
    rf_r2_ranger) %>%
  full_join(
    rf_r2_manual
  ) %>%
  select(
    response, model_var_set, nobs,
    rmse_log, mae_log, smape_log,  r2_log = ranger_r2_log, fbias_log,
    rmse_linear, mae_linear, smape_linear, r2_linear = manual_r2_linear, fbias_linear
  ) %>%
  filter(model_var_set != "local_vars_no_nla") %>%
  ungroup()

table2_object <- table2_dat %>%
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
  relocate(placeholder, .after = "fbias_log") %>% 
  # convert to flextable
  flextable(.) %>%
  theme_booktabs() %>%
  merge_v(j = ~response) %>%
  fix_border_issues() %>%
  colformat_double(j = c("rmse_log", "mae_log"), digits = 3) %>%
  colformat_double(j = c("smape_log"), digits = 0) %>%
  colformat_double(j = c("r2_log", "r2_linear"), digits = 2) %>%
  colformat_double(j = c("rmse_linear", "smape_linear"), digits = 0) %>%
  colformat_double(j = c("mae_linear", "fbias_log", "fbias_linear"), digits = 1) %>%
  set_header_labels(
    nobs = "n",
    rmse_log = "RMSE",
    mae_log = "MAE",
    smape_log = "sMAPE (%)",
    fbias_log = "MFB (%)",
    placeholder = "",
    rmse_linear = "RMSE",
    mae_linear = "MAE",
    smape_linear = "sMAPE (%)",
    fbias_linear = "MFB (%)"
  ) %>%
  # center align numeric column titles and data
  align(
    j = 3:14,
    align = "center",
    part = "all"
  ) %>%
  compose(
    part = "header",
    j = "r2_log",
    value = as_paragraph("OOB r", as_sup("2"))
  ) %>%
  compose(
    part = "header",
    j = "r2_linear",
    value = as_paragraph("OOB r", as_sup("2"))
  ) %>%
  autofit() %>% # apply before footnote
  # add footnote to n
  flextable::footnote(
    i = c(1), # rows
    j = 3, # column
    value = as_paragraph(
      "n reflects observation sites nested within reservoirs"
    ),
    ref_symbols = "a",
    part = "header"
  ) %>%
  # add footnote to RMSE
  flextable::footnote(
    i = c(1), # rows
    j = c(4,10), # column
    value = as_paragraph(
      "RMSE: Root Mean Square Error"
    ),
    ref_symbols = "b",
    part = "header"
  ) %>%
  # add footnote to MAE
  flextable::footnote(
    i = c(1), # rows
    j = c(5,11), # column
    value = as_paragraph(
      "MAE: Mean Absolute Error"
    ),
    ref_symbols = "c",
    part = "header"
  ) %>%
  # add footnote to sMAPE
  flextable::footnote(
    i = c(1), # rows
    j = c(6,12), # column
    value = as_paragraph(
      "sMAPE: Symmetric Mean Absolute Percentage Error"
    ),
    ref_symbols = "d",
    part = "header"
  ) %>%
  # add footnote to OOB r2
  flextable::footnote(
    i = c(1), # rows
    j = c(7,13), # column
    value = as_paragraph(
      "OOB r", as_sup("2"), ": coefficient of determination based on out of bag sample"
      ),
    ref_symbols = "e",
    part = "header"
  ) %>%
  # add footnote to MFB
  flextable::footnote(
    i = c(1), # rows
    j = c(8,14), # column
    value = as_paragraph(
      "MFB: Mean Fractional Bias"
    ),
    ref_symbols = "f",
    part = "header"
  ) %>%
  # add headers to group the log and linear scale metrics
  # Don't add grouping variable to first 3 and 8th columns
  add_header_row(values = c("", "log", "", "linear"),
                 colwidths = c(3,5,1,5)) %>%
  align(i = 1, j = 4:8, align = "center", part = "header") %>% # center "log"
  align(i = 1, j = 9:12, align = "center", part = "header") %>% # center "linear"
  # Remove bottom border for the first 3  and 9th columns of the new top header row
  hline(i = 1, j = 1:3, border = fp_border(width = 0), part = "header") %>%
  hline(i = 1, j = 9, border = fp_border(width = 0), part = "header") %>%
  hline(
    i = c(2,4,6),
    part = "body"
  ) %>%
  # remove border line from top of table
  hline_top(
    border = fp_border_default(width = 0), 
    part = "header"
  ) %>%
  fontsize(size = 8, part = "all") %>%
  padding(padding = 0, part = "all") %>% # Set all padding to 0 points
  line_spacing(space = 1, part = "all") %>%   # Set line spacing to single space (1)
  set_table_properties(layout = "fixed") %>%
  flextable::width(
    j = 1:14, # all columns
    width = c(0.62, 0.62, 0.52, # response, predictors, n
              0.52, 0.52, 0.52, 0.52, 0.52, # log
              0.2, # blank
              0.52, 0.52, 0.52, 0.52, 0.52) # linear
    )

return(list(
  table2_object = table2_object,
  table2_dat = table2_dat)
)

}