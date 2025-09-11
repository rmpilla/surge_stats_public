get_aquamatch_data <- function(aquamatch_file) {
  aquamatch_data <- readr::read_csv(aquamatch_file, guess_max = 5000) %>%
    janitor::clean_names() %>%
    rename(lake_id = lake_match) %>%
    select(
      lake_id, parameter, activity_start_date,
      harmonized_discrete_depth_value,
      harmonized_discrete_depth_unit,
      harmonized_value, harmonized_units
    ) %>%
    # Filter shallow samples. This removes all observations for lakes
    # 68, 208, 234, and 297. 68 has DOC, but we don't need DOC for
    # that lake. All 4 lakes have chla, but observations are old
    # (2012 and earlier) and wouldn't be used for imputing, so filtering
    # them out is OK.
    mutate(sample_depth_cat = case_when(
      harmonized_discrete_depth_value <= 1 ~ "shallow",
      harmonized_discrete_depth_value > 1 ~ "deep",
      TRUE ~ "FLY YOU FOOLS"
    )) %>%
    # only retain shallow data
    filter(sample_depth_cat == "shallow") %>%
    # calculate mean by lake, parameter (doc, chla), and date
    group_by(lake_id, parameter, activity_start_date) %>%
    summarize(
      value = mean(harmonized_value),
      units = unique(harmonized_units)
    ) %>%
    # adopt surge convention for parameter names
    mutate(parameter = case_when(
      parameter == "chlorophyll" ~ "chla",
      TRUE ~ parameter
    )) %>%
    pivot_wider(
      names_from = parameter,
      values_from = c(value, units),
      names_glue = "aquamatch_{parameter}_{.value}_shallow"
    ) %>%
    # dat_raw convention is to have "units" at the end of column names
    # (e.g., chla_shallow_units). So add "units" to end and remove from middle
    rename_with(~ paste0(., "_units"), contains("units")) %>% # add to end
    rename_with(~ str_remove(., "units_")) %>% # remove from middle
    # remove "_value" from column names
    rename_with(~ str_remove(., "_value"))

  return(aquamatch_data)
}