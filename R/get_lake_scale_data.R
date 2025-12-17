get_lake_scale_data <- function(lake_scale_file) {
  # need to assign ecoregion to Puerto Rico sites
  lake_scale_data <- readr::read_csv(lake_scale_file, guess_max = 5000) %>%
    mutate(
      value = case_when(
        lake_id == 1000 & name == "ag_eco9_nm" ~ "Coastal Plains",
        TRUE ~ value
      ),
      value = case_when(
        lake_id == 1000 & name == "ag_eco9" ~ "CPL",
        TRUE ~ value
      )
    )
  
  return(lake_scale_data)
  
}