# DATA FROM OHIO DIVISION OF WILDLIFE
# SEE EMAIL IN input_data/external_data/ohio_div_wildlife/ for provenance


get_ohio_wildlife_chem <- function(ohio_wildlife_chem_file = ohio_wildlife_chem_file){
  
  ohio_wildlife_chem <- read_csv(ohio_wildlife_chem_file) %>%
    clean_names() %>%
    mutate(
      lake_id = case_when(location == "COWAN LAKE" ~ 1012,
                               location == "KISER LAKE" ~ 1016,
                               location == "KNOX LAKE" ~ 1017,
                               location == "LADUE RESERVOIR" ~ 1018,
                               location == "LAKE LOGAN" ~ 1015,
                               location == "LAKE MILTON" ~ 1020,
                               TRUE ~ 999999999),
           analyte = case_when(measure_type_desc == "Chlorophyll" ~ "chla_shallow",
                               grepl("Sulfate", measure_type_desc) ~ "so4_shallow",
                               TRUE ~ "FLY YOU FOOLS"),
           # extract all characters before white space in trip_date
           ohio_wildlife_sample_date = as.Date(str_extract(trip_date, "^[^ ]+"), format = "%Y/%m/%d"),
           # replace "/" with"_" in measure unit
           units = gsub("/", "_", measure_unit)
    ) %>%
    select(
      lake_id,
      ohio_wildlife_sample_date,
      analyte,
      value = measure_value,
      units
    ) %>%
    # Aggregate duplicate observations by lake, date, and analyte
    group_by(lake_id, ohio_wildlife_sample_date, analyte) %>%
    summarise(value = mean(value),
              units = unique(units)) %>%
    ungroup %>%
    pivot_wider(
      names_from = analyte,
      values_from = c(value, units),
      names_glue = "ohio_wildlife_{analyte}_{.value}"
    ) %>%
    rename_with(~ str_remove(., "_value")) %>%
    mutate(ohio_wildlife_chla_shallow_sample_date = ohio_wildlife_sample_date) %>%
    rename(ohio_wildlife_so4_shallow_sample_date = ohio_wildlife_sample_date) %>%
    relocate(lake_id, contains("so4"), contains("chla"))

    
  
  
  
  return(ohio_wildlife_chem)
  
}