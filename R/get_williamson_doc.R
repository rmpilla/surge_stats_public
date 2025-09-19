# DOC EXTRACTED FROM

# Williamson, C.E. 2023. Limnological data from nearly 400 lakes across the 
# Americas and New Zealand with a focus on vertical profiles of temperature, 
# UV radiation, and optical properties ver 5. Environmental Data Initiative. 
# https://doi.org/10.6073/pasta/40bb7dc870814ae90437875c506527bf 
# (Accessed 2025-09-17).

# FILTERED TO SURGE LAKES WITH MISSING DATA BY PILLA
# FILE SHARED VIA EMAIL (missing_surge_from_MiamiOH.xlsx) on 9/15/2025
# AND RENAMED Williamson_2023_doc.csv

get_williamson_doc <- function(williamson_doc_file){
  
  williamson_doc <- read_csv(williamson_doc_file) %>%
    clean_names() %>%
    mutate(date = as.Date(date, format = "%m/%d/%Y"),
           year = format(date, "%Y"),
           month = format(date, "%m")) %>%
    # Acton has a ton of date 2006 - 2020
    # mostly older data (2006-2013) for other lakes
    # {table(.$site_name, .$year)}
    filter(
      # only keep 5/31/2016 observation from Acton
      !(site_name == "Acton_Outflow" & date != as.Date("2016-05-31")),
      # only keep lower_depth == 0 for Acton
      !(site_name == "Acton_Outflow" & lower_depth != 0),
      # only keep May - Sept observations
      month %in% c("05", "06", "07", "08", "09")
    ) %>%
    mutate(
      lake_id = case_when(
        site_name == "Acton_Outflow" ~ 1001,
        site_name == "Alum Creek_Outflow" ~ 1002,
        site_name == "Burr Oak_Outflow" ~ 1007,
        site_name == "Delaware_Outflow" ~ 1013,
        site_name == "Dillon_Outflow" ~ 1014,
        site_name == "Knox" ~ 1017,
        site_name == "Piedmont_Outflow" ~ 1026,
        site_name == "Pleasant Hill_Outflow" ~ 1027,
        TRUE ~ 999999999999
      )
    ) %>%
    #{table(.$lake_id, .$year)} # no 999999, good
    select(lake_id, 
           williamson_doc_shallow_sample_date = date,
           variable, value, unit) %>% 
    mutate(unit = "mg_l") %>%
    pivot_wider(
      names_from = variable,
      values_from = value
    ) %>%
    rename(
      williamson_doc_shallow = doc,
      williamson_doc_shallow_unit = unit 
    )
  
  return(williamson_doc)
}