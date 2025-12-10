# RETRIEVE WATER CHEMISTRY DATA FOR MISSING SURGE LAKES
# FROM USGS NWIS AND WQP PORTALS. PACKAGES TEMPORARILY
# INSTALLED, DATA DOWNLOADED AND SAVED LOCALLY. CODE
# IS NOT INTENDED TO BE RERUN TO AVOID PROBLEM WITH
# POTENTIAL CHANGES IN API.

get_usgs_chem <- function(usgs_chem_file, usgs_stations) {

# # DF OF USGS STATIONS IN LAKES WITH MISSING DATA
# # IS GENERATED IN get_usgs_stations.R AND LOADED
# # VIA TARGETS
# <- <- 
# 
# GET AND FORMAT USGS WATER CHEMISTRY FOR THE STATIONS----
# usgs_chem <- readWQPqw(
#   siteNumbers = paste(
#     "USGS",
#     usgs_stations$site_no, # see targets pipeline for data provenance
#     sep = "-"
#   ),
#   # https://help.waterdata.usgs.gov/codes-and-parameters/parameters
#   parameterCd = c("00945", # dissolved, filtered, sulfate
#                   "00681" # doc
#                   # various chla. Omit in favor of national database
#                   #c("32207", "32208", "32209", "32211")
#   )
# ) %>%
#   select(
  #     MonitoringLocationIdentifier,
  #     ActivityStartDate,
  #     CharacteristicName,
  #     ResultMeasureValue,
  #     ResultMeasure.MeasureUnitCode
  #   ) %>%
  #   # bring in lake_id from spatial object
  #   left_join(
  #     usgs_stations %>%
  #       mutate(
  #         MonitoringLocationIdentifier = paste(agency_cd, site_no, sep = "-")
  #       ) %>%
  #       select(
  #         MonitoringLocationIdentifier, lake_id
  #       )
  #   ) %>%
  #   janitor::clean_names() %>%
  #   mutate(
  #     analyte = case_when(
  #       characteristic_name == "Sulfate" ~ "so4_shallow",
  #       characteristic_name == "Organic carbon" ~ "doc_shallow",
  #       TRUE ~ characteristic_name),
  #     units = sub(pattern = "/",
  #                 replacement = "_",
  #                 x = result_measure_measure_unit_code)
  #     ) %>%
  #   rename(
  #     usgs_sample_date = activity_start_date,
  #     value = result_measure_value
  #   ) %>%
  #   select(
  #     lake_id, usgs_sample_date, analyte, value, units
  #   ) %>%
  #   # aggregate when there are multiple observations per lake and date
  #   group_by(lake_id, usgs_sample_date, analyte) %>%
  #   summarize(
  #     value = mean(value),
  #             units = unique(units)
  #     ) %>%
  #   ungroup %>%
  #   pivot_wider(
  #     names_from = analyte,
  #     values_from = c(value, units),
  #     names_glue = "usgs_{analyte}_{.value}"
  #   ) %>%
  #   rename_with(~ str_remove(., "_value")) %>%
  #   mutate(usgs_doc_shallow_sample_date = usgs_sample_date) %>%
  #   rename(usgs_so4_shallow_sample_date = usgs_sample_date)
  # 
  # write_csv(usgs_chem, "input_data/usgs_chem.csv")
  
  
  
  # READ FILE PREPARED ABOVE----
  usgs_chem <- read_csv(usgs_chem_file)
  
  return(usgs_chem)
}