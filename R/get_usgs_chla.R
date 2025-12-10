# Spaulding, S. A., L. R. C. Platt, J. C. Murphy, A. Covert, and J. W. Harvey. 
# 2024. Chlorophyll a in lakes and streams of the United States (2005–2022). 
# Scientific Data 11: 611, doi:10.1038/s41597-024-03453-3

# Data were compiled from the Water Quality Portal (WQP) and previously 
# unpublished U.S. Geological Survey’s National Water Quality Laboratory 
# (NWQL) data. 

# OBJECT CONTAINING LIST OF USGS STATIONS IN LAKES WITH MISSING DATA IS
# COMPILED VIA A CALL TO THE dataRetrieval API (SEE get_usgs_lakes.R).
# RATHER THAN REPEATEDLY HITTING THE API, THE DATA ARE WRITTEN TO DISK,
# THEN THE STATIC FILE IS READ IN.

get_usgs_chla <- function(usgs_chla_file, usgs_stations){
  
  # GET AND FORMAT USGS CHLA FOR THE STATIONS----
  usgs_chla <- read_csv(
    unzip(usgs_chla_file,
          "national_chlorophyll_corrected_chlorophyll_a_data.csv"
          )
    ) %>% # read all USGS chla
    select(site_no, date, parameter, result_va, result_units, sample_depth) %>%
    # join with list of USGS stations in SuRGE lakes with missing data
    # right_join will only retain chla records for sites in our lakes
    right_join( # will naturally join on site_no
      usgs_stations %>% # from get_usgs_stations. See targets pipeline
        st_drop_geometry %>%
        mutate(
          site_no = paste(agency_cd, site_no, sep = "-")
        ) %>%
        select(
          site_no, lake_id
        )
    ) %>% # close right_join
    # contains "chlorophyll" and "chlorophyll a"
    filter(parameter == "Chlorophyll a") %>%
    mutate(
      parameter = "chla_lab_shallow",
      result_units = "ug_l"
    ) %>%
    filter(sample_depth < 1.22) %>% # very few >1m deep
    group_by(lake_id, date, parameter) %>%
    summarize(
      value = mean(result_va),
      units = unique(result_units)
    ) %>%
    ungroup %>%
    pivot_wider(
      names_from = parameter,
      values_from = c(value, units),
      names_glue = "usgs_{parameter}_{.value}"
    ) %>%
    rename_with(~ str_remove(., "_value")) %>%
    rename(usgs_chla_lab_sample_date = date)


  return(usgs_chla)
  }