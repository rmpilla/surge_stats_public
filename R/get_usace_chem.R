

get_usace_chem <- function(usace_caesar_file,
                           usace_cave_file,
                           usace_brookville_buckhorn_carr_file) {
  # location data----
  # map to filter sites
  # bind_rows(
  #   read_xlsx(usace_caesar_file, sheet = "Location Info") %>%
  #     clean_names() %>%
  #     rename(site_id = loc_id),
  #   read_xlsx(usace_brookville_buckhorn_carr_file,
  #             sheet = "Location Information"
  #   ) %>%
  #     clean_names() %>%
  #     select(-reservoir, -location, -x6, -x7),
  #   read_xlsx(usace_cave_file, sheet = "Location Information ") %>%
  #     clean_names() %>%
  #     select(-reservoir)
  # ) %>%
  #   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  #   leaflet() %>%
  #   addTiles() %>%
  #   addCircleMarkers(
  #     label = ~site_id, # Specify the label
  #     labelOptions = labelOptions(
  #       noHide = TRUE, # Make labels permanently visible
  #       direction = "auto", # Adjust label position automatically
  #       textOnly = TRUE # Display text without a background bubble
  #     )
  #   )
  
  # chem data----
  
  # Brookville, Buckhorn, and Carr first
  usace_chem <- bind_rows(
    read_xlsx(usace_brookville_buckhorn_carr_file, sheet = "Brookville Regular Data"),
    read_xlsx(usace_brookville_buckhorn_carr_file, sheet = "Carr Creek Regular Data"),
    read_xlsx(usace_brookville_buckhorn_carr_file, sheet = "Buckhorn Regular Data")
  ) %>%
    clean_names() %>%
    select(
      site_id = location,
      year, month, day, parameter, depth_ft, value, units
    ) %>%
    mutate(
      lake_id = case_when(
        grepl("BVR", site_id) ~ 1005,
        grepl("CFK", site_id) ~ 1009,
        grepl("BHR", site_id) ~ 1006,
        TRUE ~ 99999999 # error code
      )
    ) %>%
    # exclude sites outside lake polygons. See map above.
    filter(
      !(site_id %in% c("BHR20006", "BHR20063")), # upstream of lake polygon
      site_id != "BVR20018", # upstream of lake polygon
      site_id != "CCK20015" # upstream of lake polygon
    ) %>%
    # bind Caesar Creek Reservoir
    bind_rows(
      read_xlsx(usace_caesar_file, sheet = "Regular Chem Data") %>%
        clean_names() %>%
        select(
          site_id = loc_id,
          parameter, units, value, year, month, day, depth_ft
        ) %>%
        mutate(lake_id = 1008)
    ) %>%
    # bind Cave Run data
    bind_rows(
      read_xlsx(usace_cave_file, sheet = "All Regular Data") %>%
        clean_names() %>%
        select(
          site_id = location,
          parameter, units, value, year, month, day, depth_ft
        ) %>%
        mutate(lake_id = 1010)
    ) %>% # close final bind
    # format bound data
    mutate(
      across(c(value, year, month, day, depth_ft), as.numeric),
      sample_date = as.Date(paste(year, month, day, sep = "-")),
      # replace / with _ in units
      units = sub(pattern = "/",
                  replacement = "_",
                  units)
    ) %>%
    rename(analyte = parameter) %>%
    filter(
      # < 10ft in depth. shallow only
      depth_ft <= 10
    ) %>%
    mutate(
      analyte = case_when(
        analyte == "Chlorophyll a, Corrected" ~ "chla_shallow",
        analyte == "Chlorophyll a, Uncorrected" ~ "chla_shallow", # Cave Run
        analyte == "Carbon, Dissolved Organic" ~ "doc_shallow",
        analyte == "Sulfate, Dissolved" ~ "so4_shallow",
        TRUE ~ analyte
      )
    ) %>% 
    filter(
      # only analytes of interest
      analyte %in% c("chla_shallow",
                       "doc_shallow",
                       "so4_shallow"),
    ) %>%
    select(-year, -month, -day, -depth_ft) %>%
    # # inspect values for outliers, etc
    # ggplot(aes(sample_date, value, color = site_id)) +
    # geom_point() +
    # facet_wrap(~lake_id + analyte, scale = "free")
    filter(
      # remove values based on inspection of plot above
      !is.na(value),
      # exclude all observations prior to 2010 except sulfate
      !(analyte != "so4_shallow" & sample_date < as.Date("2010-01-01")),
      # exclude 2009 sulfate values from 1008
      !(analyte == "so4_shallow" & 
          lake_id == 1008 & 
          sample_date < as.Date("2010-01-01"))
    ) %>%
    # aggregate across depths and sites
    group_by(lake_id, analyte, sample_date) %>%
    summarize(value = mean(value, na.rm = TRUE),
              units = unique(units)) %>%
    ungroup %>%
    pivot_wider(
      names_from = analyte,
      values_from = c(value, units),
      names_glue = "usace_{analyte}_{.value}"
    ) %>%
    rename_with(~ str_remove(., "_value")) %>%
    mutate(usace_doc_shallow_sample_date = sample_date,
           usace_chla_shallow_sample_date = sample_date) %>%
    rename(usace_so4_shallow_sample_date = sample_date) %>%
    relocate(lake_id, contains("so4"), contains("doc"))
  
  
  return(usace_chem)
}