# READ CHEMISTRY DATA FROM OHIO EPA 

get_ohio_epa_chem <- function(ohio_epa_chem_file,
                              ohio_epa_chem_ladue_file, 
                              ohio_epa_chem_milton_file){
  
# Read raw data----
  
  # Read raw data provided via email
  # Robert.Miltner@epa.ohio.gov 9/22/2025
  ohio_epa_chem <- readRDS(ohio_epa_chem_file) %>%
    as_tibble() 
  
  # Read raw data provided via email
  # 9/22/2025 e-mail from Ruth Briland Ruth.Briland@epa.ohio.gov
  # See issue #14 for details
  ohio_epa_chem_milton <- read_csv(ohio_epa_chem_milton_file) %>%
    clean_names %>%
    filter(
      parameter == "SULFATE, TOTAL",
      station_name %in% c("LAKE MILTON, L-1", "Lake Milton L-2")
      ) %>%
    mutate(lake_id = 1020,
           # replace / with _ in units
           units = sub(pattern = "/",
                       replacement = "_",
                       units),
           # format collection date
           ohio_epa_sample_date = as.Date(
             str_extract(collect_date, "^[^ ]+"), # extract date  
             format = "%m/%d/%Y"),
           parameter = "so4_shallow"
           ) %>%
    select(lake_id, ohio_epa_sample_date,
           analyte = parameter,
           value = result,
           units)
  
  # Read raw data provided via email
  # 9/22/2025 e-mail from Ruth Briland Ruth.Briland@epa.ohio.gov
  # See issue #14 for details
  ohio_epa_chem_ladue <- read_csv(ohio_epa_chem_ladue_file) %>%
    clean_names %>%
    filter(
      parameter %in% c("Sulfate", "DOC", "Chlorophyll_a")
    ) %>%
    mutate(lake_id = 1018,
           # replace / with _ in units
           units = sub(pattern = "/",
                       replacement = "_",
                       unit),
           # format collection date
           ohio_epa_sample_date = as.Date(
             str_extract(sample_dt, "^[^ ]+"), # extract date  
             format = "%m/%d/%Y"),
           parameter = case_when(parameter == "Sulfate" ~ "so4_shallow",
                                 parameter == "DOC" ~ "doc_shallow",
                                 parameter == "Chlorophyll_a" ~ "chla_shallow",
                                 TRUE ~ "GANDALF"),
           result = as.numeric(result)
    ) %>%
    select(lake_id, ohio_epa_sample_date,
           analyte = parameter,
           value = result,
           units)

  
  
  
# Map of observation sites----
  ohio_epa_chem %>%
    # both kiser stations are good
    # both la due are good
    # only Lake Waynoka L-1
    filter(
      grepl(c("kiser|knox|cowan|logan|milton|ladue|la due|lake waynoka|roaming|mohawk|apple"), station, ignore.case = TRUE)
    ) %>%
    st_as_sf(coords = c("longdd", "latdd"), crs = 4326) %>%
    leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      label = ~station, # Specify the label
      labelOptions = labelOptions(
        noHide = TRUE, # Make labels permanently visible
        direction = "auto", # Adjust label position automatically
        textOnly = TRUE # Display text without a background bubble
      )
    )
    
    
# format data---- 
  ohio_epa_chem <- ohio_epa_chem %>%
    # both kiser stations are good
    # both la due are good
    # only Lake Waynoka L-1
    filter(
      grepl(c("kiser|knox|cowan|logan|milton|ladue|la due|lake waynoka l-1|roaming|mohawk|apple"), station, ignore.case = TRUE)
    ) %>%
    mutate(
      lake_id = case_when(grepl("kiser", station, ignore.case = TRUE) ~ 1016,
                          grepl("waynoka", station, ignore.case = TRUE) ~ 1023,
                          grepl("ladue", station, ignore.case = TRUE) ~ 1018,
                          grepl("logan", station, ignore.case = TRUE) ~ 1015,
                          TRUE ~ 9999999
                          )
    ) %>%
  #distinct(station)
  #distinct(watertype) # Lake, good
  #distinct(parameter)
    # la due and Waynoka have a few values for paramter == Depth that are
    # greater than a few meters. These instances all have two depths
    # for each date, however, but the analytes only have one result
    # for each date. Don't know how to interpret Depth parameter.
    # exclude from filter below. Assume all chem measurements are shallow
  filter(parameter %in% c("chla", "so4", "doc")) %>%
  mutate(
    # create a column to document when a result is left censored
    censored = case_when(grepl("<", result) ~ "left",
                         TRUE ~ NA_character_),
    # strip "<" from result, convert to numeric
    result = str_replace(result, pattern = "<", replacement = "") %>%
      as.numeric,
    # convert left censored values to 1/2 detection limit
    result = case_when(censored == "left" ~ result/2,
                        TRUE ~ result),
    # format collection date
    ohio_epa_sample_date = as.Date(cdate, format = "%m-%d-%Y"),
    # replace / with _ in units
    units = sub(pattern = "/",
                replacement = "_",
                unit)
    ) %>% 
    select(lake_id, station, ohio_epa_sample_date,
           analyte = parameter,
           value = result, 
           units
           ) %>%
    mutate(
      # all shallow. See 52-56 above
      analyte = case_when(
        analyte == "chla" ~ "chla_shallow",
        analyte == "doc" ~ "doc_shallow",
        analyte == "so4" ~ "so4_shallow",
        TRUE ~ analyte
      )
      ) %>%
    bind_rows(ohio_epa_chem_milton,
              ohio_epa_chem_ladue) %>%
    
    # # take a look at duplicate observations from same lake, date, analyte.
    # # Duplicates agree reasonably well, go ahead and aggregate
    # ggplot(aes(ohio_epa_sample_date, value)) +
    # geom_point() +
    # facet_wrap(~lake_id + analyte, scales = "free")
    
    # Aggregate duplicate observations by lake, date, and analyte
    group_by(lake_id, ohio_epa_sample_date, analyte) %>%
    summarise(value = mean(value),
              units = unique(units)) %>%
    ungroup %>%
    pivot_wider(
      names_from = analyte,
      values_from = c(value, units),
      names_glue = "ohio_epa_{analyte}_{.value}"
    ) %>%
    rename_with(~ str_remove(., "_value")) %>%
    mutate(ohio_epa_doc_shallow_sample_date = ohio_epa_sample_date,
           ohio_epa_chla_shallow_sample_date = ohio_epa_sample_date) %>%
    rename(ohio_epa_so4_shallow_sample_date = ohio_epa_sample_date) %>%
    relocate(lake_id, contains("so4"), contains("doc"))

  
  
return(ohio_epa_chem)


}