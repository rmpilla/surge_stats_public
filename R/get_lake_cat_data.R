
get_lake_cat_data <- function(lake_cat_file) {


# DATA PULLED FROM API, THEN STORED LOCALLY
# CODE TO PULL FROM API IS COMMENTED OUT
# KEEPING IN SURGE_STAT REPO TO RECORD 
# PROVENANCE OF LAKECAT DATA.

# # devtools::install_github("USEPA/StreamCatTools", build_vignettes=FALSE)
# # download from API
# # Names of lakeCat variables
# # no longer pulling "catareasqkm", "wsareasqkm"! See
# # https://github.com/USEPA/StreamCatTools/issues/62
# lake_cat_vars <- lc_get_params(param='metric_names') %>%
#   as_tibble %>%
#   # ignore older data
#   filter(!grepl(paste0(1984:2018, collapse = "|"), value)) %>%
#   pull
# 
# # Get lakeCat data
# lake_cat <- lc_get_data(metric = paste(lake_cat_vars, collapse=","),
#                        aoi = 'cat,ws',
#                        comid = surge_comid$nhd_plus_waterbody_comid,
#                        showAreaSqKm=TRUE) %>%
#   janitor::clean_names() %>%
#   # add lake_id
#   left_join(surge_comid)

  
# # Now get streamcat data for Carr Fork Lake (comid 456124)  
#   stream_cat_vars <- sc_get_params(param='metric_names') %>%
#     as_tibble %>%
#     # ignore older data
#     filter(!grepl(paste0(1984:2018, collapse = "|"), value)) %>%
#     # restrict to streamCat variables that are also in lakeCat
#     filter(value %in% lake_cat_vars) %>%
#     pull
# 
#   # Get streamCat data
#   stream_cat <- sc_get_data(metric = paste(stream_cat_vars, collapse=","),
#                          aoi = 'cat,ws',
#                          comid = 456124,
#                          showAreaSqKm=TRUE) %>%
#     janitor::clean_names() %>%
#     # add lake_id
#     mutate(lake_id = 1009) # carr fork
#   
#   
#   write_csv(dplyr::bind_rows(lake_cat, stream_cat), file =  "input_data/lake_cat.csv")
  
  
# READ LOCAL FILE
lake_cat_data <- readr::read_csv(lake_cat_file, guess_max = 5000) %>%
  janitor::clean_names() %>%
  # omit lakes that were not sampled
  filter(!(lake_id %in% c(56, # physically inaccesible
                          15, # landowner denial 
                          1033 # Falls Lake
                          ))) %>%
# Aggregate and select variables of interest
  mutate(nitrogencat = manurecat + cbnfcat + fertcat) %>%
  mutate(nitrogenws = manurews + cbnfws + fertws) %>%
  select(
    comid,
    lake_id,
    kffactcat,
    kffactws,
    runoffcat,
    runoffws,
    omcat,
    omws,
    scat,
    sws,
    pctcrop2019cat,
    pctcrop2019ws,
    catareasqkm, 
    wsareasqkm, 
    nitrogencat,
    nitrogenws,
    npdesdenscat,
    npdesdensws,
    agkffactcat,
    agkffactws,
    elevcat,
    elevws
  )

# # Inventory records
# dim(lake_cat_data) # 145 rows.  which one is missing?  
# 
# surge_comid[!(surge_comid$comid %in% lake_cat$comid), ] #800045123 is not in lakeCat
# 
# lake.list %>% filter(nhd_plus_waterbody_comid == 800045123) # Puerto Rico

return(lake_cat_data)
}
