# RETRIEVE USGS STATIONS FOR MISSING SURGE LAKES
# PACKAGES TEMPORARILY INSTALLED, DATA DOWNLOADED AND SAVED LOCALLY. CODE
# IS NOT INTENDED TO BE RERUN TO AVOID PROBLEM WITH
# POTENTIAL CHANGES IN API.

get_usgs_stations <- function(usgs_stations_file) {
  

  # # libraries needed for data retrieval
  # renv::install("dataRetrieval")
  # library(dataRetrieval)
  # renv::install("sf")
  # library("sf")
  # renv::install("leaflet")
  # library(leaflet)
  # 
  # GET USGS MONITORING SITE----
  # # Get all USGS lake sites in KY, IN, OH, ID, and OR
  # all_sites <-
  #   bind_rows(
  #     whatNWISsites(stateCd = "OH"),
  #     whatNWISsites(stateCd = "KY"),
  #     whatNWISsites(stateCd = "IN"),
  #     whatNWISsites(stateCd = "ID"), # lake_id == 323
  #     whatNWISsites(stateCd = "OR")  # lake_id == 239
  #   ) %>%
  #   filter(
  #     site_tp_cd == "LK" #lakes only
  #   ) %>%
  #   st_as_sf( # convert to sf
  #     coords = c("dec_long_va", "dec_lat_va"),
  #     crs = st_crs(4326)
  #   )
  # 
  # # optionally view stations
  # all_sites %>%
  #   leaflet() %>%
  #   addTiles() %>%
  #   addMarkers(.)
  # 
  # 
  # # View the structure of the data
  # head(all_sites)
  # 
  # # GET LAKE POLYGONS----
  # lake_polygons <- st_read("input_data/2_lake_polygons.gpkg") %>%
  #   st_transform(4326) %>%
  #   # only retain polygons for lakes that we need data
  #   # see nla_chem_comparison_report.Rmd for details
  #   filter(
  #     lake_id %in% c(1001:1032, 83, 204, 239, 323)
  #   )
  # 
  # # make sure spatial objects have same projection
  # st_crs(all_sites) == st_crs(lake_polygons) # TRUE
  # 
  # # optionally view polygons
  # lake_polygons %>%
  #   leaflet() %>%
  #   addTiles() %>%
  #   addPolygons(.)
  # 
  # # GET USGS SITES PER POLYGON----
  # # Perform a spatial join to find which sites fall within which polygons
  # sites_in_polygons <- bind_rows(
  #   st_intersection(all_sites, lake_polygons),
  #   # Manually add a few sites not picked up by intersection
  #   all_sites %>% filter(site_no == 401409082584000) %>% mutate(lake_id = 1002), # Alum Cr. Beach
  #   all_sites %>% filter(site_no == 04201900) %>% mutate(lake_id = 1018), # LaDue
  #   all_sites %>% filter(site_no == 413918080502500) %>% mutate(lake_id = 1022), # Roaming Rock
  #   all_sites %>% filter(site_no == 03092450) %>% mutate(lake_id = 1024), # MJ Kirwan
  #   all_sites %>% filter(site_no == 401926081105100) %>% mutate(lake_id = 1030), # Tappan Lake Beach
  #   all_sites %>% filter(site_no == 395433081250100) %>% mutate(lake_id = 1029), # Senecaville Lake Beach
  #   all_sites %>% filter(site_no == 390111084080300) %>% mutate(lake_id = 1031), # Harsha Lake Beach  
  #   all_sites %>% filter(site_no == 390121084054100) %>% mutate(lake_id = 1031), # Harsha Lake Campground Beach 
  #   all_sites %>% filter(site_no == 13141300) %>% mutate(lake_id = 323), # Mormon Reservoir, ID 
  # )
  # 
  # 
  # # View the result, which includes both site information and polygon attributes
  # leaflet() %>%
  #   addTiles() %>%
  #   addPolygons(data = lake_polygons) %>%
  #   addMarkers(dat = sites_in_polygons, label = ~site_no)
  # 
  # # This output file is target "usgs_stations_file"
  # st_write(sites_in_polygons,
  #          "input_data/usgs_stations.gpkg",
  #         append = FALSE)

  
  usgs_stations <- st_read(usgs_stations_file)
  
  return(usgs_stations)
}