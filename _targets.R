# SETUP----
# libraries for _targets.R
library(targets)
library(tarchetypes) # for tar_file
library(visNetwork) # for tar_visnetwork)
library(styler) # for tidyverse styler conventions
library(miniUI) # for styler Addin
library(pacman) # load packages outside of tar_make


# # for development
# pacman::p_load(
#   tidyverse, # data wrangling
#   janitor, # enforce naming conventions
#   GGally, # pairs plots with rho
#   plotly, # interactive plots
#   tidymodels # modeling framework
# )

# Set target options:
suppressPackageStartupMessages({ # cleaner output
  tar_option_set(
    # Packages that your targets need for their tasks.
    packages = c("tidyverse", 
                 "janitor", # enforce naming conventions
                 "GGally", # pairs plots with rho 
                 "plotly",
                 "tidymodels",
                 "kableExtra", 
                 "sf",
                 "leaflet" # nla_chem_comparison
                 )
  )
})


# SOURCE CUSTOM FUNCTIONS----
source("R/get_lake_cat_data.R") # get lakeCat variables
source("R/get_nla_chem.R") # get NLA data
source("R/get_lagos_chem.R") # read in lagos chem data
source("R/get_usgs_stations.R") # read in USGS stations for missing data
source("R/get_usgs_chem.R") # get usgs chem data from WQP
source("R/get_usgs_chla.R") # get usgs chla data from WQP
source("R/get_williamson_doc.R") # get DOC for some 2016 lakes
source("R/merge_data.R") # merge data from SuRGE repository
source("R/define_original_vars.R") # define full list of variables
source("R/define_modeling_vars.R") # find vars with missing observations
source("R/get_aquamatch_data.R") # aquamatch doc and chla
source("R/impute_data.R") # impute missing data
source("R/trans_data.R") # transform data
source("R/update_modeling_vars.R") # update list of vars to reflect transformations

# TARGETS PIPELINE------------------

list(
  ## get data-----
  ## tar_file initiates tracking of data in each file. targets will
  ## notice if an input file has changed and will re-run the affected targets.
  tar_file(site_descriptors_file, "input_data/3_site_descriptors_data.csv"),
  tar_target(
    name = site_descriptors_data,
    command = readr::read_csv(site_descriptors_file, guess_max = 5000)),
  
  tar_file(emission_rate_points_file, "input_data/4_emission_rate_points.csv"),
  tar_target(
    name = emission_rate_points_data,
    command = readr::read_csv(emission_rate_points_file, guess_max = 5000)),
  
  tar_file(emissions_lake_file, "input_data/5_emissions_lake.csv"),
  tar_target(
    name = emissions_lake_data,
    command = readr::read_csv(emissions_lake_file, guess_max = 5000)),
  
  tar_file(depth_profiles_file, "input_data/6_depth_profiles.csv"),
  tar_target(
    name = depth_profiles_data,
    command = readr::read_csv(depth_profiles_file, guess_max = 5000)),
  
  tar_file(name = site_data_file, command = "input_data/7_site_data.csv"),
  tar_target(
    name = site_data,
    command = readr::read_csv(site_data_file, guess_max = 5000)),
  
  tar_file(name = lake_scale_file, "input_data/8_lake_scale.csv"),
  tar_target(
    name = lake_scale_data,
    command = readr::read_csv(lake_scale_file, guess_max = 5000)),
  
  tar_file(phyto_data_file, "input_data/9_phyto_data.csv"),
  tar_target(
    name = phyto_data,
    command = readr::read_csv(phyto_data_file, guess_max = 5000)),
  
  tar_file(lake_cat_file, "input_data/lake_cat.csv"),
  tar_target(
    name = lake_cat_data,
    command = get_lake_cat_data(
      lake_cat_file = lake_cat_file)),
  
  tar_file(nla17_water_chem_file, "input_data/nla_2017_water_chemistry_chla-data.csv"),
  tar_file(nla17_sed_chem_file, "input_data/nla_2017_sediment_chemistry-data.csv"),
  tar_file(nla22_water_chem_file, "input_data/nla22_waterchem_wide.csv"),
  tar_target(
    name = nla_chem,
    command = get_nla_chem(nla17_water_chem_file = nla17_water_chem_file,
                           nla17_sed_chem_file = nla17_sed_chem_file,
                           nla22_water_chem_file = nla22_water_chem_file)),
  
  tar_file(lagos_chem_file, "input_data/lagos_chem.rds"),
  tar_target(
    name = lagos_chem,
    command = get_lagos_chem(lagos_chem_file = lagos_chem_file)),
  
  tar_file(aquamatch_file, "input_data/surge_doc-chl_AquaMatch-matchups_2025Sep04.csv"),
  tar_target(
    name = aquamatch_data,
    command = get_aquamatch_data(aquamatch_file = aquamatch_file)),
  
  tar_file(usgs_stations_file, "input_data/usgs_stations.gpkg"),
  tar_target(
    name = usgs_stations,
    command = get_usgs_stations(usgs_stations_file = usgs_stations_file)), 
  
  tar_file(usgs_chem_file, "input_data/usgs_chem.csv"),
  tar_target(
    name = usgs_chem, 
    command = get_usgs_chem(usgs_chem_file = usgs_chem_file,
                            usgs_stations = usgs_stations)), 
  
  tar_file(usgs_chla_file, "input_data/national_chlorophyll_corrected_chlorophyll_a_data.zip"),
  tar_target(
    name = usgs_chla,
    command = get_usgs_chla(usgs_chla_file = usgs_chla_file,
                            usgs_stations = usgs_stations)),
  
  tar_file(williamson_doc_file, "input_data/williamson_2023_doc.csv"),
  tar_target(
    name = williamson_doc,
    command = get_williamson_doc(williamson_doc_file)),
  
  ## recreate `dat` object from SuRGE repository-----
  tar_target(
    name = dat_raw,
    command = merge_data(
      site_descriptors_data = site_descriptors_data,
      emission_rate_points_data = emission_rate_points_data,
      emissions_lake_data = emissions_lake_data,
      site_data = site_data,
      lake_scale_data = lake_scale_data,
      phyto_data = phyto_data,
      lake_cat_data = lake_cat_data,
      nla_chem = nla_chem
    )
  ),
  
  
  ## initial list of predictor variables----
  tar_target(
    name = original_vars,
    command = define_original_vars() # no argument to this function
  ),
  
  ## impute missing chemistry date
  tar_target(
    name = dat_impute,
    command = impute_data(dat_raw = dat_raw, 
                          lagos_chem = lagos_chem, 
                          aquamatch_data = aquamatch_data)
  ),

  ## screen vars for missing observations, select subset for modeling-----
  tar_target(
    name = modeling_vars,
    command = define_modeling_vars(dat_impute = dat_impute,
                                original_vars = original_vars)
  ),
  
  ## transform data-----
  tar_target(
    name = dat_trans,
    command = trans_dat(dat_impute = dat_impute,
                        modeling_vars = modeling_vars)
  ),

  ## update list of modeling vars to reflect transformations-----
  tar_target(
    name = modeling_vars_update,
    command = update_modeling_vars(dat_trans = dat_trans,
                                   modeling_vars = modeling_vars)
  ),
  # next step is screen for collinearity
  

  tar_render(nla_chem_comparison_report, 
             "reports/nla_chem_comparison_report.Rmd")
)

