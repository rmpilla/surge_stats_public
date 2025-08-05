# SETUP----
# libraries for _targets.R
library(targets)
library(tarchetypes) # for tar_file

# Set target options:
suppressPackageStartupMessages({ # cleaner output
tar_option_set(
  # Packages that your targets need for their tasks.
  packages = c("tidyverse", 
               "janitor", # enforce naming conventions
               "GGally") # pairs plots with rho 
)
})


# SOURCE CUSTOM FUNCTIONS----
source("R/merge_data.R") # merge data from SuRGE repository


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
  
  ## combine and recreate `dat` object from SuRGE repository-----
  # still need phyto data
  tar_target(
    name = dat,
    command = merge_data(
      site_descriptors_data = site_descriptors_data,
      emission_rate_points_data = emission_rate_points_data,
      emissions_lake_data = emissions_lake_data,
      site_data = site_data,
      lake_scale_data = lake_scale_data,
      phyto_data = phyto_data
      )
  )
  )

