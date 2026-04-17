# SETUP----
# libraries for _targets.R
library(targets)
library(tarchetypes) # for tar_file
library(visNetwork) # for tar_visnetwork
library(quarto) # for tar_quarto
library(styler) # for tidyverse styler conventions
library(miniUI) # for styler Addin
library(pacman) # load packages outside of tar_make
library(trackdown) # manage interaction with google docs

# # for development
# pacman::p_load(
#   tidyverse, # data wrangling
#   janitor, # enforce naming conventions
#   GGally, # pairs plots with rho
#   plotly, # interactive plots
#   tidymodels, # modeling framework
#   kableExtra,
#   sf,
#   leaflet, # nla_chem_comparison
#   readxl,
#   kableExtra,
#   tictoc,
#   mgcv,
#   spmodel,
#   magrittr,
#   english,
#   flextable,
#   patchwork
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
                 "leaflet", # nla_chem_comparison
                 "readxl",
                 "tictoc",
                 "mgcv",
                 "spmodel",
                 "magrittr", # %<>% in finalize_modeling_vars.R
                 "patchwork", # multiple ggplot panels
                 "plotly", # 3-D interaction plots
                 "htmlwidgets", # render 3-D image as PNG
                 "webshot2", # render 3-D image as PNG
                 "magick",
                 "tigris",
                 "ggnewscale",
                 "scatterpie"
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
source("R/get_lake_scale_data.R") # add ag_eco9 to Puerto Rico
source("R/merge_data.R") # merge data from SuRGE repository
source("R/define_original_vars.R") # define full list of variables
source("R/define_modeling_vars.R") # find vars with missing observations
source("R/get_aquamatch_data.R") # aquamatch doc and chla
source("R/get_usace_chem.R") # USACE chemistry data
source("R/get_ohio_epa_chem.R") # ohio EPA chemistry data
source("R/get_ohio_wildlife_chem.R") # chem from Ohio Div of Wildlife
source("R/impute_data.R") # impute missing data
source("R/trans_data.R") # transform data
source("R/update_modeling_vars.R") # update list of vars to reflect transformations
source("R/get_bp_data.R") #format barometric pressure metrics as predictors
source("R/get_national_morpho_data.R") #format national scale morphology data as predictors
source("R/get_solar_data.R") # ERA5 solar irradiance data

# restrict predictor variable list based on collinearity
source("R/aed_corvif.R") # load vif function
source("R/get_final_morpho_vars.R") # restrict morpho vars based on collinearity
source("R/get_final_chem_vars.R") # restrict chem vars based on collinearity
source("R/get_final_clim_vars.R") # restrict clim vars based on collinearity
source("R/get_final_temp_vars.R") # restrict temp vars based on collinearity
source("R/get_final_hydrol_vars.R") # restrict hydrol vars based on collinearity
source("R/get_final_trophic_vars.R") # restrict trophic vars based on collinearity
source("R/get_final_landscape_vars.R") # restrict landscape vars based on collinearity
source("R/finalize_modeling_vars.R") # finalize_modeling_vars()
source("R/clean_predictor_variable_names.R")

# models
#source("R/run_gamm.R") # create gam_object, list of gams
source("R/run_random_forest.R") # create random_forest_object, list of spatial random forest models
source("R/get_variable_importance.R")
source("R/get_interaction_results.R")
source("R/get_ebul_percent.R")

# manuscript
source("R/get_table2_dat.R") # data for table 2
source("R/make_map_figure.R")
source("R/make_predicted_observed_plot_main_text.R")
source("R/make_variable_importance_plot.R")
source("R/make_partial_dependence_plots.R")
source("R/make_interaction_plots.R")
source("R/make_ebul_percent_figure.R")

# TARGETS PIPELINE------------------

list(
  ## get data-----
  ## tar_file initiates tracking of data in each file. targets will
  ## notice if an input file has changed and will re-run the affected targets.
  tar_file(lake_polygons_file, "input_data/2_lake_polygons.gpkg"),
  tar_target(
    name = lake_polygons,
    command = sf::read_sf(lake_polygons_file)
  ),
  
  tar_file(site_descriptors_file, "input_data/3_site_descriptors_data.csv"),
  tar_target(
    name = site_descriptors_data,
    command = readr::read_csv(site_descriptors_file, guess_max = 5000)
  ),
  tar_file(emission_rate_points_file, "input_data/4_emission_rate_points.csv"),
  tar_target(
    name = emission_rate_points_data,
    command = readr::read_csv(emission_rate_points_file, guess_max = 5000)
  ),
  
  tar_file(emissions_lake_file, "input_data/5_emissions_lake.csv"),
  tar_target(
    name = emissions_lake_data,
    command = readr::read_csv(emissions_lake_file, guess_max = 5000)
  ),
  
  tar_file(depth_profiles_file, "input_data/6_depth_profiles.csv"),
  tar_target(
    name = depth_profiles_data,
    command = readr::read_csv(depth_profiles_file, guess_max = 5000)
  ),
  
  tar_file(name = site_data_file, command = "input_data/7_site_data.csv"),
  tar_target(
    name = site_data,
    command = readr::read_csv(site_data_file, guess_max = 5000)
  ),
  
  tar_file(name = lake_scale_file, "input_data/8_lake_scale.csv"),
  tar_target(
    name = lake_scale_data,
    command = get_lake_scale_data(lake_scale_file)
  ),
  
  tar_file(phyto_data_file, "input_data/9_phyto_data.csv"),
  tar_target(
    name = phyto_data,
    command = readr::read_csv(phyto_data_file, guess_max = 5000)
  ),
  
  tar_file(name = bp_data_file, "input_data/10_bp_data.csv"),
  tar_target(
    name = bp_data,
    command = get_bp_data(
      bp_data_file = bp_data_file,
      emission_rate_points_data = emission_rate_points_data
    )
  ),
  
  tar_file(name = national_morpho_file, "input_data/surge_res_morpho_all.csv"),
  tar_target(
    name = national_morpho_data,
    command = get_national_morpho_data(
      national_morpho_file = national_morpho_file,
      lake_scale_data = lake_scale_data
    )
    ),
  
  tar_file(lake_cat_file, "input_data/external_data/lake_cat.csv"),
  tar_target(
    name = lake_cat_data,
    command = get_lake_cat_data(
      lake_cat_file = lake_cat_file
    )
  ),
  
  tar_file(name = missouri_river_strat_file, "input_data/69_70_stratification_indices.csv"),
  tar_target(name = missouri_river_strat_data, 
             command = read_csv(missouri_river_strat_file)),
  
  tar_file(nla17_water_chem_file, "input_data/external_data/nla/nla_2017_water_chemistry_chla-data.csv"),
  tar_file(nla17_sed_chem_file, "input_data/external_data/nla/nla_2017_sediment_chemistry-data.csv"),
  tar_file(nla22_water_chem_file, "input_data/external_data/nla/nla22_waterchem_wide.csv"),
  tar_target(
    name = nla_chem,
    command = get_nla_chem(
      nla17_water_chem_file = nla17_water_chem_file,
      nla17_sed_chem_file = nla17_sed_chem_file,
      nla22_water_chem_file = nla22_water_chem_file
    )
  ),
  
  tar_file(lagos_chem_file, "input_data/external_data/lagos_chem.rds"),
  tar_target(
    name = lagos_chem,
    command = get_lagos_chem(lagos_chem_file = lagos_chem_file)
  ),
  
  tar_file(aquamatch_file, "input_data/external_data/surge_doc-chl_AquaMatch-matchups_2025Sep04.csv"),
  tar_target(
    name = aquamatch_data,
    command = get_aquamatch_data(aquamatch_file = aquamatch_file)
  ),
  
  tar_file(usgs_stations_file, "input_data/external_data/usgs/usgs_stations.gpkg"),
  tar_target(
    name = usgs_stations,
    command = get_usgs_stations(usgs_stations_file = usgs_stations_file)
  ),
  
  tar_file(usgs_chem_file, "input_data/external_data/usgs/usgs_chem.csv"),
  tar_target(
    name = usgs_chem,
    command = get_usgs_chem(
      usgs_chem_file = usgs_chem_file,
      usgs_stations = usgs_stations
    )
  ),
  tar_file(usgs_chla_file, "input_data/external_data/national_chlorophyll_corrected_chlorophyll_a_data.zip"),
  tar_target(
    name = usgs_chla,
    command = get_usgs_chla(
      usgs_chla_file = usgs_chla_file,
      usgs_stations = usgs_stations
    )
  ),
  
  tar_file(williamson_doc_file, "input_data/external_data/williamson_2023_doc.csv"),
  tar_target(
    name = williamson_doc,
    command = get_williamson_doc(williamson_doc_file)
  ),
  
  tar_file(usace_caesar_file, "input_data/external_data/usace/USACE Caesar Creek Data 2009-2025.xlsx"),
  tar_file(usace_cave_file, "input_data/external_data/usace/USACE Cave Run Data_2009-2025.xlsx"),
  tar_file(usace_harsha_file, "input_data/external_data/usace/USACE Harsha WQ Data - 2009-2025.xlsx"),
  tar_file(usace_brookville_buckhorn_carr_file, "input_data/external_data/usace/USACE_Brookville_Buckhorn_Carr Creek_Data.xlsx"),
  tar_file(usace_huntington_file, "input_data/external_data/usace/USACE_Huntington_District.xlsx"),
  tar_file(usace_pittsburg_file, "input_data/external_data/usace/USACE_Pittsburg_District.xlsx"),
  tar_target(usace_chem, 
             command = get_usace_chem(
    usace_caesar_file, usace_cave_file,
    usace_harsha_file,
    usace_brookville_buckhorn_carr_file,
    usace_huntington_file,
    usace_pittsburg_file
  )),
  
  tar_file(ohio_epa_chem_file, "input_data/external_data/ohio_epa/ohio_epa_lake_data.rds"),
  tar_file(ohio_epa_chem_milton_file, "input_data/external_data/ohio_epa/STORET_historicalWQ.csv"),
  tar_file(ohio_epa_chem_ladue_file, "input_data/external_data/ohio_epa/EA3_Chemistry_F01W75.csv"),
  tar_target(
    name = ohio_epa_chem,
    command = get_ohio_epa_chem(
      ohio_epa_chem_file,
      ohio_epa_chem_ladue_file,
      ohio_epa_chem_milton_file
    )
  ),
  
  tar_file(
    ohio_wildlife_chem_file,
    "input_data/external_data/ohio_div_wildlife/DataRequest_USEPA_JakeBeaulieu_20251230.xlsx"
  ),
  tar_target(
    name = ohio_wildlife_chem,
    command = get_ohio_wildlife_chem(ohio_wildlife_chem_file = ohio_wildlife_chem_file)
  ),
  
  tar_file(
    solar_data_file,
    "input_data/external_data/shortwave_radiation.csv"
  ),
  tar_target(
    name = solar_data,
             command = get_solar_data(solar_data_file = solar_data_file)
  ),



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
      missouri_river_strat_data = missouri_river_strat_data,
      lake_cat_data = lake_cat_data,
      nla_chem = nla_chem,
      bp_data = bp_data,
      solar_data = solar_data
    )
  ),


  ## initial list of predictor variables----
  tar_target(
    name = original_vars,
    command = define_original_vars() # no argument to this function
  ),

  ## impute missing chemistry date----
  tar_target(
    name = dat_impute,
    command = impute_data(
      dat_raw = dat_raw,
      lagos_chem = lagos_chem,
      aquamatch_data = aquamatch_data,
      usgs_chem = usgs_chem,
      usgs_chla = usgs_chla,
      williamson_doc = williamson_doc,
      usace_chem = usace_chem,
      ohio_epa_chem = ohio_epa_chem,
      ohio_wildlife_chem = ohio_wildlife_chem
    )
  ),

  ## screen vars for missing observations, select subset for modeling-----
  tar_target(
    name = modeling_vars,
    command = define_modeling_vars(
      dat_impute = dat_impute,
      original_vars = original_vars
    )
  ),

  ## TRANSFORM DATA-----
  tar_target(
    name = dat_trans,
    command = trans_dat(
      dat_impute = dat_impute,
      modeling_vars = modeling_vars
    )
  ),

  ## update list of modeling vars to reflect transformations
  tar_target(
    name = modeling_vars_update,
    command = update_modeling_vars(
      dat_trans = dat_trans,
      modeling_vars = modeling_vars
    )
  ),

  # SCREEN FOR COLLINEARITY----
  # morphology vars
  tar_target(
    name = final_morpho_vars,
    command = get_final_morpho_vars(
      dat_trans = dat_trans,
      modeling_vars_update = modeling_vars_update
    )
  ),

  # hydrology vars
  tar_target(
    name = final_hydrol_vars,
    command = get_final_hydrol_vars(
      dat_trans = dat_trans,
      modeling_vars_update = modeling_vars_update
    )
  ),

  # chemistry vars
  ## not needed. Using all vars
  # tar_target(
  #   name = final_chem_vars,
  #   command = get_final_chem_vars(dat_trans = dat_trans,
  #                                   modeling_vars_update = modeling_vars_update)
  # ),

  # climate vars
  tar_target(
    name = final_clim_vars,
    command = get_final_clim_vars(dat_trans = dat_trans,
                                    modeling_vars_update = modeling_vars_update)
  ),

  # temperature vars
  tar_target(
    name = final_temp_vars,
    command = get_final_temp_vars(dat_trans = dat_trans,
                                    modeling_vars_update = modeling_vars_update)
  ),

  # trophic vars
  tar_target(
    name = final_trophic_vars,
    command = get_final_trophic_vars(
      dat_trans = dat_trans,
      modeling_vars_update = modeling_vars_update
    )
  ),

  # landscape vars
  tar_target(
    name = final_landscape_vars,
    command = get_final_landscape_vars(
      dat_trans = dat_trans,
      modeling_vars_update = modeling_vars_update
    )
  ),
  
  # finalize modeling vars
  tar_target(
    name = modeling_vars_final,
    command = finalize_modeling_vars(
      modeling_vars_update = modeling_vars_update,
      final_morpho_vars = final_morpho_vars,
      final_hydrol_vars = final_hydrol_vars,
      final_trophic_vars = final_trophic_vars,
      final_landscape_vars = final_landscape_vars,
      final_clim_vars = final_clim_vars,
      final_temp_vars = final_temp_vars,
      dat_trans = dat_trans
    )
  ),

  tar_target(
    name = clean_predictor_names_crosswalk,
    command = clean_predictor_variable_names(modeling_vars_final)
  ),

  # MODELS----
  # tar_target(
  #   name = gam_object,
  #   command = run_gamm(modeling_vars_final = modeling_vars_final,
  #                      dat_trans = dat_trans)
  # ),

  tar_target(
    name = random_forest_object,
    command = run_random_forest(
      modeling_vars_final = modeling_vars_final,
      dat_trans = dat_trans
    )
  ),
  
  tar_target(
    name = variable_importance,
    command = get_variable_importance(
      random_forest_object = random_forest_object
    )
  ),
  
  tar_target(
    name = interaction_results,
    command = get_interaction_results(
      random_forest_object = random_forest_object,
      dat_trans = dat_trans
    )
  ),
  
  tar_target(
    name = lake_scale_percent_ebul,
    command = get_ebul_percent(emissions_lake_data = emissions_lake_data,
                                  lake_polygons = lake_polygons, 
                                  dat_trans = dat_trans)
  ),
  
  
  # REPORTS----
  tar_render(
    chemistry_and_chla_data_sources_report,
    "reports/chemistry_and_chla_data_sources.Rmd"
  ),
  
  # MANUSCRIPT----
  tar_target(
    name = table2_dat,
    command = get_table2_dat(
      random_forest_object = random_forest_object,
      dat_trans = dat_trans)
  ),
  
  tar_target(
    name = map_figure,
    command = make_map_figure(
      emissions_lake_data = emissions_lake_data,
      lake_polygons = lake_polygons
    )
  ),
  
  tar_target(
    name = predicted_observed_plot,
    command = make_predicted_observed_plot_main_text(
      random_forest_object = random_forest_object,
      dat_trans = dat_trans
    )
  ),
  
  
  tar_target(
    name = variable_importance_plot,
    command = make_variable_importance_plot(
      variable_importance = variable_importance,
      clean_predictor_names_crosswalk = clean_predictor_names_crosswalk,
      var_group_use = "local_vars_all"
    )
  ),
  
  tar_target(
    name = partial_dependence_plots,
    command = make_partial_dependence_plots(
      random_forest_object = random_forest_object,
      dat_trans = dat_trans,
      variable_importance = variable_importance,
      clean_predictor_names_crosswalk = clean_predictor_names_crosswalk,
      models = c("ch4_total.local_vars_all",
                 "ch4_diffusion.local_vars_all",
                 "ch4_ebullition.local_vars_all"),
      explanatory_vars = c("littoral_fraction",
                           "mean_depth",
                           "nla17_sed_c",
                           "so4_shallow",
                           "surface_area")
    )
  ),
  
  
  tar_target(
    name = interaction_plots,
    command = make_interaction_plots(
      interaction_results = interaction_results
    )
  ),
  
  tar_target(
    refs, 
    "R/references.bib", 
    format = "file"
  ),
  
  
  ## SI material
  tar_target(
    name = SI_variable_importance_globalvars,
    command = make_variable_importance_plot(
      variable_importance = variable_importance,
      clean_predictor_names_crosswalk = clean_predictor_names_crosswalk,
      var_group_use = "global_vars"
    )
  ),
  
  tar_target(
    name = SI_partial_dependence_plots,
    command = make_partial_dependence_plots(
      random_forest_object = random_forest_object,
      dat_trans = dat_trans,
      variable_importance = variable_importance,
      clean_predictor_names_crosswalk = clean_predictor_names_crosswalk,
      models = c("ch4_total.local_vars_all",
                 "ch4_diffusion.local_vars_all",
                 "ch4_ebullition.local_vars_all"),
      explanatory_vars = c("year_completed",
                           "buoyf")
    )
  ),
  
  tar_target(name = SI_ebul_percent_plot,
             command = make_ebul_percent_figure(lake_scale_percent_ebul = lake_scale_percent_ebul)
  ),
  
  # tar_quarto looks at the tar_load calls inside the .qmd to define dependencies 
  # on other targets. It also tracks changes in references.bib
  tar_quarto(
    surge_stats_paper, 
    "R/SuRGE_stats_paper.qmd",
    quiet = FALSE,
    output_file = paste0("SuRGE stats paper [version ", Sys.Date(), "]",".docx")
  ),
  
  tar_quarto(
    surge_supplementary_doc, 
    "R/SuRGE_supplementary_doc.qmd",
    quiet = FALSE,
    output_file = paste0("SuRGE supplementary [version ", Sys.Date(), "]",".docx")
  )
)
