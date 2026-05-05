define_original_vars <- function() {
  # 1. MORPHO VARIABLES--------
  morpho_vars <- c(
    "surface_area",
    "surface_area_global",
    "shoreline_length",
    "shoreline_development",
    "max_width", # Max distance perpendicular to max lake length
    "mean_width", # Lake surface area divided by fetch
    "max_length", # longest open water distance of a lake (fetch)
    "mean_depth",
    "mean_depth_global",
    "max_depth",
    "max_depth_global",
    "site_depth", # point specific sample depth
    "volume",
    "littoral_fraction",
    "littoral_fraction_global",
    "circularity", # circular (value approaches 1) vs elongated (value approaches 0).
    "dynamic_ratio", # SA:mean depth. Low values = bowl shaped, high values = dish-like
    
    # NWI variables
    ## lacustrine + subclasses
    "lacustrine", # < 30% persistent emergent vegetation
    "limnetic", # >2.5m deep in lacustrine
    "littoral", # <2.5m deep in lacustrine, or max extent of nonpersistent emergents
    
    ## palustrine + subclasses
    "palustrine", # wetlands, <8Ha, max depth < 2.5m
    
    ## riverine + subclasses
    "riverine", #  habitats contained within a channel
    "intermittent", # seasonally inundated riverine
    "lower_perennial", # low gradient riverine
    "upper_perennial", # high gradient riverine
    
    ## classes of lacustrine/palustrine/riverine
    "emergent",
    "aquatic_bed",
    
    ## stratification variables
    "buoyf",
    "thermdep2"
  ) %>%
    # name vector elements. This format is handy when joining
    # all these vectors into a DF
    rlang::set_names("morpho_var")
  
  # 2. TROPHIC VARIABLES--------
  trophic_vars <- c(
    # lab-based variables
    "algal_density", # cell counts
    "cyano_density", # cell counts
    "chla_lab_shallow",
    
    # 2016 survey include only shallow chemistry
    "no2_3_shallow",
    "tp_shallow",
    "tn_shallow",
    "op_shallow",
    "toc_shallow",
    
    # sonde values
    "turbidity_shallow",
    "turbidity_deep",
    "chla_sonde_shallow",
    "chla_sonde_deep",
    
    # LAGOS
    "chl_predicted_sample_season_visit1",
    "doc_predicted_sample_season_visit1",
    "doc_predicted_sample_season_visit2",
    "chl_predicted_sample_season_visit2",
    "chl_predicted_sample_month_visit1",
    "chl_predicted_sample_month_visit2",
    "doc_predicted_sample_month_visit1",
    "doc_predicted_sample_month_visit2",
    
    # NLA sediment chemistry
    "nla17_sed_c"
  ) %>%
    # name vector elements. This format is handy when joining
    # all these vectors into a DF
    rlang::set_names("trophic_var")
  
  # 3. CLIMATE VARIABLES--------
  clim_vars <- c(
    # ERA 5 variables
    "precipitation_chamber",
    "precipitation_trap",
    "wind_speed_chamber",
    "wind_speed_trap",
    "delta_bp_ebu",
    "delta_bp_diff",
    "shortwave_radiation_sum_mj_m2", # see solar data
    
    # LakeCat variables
    "precip8110cat", # 1981-2010 mean precip (mm) catchment
    "precip8110ws", # 1981-2010 mean precip (mm) watershed
    "runoffcat", # mean runoff (mm) catchment
    "runoffws" # mean runoff (mm) watershed
  ) %>%
    # name vector elements. This format is handy when joining
    # all these vectors into a DF
    rlang::set_names("clim_var")
  
  # 4. TEMPERATURE----
  temp_vars <-
    paste(
      "temp",
      c("deep", "shallow"),
      sep = "_"
    ) %>%
    # LakeCat PRISM temperature
    c("tmax8110cat", # 1981-2010 normal maximum temp catchment
      "lat") %>% # latitude, a proxy for light, but might correlate with temp
    # name vector elements. This format is handy when joining
    # all these vectors into a DF
    rlang::set_names("temp_var")
  
  # 5. HYDROLOGY VARIABLES--------
  hydrol_vars <- c(
    "e_i",
    "residence_time"
  ) %>%
    # name vector elements. This format is handy when joining
    # all these vectors into a DF
    rlang::set_names("hydrol_var")
  
  
  # 6. LANDSCAPE VARIABLES--------
  landscape_vars <- c("kffactcat", # soil erodibility factor
                      "omcat",
                      "scat", # Mean % lithological sulfur
                      "pctcrop2019cat",
                      "wsareasqkm",
                      "nitrogencat", 
                      #"npdesdenscat", "npdesdensws", # Density of NPDES sites. all 0 except 1
                      "agkffactcat",  # ag soil erodibility factor
                      "elevcat",
                      "rda") %>% # relative drainage area
    # name vector elements. This format is handy when joining
    # all these vectors into a DF
    rlang::set_names("landscape_var")
  
  # 7. CHEMISTRY VARIABLES--------
  chem_vars <- expand.grid(c("_deep", "_shallow"),
                           c(
                             #"tn", "tp", "op", "no2_3", in tropic status vars
                             "nh4", "no2",  # nutrients
                             "f", "cl", "br", "so4", # anions
                             "toc", # organics, doc in trophic status
                             "al", "as", "ba", "be", "ca", # metals
                             "cd", "cr", "cu", "fe",
                             "k", "li", "mg", "mn", "na",
                             "ni", "pb", "p", "sb", "si",
                             "sn", "sr", "s", "v", "zn",
                             "do", "ph", "sp_cond"
                           ),
                           stringsAsFactors = FALSE
  ) %>%
    mutate(analytes_to_fill = paste0(Var2, Var1)) %>% # paste
    pull(analytes_to_fill) %>% # extract to vector
    # name vector elements. This format is handy when joining
    # all these vectors into a DF
    rlang::set_names("chem_var")
  
  # 7. OTHERS--------
  other_vars <- c(
    "age",
    "ag_eco9_nm",
    "doy"
  ) %>%
    # name vector elements. This format is handy when joining
    # all these vectors into a DF
    rlang::set_names("other_var")
  
  # 8. RETURN VALUES----
  return(
    tibble::lst(
      # first list element
      local_vars_all = tibble::lst(
        # exclude global morpho vars
        morpho_vars = morpho_vars[!grepl("_global", morpho_vars)], 
        # exclude LAGOS vars
        trophic_vars = trophic_vars[!grepl("predicted", trophic_vars)], 
        # exclude global climate variables
        clim_vars = clim_vars[!grepl(c("runoff|precip8110"), clim_vars)], 
        # exclude global temp variables
        temp_vars = temp_vars[!grepl(c("tmax"), temp_vars)], 
        hydrol_vars,
        # exclude most landscape vars
        landscape_vars = landscape_vars[landscape_vars %in% c("wsareasqkm", "rda")], 
        chem_vars, other_vars
      ),
      
      # second list element
      local_vars_no_nla = tibble::lst(
        # exclude global morpho vars
        morpho_vars = morpho_vars[!grepl("_global", morpho_vars)], 
        # exclude NLA sediment chemistry
        trophic_vars = trophic_vars[!grepl(c("nla17_sed_c|predicted"), trophic_vars)], 
        # exclude global climate variables
        clim_vars = clim_vars[!grepl(c("runoff|precip"), clim_vars)], 
        # exclude global temp variables
        temp_vars = temp_vars[!grepl(c("tmax"), temp_vars)], 
        # exclude NLA derived hydrology variables
        #hydrol_vars,
        # exclude most landscape vars
        landscape_vars = landscape_vars[landscape_vars %in% c("wsareasqkm", "rda")], 
        chem_vars, other_vars
      ),
      
      # third list element
      global_vars = tibble::lst(
        # exclude on site measurements
        morpho_vars = morpho_vars[!morpho_vars %in% c("buoyf", "thermdep2", "site_depth",
                                                      "max_depth", "mean_depth", "surface_area",
                                                      "littoral_fraction")], 
        # only keep LAGOS predictors
        trophic_vars = trophic_vars[grepl("predicted", trophic_vars)], 
        # only keep lakeCat climate variables
        clim_vars = clim_vars[clim_vars %in% c("precip8110cat", "precip8110ws", "runoffcat", "runoffws")], 
        # only keep lakeCat temp variables
        temp_vars = temp_vars[grepl(c("tmax|lat"), temp_vars)], 
        # exclude NLA derived hydrology variables
        #hydrol_vars,
        # keep all landscape vars
        landscape_vars, 
        # exclude all chem vars
        #chem_vars,
        # only keep ag_eco9_nm
        other_vars = other_vars[other_vars %in% c("ag_eco9_nm", "doy")]
      )
    )
  )
}

# 9. HAVE WE CLASSIFIED ALL VARIABLES?----
# # yes
# dat %>%
#   dplyr::select(
#     # exclude variables that are not predictors
#     -contains("flag"),
#     -contains("comment"),
#     -contains("units"),
#     -contains("date")
#   ) %>%
#   colnames() %>% # extract names
#   dplyr::as_tibble() %>% # convert to tibble
#   dplyr::rename(var = value) %>%
#   dplyr::full_join(
#     map(list(
#       morpho_vars, trophic_vars, clim_vars, temp_vars, hydrol_vars,
#       chem_vars, other_vars
#     ), ~ .x %>% enframe(name = "var_type", value = "var")) %>%
#       list_rbind()
#   ) %>% # close full_join
#   filter(is.na(var_type)) %>% # filter out classified variables
#   print(n = Inf)
