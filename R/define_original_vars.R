define_original_vars <- function() {
  # 1. MORPHO VARIABLES--------
  morpho_vars <- c(
    "surface_area",
    "shoreline_length",
    "shoreline_development",
    "max_width", # Max distance perpendicular to max lake length
    "mean_width", # Lake surface area divided by fetch
    "max_length", # longest open water distance of a lake (fetch)
    "mean_depth",
    "max_depth",
    "site_depth", # point specific sample depth
    "volume",
    "littoral_fraction",
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
    set_names("morpho_var")
  
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
    "doc_shallow",
    
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
    "doc_predicted_sample_month_visit2"
  ) %>%
    # name vector elements. This format is handy when joining
    # all these vectors into a DF
    set_names("trophic_var")
  
  # 3. CLIMATE VARIABLES--------
  # what about lake_cat variables?
  clim_vars <- c(
    # ERA 5 variables
    "precipitation",
    "wind_speed"
  ) %>%
    # name vector elements. This format is handy when joining
    # all these vectors into a DF
    set_names("clim_var")
  
  # 8. TEMPERATURE
  temp_vars <-
    paste(
      "temp",
      c("deep", "shallow"),
      sep = "_"
    ) %>%
    # name vector elements. This format is handy when joining
    # all these vectors into a DF
    set_names("temp_var")
  
  # 4. HYDROLOGY VARIABLES--------
  hydrol_vars <- c(
    "e_i",
    "residence_time",
    "e_i_type"
  ) %>%
    # name vector elements. This format is handy when joining
    # all these vectors into a DF
    set_names("hydrol_var")
  
  
  # 5. LANDSCAPE VARIABLES--------
  landscape_vars <- c("kffactcat", "kffactws", # soil erodibility factor
                      "omcat", "omws",
                      "scat", "sws", # Mean % lithological sulfur
                      "pctcrop2019cat", "pctcrop2019ws",  
                      "catareasqkm", "wsareasqkm",
                      "nitrogencat", "nitrogenws", 
                      "npdesdenscat", "npdesdensws", # Density of NPDES sites
                      "agkffactcat", "agkffactws", # ag # soil erodibility factor
                      "elevcat", "elevws") %>%
    # name vector elements. This format is handy when joining
    # all these vectors into a DF
    set_names("landscape_var")
  
  # 6. CHEMISTRY VARIABLES--------
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
                             "do", "ph", "sp_cond", "turbidity"
                           ),
                           stringsAsFactors = FALSE
  ) %>%
    mutate(analytes_to_fill = paste0(Var2, Var1)) %>% # paste
    pull(analytes_to_fill) %>% # extract to vector
    # name vector elements. This format is handy when joining
    # all these vectors into a DF
    set_names("chem_var")
  
  # 7. OTHERS--------
  other_vars <- c(
    "year_completed",
    "ag_eco9_nm"
  ) %>%
    # name vector elements. This format is handy when joining
    # all these vectors into a DF
    set_names("other_var")
  
  # 8. RETURN VALUES----
  return(
    tibble::lst(
      morpho_vars, trophic_vars, clim_vars, temp_vars, hydrol_vars,
         chem_vars, other_vars)
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
