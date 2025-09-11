define_modeling_vars <- function(original_vars, dat_impute) {
  
  # CREATE LIST TO HOLD SUBSET OF VARS TO BE USED FOR MODELING----
  modeling_vars <- original_vars
  
  # 1. Morphology variables----
  # thermdep2 is NA for unstratified lakes
  # some negative buoyf values
  # One missing buoyf value (lake 71, no depth profile at that lake)
  # missing NWI data (see SuRGE issue #110)
  dat_impute %>% 
    select(lake_id, site_id, visit, 
           all_of(
             unname( # must remove name attribute for all_of
               original_vars$morpho_vars
             ) # close unname
           ) # close all_of
    ) %>% # close select
    # filter missing values
    filter(rowSums(is.na(.)) > 0) %>%
    select(lake_id, where(anyNA)) %>%
    distinct()
  
  # list of vars to exclude from list
  exclude_morpho_vars <- c(
    "thermdep2", # NA for a few lakes
    # [8/11/2025] missing NWI for a few lakes. Mark Mitchell working on it.
    # see SuRGE repo issue #110
    "lacustrine", "limnetic", "littoral", "palustrine", 
    "riverine", "intermittent", "lower_perennial", "upper_perennial", 
    "emergent", "aquatic_bed")
  
  # remove excluded vars from original_vars
  modeling_vars$morpho_vars <- modeling_vars$morpho_vars[!(modeling_vars$morpho_vars %in% exclude_morpho_vars)] 
  
  
  # 2. Trophic variables----
  dat_impute %>% 
    select(lake_id, site_id, visit, 
           all_of(
             unname( # must remove name attribute for all_of
               original_vars$trophic_vars
             ) # close unname
           ) # close all_of
    ) %>% # close select
    # filter missing values
    select(lake_id, where(anyNA)) %>%
    distinct()
  
  
  # FEW MEASUREMENTS OF LAB-BASED CHLA IN 2016!
  # Inspect missing values
  dat_impute %>% 
    select(lake_id, site_id, visit, 
           # no2_3_shallow, nh4_shallow, op_shallow,
           # tp_shallow, tn_shallow, 
           # doc_shallow,
           turbidity_shallow, turbidity_deep, 
           chla_sonde_shallow, chla_sonde_deep 
    ) %>%
    # filter on variable of interest
    # 250 and 281 visit 1 missing chem (samples got lost in shipping)
    # filter(is.na(tp_shallow)) %>%
    # filter(is.na(tn_shallow)) %>%

    # sonde
    # filter(is.na(turbidity_shallow)) %>% # missing from most of 1009, 184, 190, 235, 237, 250, 
    # 276, 281, 328, 9
    # filter(is.na(turbidity_deep)) %>% # 1000, 999, 328, 281, 276, 250, 237, 236...
    filter(is.na(chla_sonde_shallow)) %>% # 157, 159, 165, 211, 247, 250, 281, 302,323, 999
    # filter(is.na(chla_sonde_deep)) %>%
    print(n=Inf)
  
  # list of vars to exclude from list
  exclude_trophic_vars <- c(
    "algal_density", "cyano_density" # only SuRGE
  )
  
  
  # remove excluded vars from original_vars
  modeling_vars$trophic_vars <- modeling_vars$trophic_vars[!(modeling_vars$trophic_vars %in% exclude_trophic_vars)] 
  
  # 6. Chemistry variables----
  dat_impute %>% 
    select(lake_id, site_id, visit, 
           all_of(
             unname( # must remove name attribute for all_of
               original_vars$chem_vars
             ) # close unname
           ) # close all_of
    ) %>% # close select
    # filter missing values
    filter(rowSums(is.na(.)) > 0) %>%
    select(lake_id, where(anyNA)) %>%
    distinct()
  
  # Restrict list to those we want for modeling
  modeling_vars$chem_vars <- modeling_vars$chem_vars %>%
    # 2016 only has shallow chemistry
    .[!grepl("_deep", .)] %>% # remove deep chemistry
    # add in deep sonde measurement
    c("do_deep") %>%
    # NO2 not widely available
    .[!grepl("no2_shallow", .)] %>% #
    # specific conductivity missing from a few lakes
    .[!grepl("sp_cond_shallow", .)] %>%
    # pH not a predictor and missing from a few lakes
    .[!grepl("ph_shallow", .)] %>%
    # IC and metals only for >=2021, but we do want SO4
    .[!(. %in% paste0(c("f", "cl", "br",  # anions
                        "al", "as", "ba", "be", "ca", # metals
                        "cd", "cr", "cu", "fe",
                        "k", "li", "mg", "mn", "na",
                        "ni", "pb", "p", "sb", "si",
                        "sn", "sr", "s", "v", "zn"),
                      "_shallow"))] %>%
    set_names("chem_var")

  # Review after excluding vars above
  dat_impute %>% 
    select(lake_id, site_id, visit, 
           all_of(
             unname( # must remove name attribute for all_of
               modeling_vars$chem_vars
             ) # close unname
           ) # close all_of
    ) %>% # close select
    # filter missing values
    select(lake_id, where(anyNA)) 
    # distinct() %>%
    # filter(lake_id >= 1001) %>%
    # select(lake_id, nh4_shallow,  
    #        toc_shallow, turbidity_shallow, turbidity_deep, 
    #        do_shallow, do_deep) %>%
    # print(n=Inf)
  
  
return(modeling_vars)
}