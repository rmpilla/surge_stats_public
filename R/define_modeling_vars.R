define_modeling_vars <- function(dat_raw_lakecat, original_vars) {
  
  # CREATE LIST TO HOLD SUBSET OF VARS TO BE USED FOR MODELING----
  modeling_vars <- original_vars
  
  # 1. Morphology variables----
  # thermdep2 is NA for unstratified lakes
  # some negative buoyf values
  # One missing buoyf value (lake 71, no depth profile at that lake)
  # missing NWI data (see SuRGE issue #110)
  dat_raw %>% 
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
  dat_raw %>% 
    select(lake_id, site_id, visit, 
           all_of(
             unname( # must remove name attribute for all_of
               original_vars$trophic_vars
             ) # close unname
           ) # close all_of
    ) %>% # close select
    # filter missing values
    filter(rowSums(is.na(.)) > 0) %>%
    select(lake_id, where(anyNA)) %>%
    distinct()
  ##PICK UP HERE. MISSING SOME LAB-BASED CHLA FROM 2016!
  
  # 6. Chemistry variables----
  dat_raw %>% 
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
  
  # list of chem vars to exclude from modeling list
  exclude_chem_vars <- 
    # 2016 only has shallow chemistry
    expand.grid("_deep",
                c(
                  #"tn", "tp", "op", "no2_3", in tropic status vars
                  "nh4", "no2",  # nutrients
                  "f", "cl", "br", "so4", # anions
                  "doc", "toc", # organics
                  "al", "as", "ba", "be", "ca", # metals
                  "cd", "cr", "cu", "fe",
                  "k", "li", "mg", "mn", "na",
                  "ni", "pb", "p", "sb", "si",
                  "sn", "sr", "s", "v", "zn"
                ),
                stringsAsFactors = FALSE
    ) %>%
    mutate(chem_vars_to_exclude = paste0(Var2, Var1)) %>% # paste
    pull(chem_vars_to_exclude) 
  
  # remove excluded vars from original_vars
  modeling_vars$chem_vars <- modeling_vars$chem_vars[!(modeling_vars$chem_vars %in% exclude_chem_vars)] 
  
  
  
return(modeling_vars)
}