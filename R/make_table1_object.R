make_table1_object <- function(clean_predictor_names_crosswalk){
  
  table1_dat <- tribble(
    ~Variable_Group, ~Variable, ~Scale, ~Response,
    
    # morphometry
    "morphometry", "buoyf", "local", "diffusion, ebullition, total",
    "morphometry", "site_depth", "local", "diffusion, ebullition, total",
    "morphometry", "circularity", "local, national", "diffusion, ebullition, total",      
    "morphometry", "dynamic_ratio", "local, national", "diffusion, ebullition, total",        
    "morphometry", "littoral_fraction", "local, national", "diffusion, ebullition, total",    
    "morphometry", "max_depth", "local, national", "diffusion, ebullition, total",           
    "morphometry",  "max_length", "local, national", "diffusion, ebullition, total",           
    "morphometry",  "max_width", "local, national", "diffusion, ebullition, total",            
    "morphometry",  "mean_depth", "local, national", "diffusion, ebullition, total",           
    "morphometry",  "mean_width", "local, national", "diffusion, ebullition, total",           
    "morphometry",  "shoreline_development", "local, national", "diffusion, ebullition, total",
    "morphometry", "shoreline_length", "local, national", "diffusion, ebullition, total",     
    "morphometry", "surface_area", "local, national", "diffusion, ebullition, total",        
    "morphometry", "volume", "local, national", "diffusion, ebullition, total",
    
    # climate
    "climate", "temp_deep", "local", "diffusion, ebullition, total",
    "climate", "temp_shallow", "local", "diffusion, ebullition, total",
    "climate", "precipitation_chamber", "local", "diffusion",
    "climate", "precipitation_trap", "local", "ebullition",
    "climate", "wind_speed_chamber", "local", "diffusion",
    "climate", "wind_speed_trap", "local", "ebullition",
    "climate", "delta_bp_ebu", "local", "ebullition",
    "climate", "delta_bp_diff", "local", "diffusion",
    "climate", "shortwave_radiation_sum_mj_m2", "local", "diffusion",
    "climate", "precip8110cat", "national", "diffusion, ebullition, total",
    "climate", "precip8110ws", "national", "diffusion, ebullition, total",
    "climate", "runoffcat", "national", "diffusion, ebullition, total",
    "climate", "runoffws", "national", "diffusion, ebullition, total",
    "climate", "tmax8110cat", "national", "diffusion, ebullition, total",
    
    
    # trophic
    "trophic", "chla_lab_shallow", "local", "diffusion, ebullition, total",
    "trophic", "no2_3_shallow", "local", "diffusion, ebullition, total",
    "trophic", "tp_shallow", "local", "diffusion, ebullition, total",
    "trophic", "tn_shallow", "local", "diffusion, ebullition, total",
    "trophic", "op_shallow", "local", "diffusion, ebullition, total",
    "trophic", "toc_shallow", "local", "diffusion, ebullition, total",
    "trophic", "chla_sonde_shallow", "local", "diffusion, ebullition, total",
    "trophic", "nla17_sed_c", "local", "diffusion, ebullition, total",
    "trophic", "chl_predicted_sample_month_visit1", "national", "diffusion, ebullition, total",
    "trophic", "doc_predicted_sample_month_visit1", "national", "diffusion, ebullition, total",
    
    # hydrology
    "hydrology", "e_i", "local", "diffusion, ebullition, total",
    "hydrology", "residence_time", "local", "diffusion, ebullition, total",
    
    # landscape
    "landscape", "wsareasqkm", "local, national", "diffusion, ebullition, total",
    "landscape", "rda", "local, national", "diffusion, ebullition, total",
    "landscape", "kffactcat", "national",  "diffusion, ebullition, total",
    "landscape", "omcat", "national",  "diffusion, ebullition, total",
    "landscape", "scat", "national",  "diffusion, ebullition, total",
    "landscape", "pctcrop2019cat", "national",  "diffusion, ebullition, total",
    "landscape", "wsareasqkm", "national",  "diffusion, ebullition, total",
    "landscape", "nitrogencat", "national",  "diffusion, ebullition, total",
    "landscape", "agkffactcat", "national",  "diffusion, ebullition, total",
    "landscape", "elevcat", "national",  "diffusion, ebullition, total",
    
    # chemistry
    "chemistry", "nh4_shallow", "local", "diffusion, ebullition, total",
    "chemistry", "so4_shallow", "local", "diffusion, ebullition, total",
    "chemistry", "toc_shallow", "local", "diffusion, ebullition, total",
    "chemistry", "do_deep", "local", "diffusion, ebullition, total",
    "chemistry", "turbidity_shallow", "local", "diffusion, ebullition, total",
    
    # other
    "other", "age", "local", "diffusion, ebullition, total",
    "other", "ag_eco9_nm", "local, national", "diffusion, ebullition, total",
    "other", "doy", "local", "diffusion, ebullition, total"
  ) %>%
    left_join(clean_predictor_names_crosswalk,
              by = c("Variable" = "variable")) %>%
    mutate(variable_clean_flextable = str_replace_all(variable_clean, pattern = "~", replacement = " "),
           variable_clean_flextable = case_when(Variable == "e_i" ~ "evaporation:inflow",
                                                Variable == "nh4_shallow" ~ "NH~4~^+^ (shallow)",
                                                Variable == "so4_shallow" ~ "SO~4~^2-^ (shallow)",
                                                TRUE ~ variable_clean_flextable),
           Variable_Group = factor(Variable_Group, levels = c("morphometry", "climate", "trophic",
                                                              "hydrology", "landscape", "chemistry", "other")),
           Response = factor(Response, levels = c("diffusion, ebullition, total",
                                                  "diffusion", "ebullition"))) %>%
    filter(!is.na(variable_clean)) %>%
    select(Variable_Group, variable_clean_flextable, flextable_units, 
           Scale, Response) %>%
    arrange(Variable_Group, Scale, Response, variable_clean_flextable)
  
  
  
  bottom_varGroup_borders <- table1_dat %>%
    mutate(row_id = row_number()) %>%
    group_by(Variable_Group) %>%
    mutate(is_last = row_id == max(row_id)) %>%
    ungroup()
  
  
  
  table1_object <- flextable(table1_dat) %>%
    ftExtra::colformat_md() %>%
    theme_booktabs() %>%
    fontsize(size = 7.5, part = "all") %>%
    padding(padding = 0, part = "all") %>% # Set all padding to 0 points
    line_spacing(space = 1, part = "all") %>%  # Set line spacing to single space (1)
    merge_v(j = c("Variable_Group"#, "Scale", "Response"
    )) %>%
    fix_border_issues() %>%
    hline(i = which(bottom_varGroup_borders$is_last),
          border = officer::fp_border(width = 1)) %>%
    flextable::set_header_labels(Variable_Group = "Variable Group",
                      variable_clean_flextable = "Variable Name",
                      flextable_units = "Units") %>%
    set_table_properties(layout = "fixed") %>%
    width(j = 1:5, width = c(0.81, 2.06, 0.81, 0.94, 1.37))
  
  
  return(table1_object)
  
}

