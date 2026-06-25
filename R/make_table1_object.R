make_table1_object <- function(clean_predictor_names_crosswalk){
  
  table1_dat <- tribble(
    ~Variable_Group, ~Variable, ~Model, ~Aggregation, ~Response,
    
    # morphometry
    "morphometry", "buoyf", "local", NA, "diffusion, ebullition, total",
    "morphometry", "site_depth", "local", NA, "diffusion, ebullition, total",
    "morphometry", "circularity", "local, national", NA, "diffusion, ebullition, total",      
    "morphometry", "dynamic_ratio", "local, national", NA, "diffusion, ebullition, total",        
    "morphometry", "littoral_fraction", "local, national", NA, "diffusion, ebullition, total",    
    "morphometry", "max_depth", "local, national", NA, "diffusion, ebullition, total",           
    "morphometry",  "max_length", "local, national", NA, "diffusion, ebullition, total",           
    "morphometry",  "max_width", "local, national", NA, "diffusion, ebullition, total",            
    "morphometry",  "mean_depth", "local, national", NA, "diffusion, ebullition, total",           
    "morphometry",  "mean_width", "local, national", NA, "diffusion, ebullition, total",           
    "morphometry",  "shoreline_development", "local, national", NA, "diffusion, ebullition, total",
    "morphometry", "shoreline_length", "local, national", NA, "diffusion, ebullition, total",     
    "morphometry", "surface_area", "local, national", NA, "diffusion, ebullition, total",        
    "morphometry", "volume", "local, national", NA, "diffusion, ebullition, total",
    
    # climate
    "climate", "temp_deep", "local", NA, "diffusion, ebullition, total",
    "climate", "temp_shallow", "local", NA, "diffusion, ebullition, total",
    "climate", "precipitation_chamber", "local", "hourly value", "diffusion",
    "climate", "precipitation_trap", "local", "cumulative over deployment", "ebullition",
    "climate", "wind_speed_chamber", "local", "hourly value", "diffusion",
    "climate", "wind_speed_trap", "local", "mean hourly value over deployment", "ebullition",
    "climate", "delta_bp_ebu", "local", "slope of pressure change from 3 hours prior through the duration of the deployment", "ebullition",
    "climate", "delta_bp_diff", "local", "slope of change for 24 hours prior to chamber deployment", "diffusion",
    "climate", "shortwave_radiation_sum_mj_m2", "local", "mean daily cumulative radiation", "diffusion",
    "climate", "precip8110cat", "national", "annual mean 1981-2010", "diffusion, ebullition, total",
    "climate", "precip8110ws", "national", "annual mean 1981-2010", "diffusion, ebullition, total",
    "climate", "runoffcat", "national", "annual mean 1900-2008", "diffusion, ebullition, total",
    "climate", "runoffws", "national", "annual mean 1900-2008", "diffusion, ebullition, total",
    "climate", "tmax8110cat", "national", "annual mean 1981-2010", "diffusion, ebullition, total",
    
    
    # trophic
    "trophic", "chla_lab_shallow", "local", NA, "diffusion, ebullition, total",
    "trophic", "no2_3_shallow", "local", NA, "diffusion, ebullition, total",
    "trophic", "tp_shallow", "local", NA, "diffusion, ebullition, total",
    "trophic", "tn_shallow", "local", NA, "diffusion, ebullition, total",
    "trophic", "op_shallow", "local", NA, "diffusion, ebullition, total",
    "trophic", "toc_shallow", "local", NA, "diffusion, ebullition, total",
    "trophic", "chla_sonde_shallow", "local", NA, "diffusion, ebullition, total",
    "trophic", "nla17_sed_c", "local", NA, "diffusion, ebullition, total",
    "trophic", "chl_predicted_sample_month_visit1", "national", "Mean during month of emission rate observation", "diffusion, ebullition, total",
    "trophic", "doc_predicted_sample_month_visit1", "national", "Mean during month of emission rate observation", "diffusion, ebullition, total",
    
    # hydrology
    "hydrology", "e_i", "local", NA, "diffusion, ebullition, total",
    "hydrology", "residence_time", "local", NA, "diffusion, ebullition, total",
    
    # landscape
    "landscape", "wsareasqkm", "local, national", NA, "diffusion, ebullition, total",
    "landscape", "rda", "local, national", NA, "diffusion, ebullition, total",
    "landscape", "kffactcat", "national",  NA, "diffusion, ebullition, total",
    "landscape", "omcat", "national",  NA, "diffusion, ebullition, total",
    "landscape", "scat", "national",  NA, "diffusion, ebullition, total",
    "landscape", "pctcrop2019cat", "national",  NA, "diffusion, ebullition, total",
    "landscape", "wsareasqkm", "national",  NA, "diffusion, ebullition, total",
    "landscape", "nitrogencat", "national",  NA, "diffusion, ebullition, total",
    "landscape", "agkffactcat", "national",  NA, "diffusion, ebullition, total",
    "landscape", "elevcat", "national",  NA, "diffusion, ebullition, total",
    
    # chemistry
    "chemistry", "nh4_shallow", "local", NA, "diffusion, ebullition, total",
    "chemistry", "so4_shallow", "local", NA, "diffusion, ebullition, total",
    #"chemistry", "toc_shallow", "local", NA, "diffusion, ebullition, total",
    "chemistry", "do_deep", "local", NA, "diffusion, ebullition, total",
    "chemistry", "turbidity_shallow", "local", NA, "diffusion, ebullition, total",
    
    # other
    "other", "age", "local", NA, "diffusion, ebullition, total",
    "other", "ag_eco9_nm", "local, national", NA, "diffusion, ebullition, total",
    "other", "doy", "local", NA, "diffusion, ebullition, total"
  ) %>%
    left_join(clean_predictor_names_crosswalk,
              by = c("Variable" = "variable")) %>%
    mutate(variable_clean_flextable = str_replace_all(variable_clean, pattern = "~", replacement = " "),
           variable_clean_flextable = case_when(Variable == "nh4_shallow" ~ "NH~4~^+^ (shallow)",
                                                Variable == "so4_shallow" ~ "SO~4~^2-^ (shallow)",
                                                TRUE ~ variable_clean_flextable),
           Variable_Group = factor(Variable_Group, levels = c("morphometry", "climate", "trophic",
                                                              "hydrology", "landscape", "chemistry", "other")),
           Response = factor(Response, levels = c("diffusion, ebullition, total",
                                                  "diffusion", "ebullition"))) %>%
    filter(!is.na(variable_clean)) %>%
    select(Variable_Group, variable_clean_flextable, flextable_units, 
           Model, Aggregation, Response) %>%
    arrange(Variable_Group, Model, Response, variable_clean_flextable)
  
  
  
  bottom_varGroup_borders <- table1_dat %>%
    mutate(row_id = row_number()) %>%
    group_by(Variable_Group) %>%
    mutate(is_last = row_id == max(row_id)) %>%
    ungroup()
  
  
  
  table1_object <- flextable(table1_dat) %>%
    ftExtra::colformat_md() %>%
    theme_booktabs() %>%
    merge_v(j = c("Variable_Group"#, "Scale", "Response"
    )) %>%
    fix_border_issues() %>%
    hline(i = which(bottom_varGroup_borders$is_last),
          border = officer::fp_border(width = 1)) %>%
    flextable::set_header_labels(Variable_Group = "Variable Group",
                      variable_clean_flextable = "Variable Name",
                      flextable_units = "Units") %>%
    set_table_properties(layout = "fixed") %>%
    flextable::width(j = 1:6, width = c(0.81, 2.06, 0.81, 0.94, 1.37, 1.37)) %>%
    flextable::footnote(i = 8:14, j = 2, 
                        ref_symbols = "1", part = "body", 
             value = as_paragraph("Modeled at hourly timestep.")) %>%
    flextable::footnote(i = 15:16, j = 2, 
                        ref_symbols = "2", part = "body", 
                        value = as_paragraph("Modeled at daily timestep.")) %>%
    flextable::footnote(i = 17, j = 2, 
                        ref_symbols = "3", part = "body", 
                        value = as_paragraph("Modeled at monthly timestep.")) %>%
    flextable::footnote(i = c(21,27,28), j = 2, 
                        ref_symbols = "4", part = "body", 
                        value = as_paragraph("Measured during 2017 National Lakes Assessment.")) %>%
    flextable::footnote(i = 25:26, j = 2, 
                        ref_symbols = "5", part = "body", 
                        value = as_paragraph("Number of observations per month vary across reservoirs depending on satellite flyover frequency and cloud cover during flyover.")) %>%
    fontsize(size = 7.5, part = "all") %>%
    padding(padding = 0, part = "all") %>% # Set all padding to 0 points
    line_spacing(space = 1, part = "all")  # Set line spacing to single space (1)
  
  
  return(table1_object)
  
}

