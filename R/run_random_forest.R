# FIT SPATIAL NESTED RANDOM FOREST FROM FINAL MODELING VARS

run_random_forest <- function(dat_trans,
                              modeling_vars_final){
  
  modeling_vars_untrans = modeling_vars_final %>%
    unlist() %>%
    gsub(pattern = "^l_", replacement = "") %>%
    relist(modeling_vars_final)
  
  # transform response variables given skewness
  dat_trans$l_ch4_total = log1p(dat_trans$ch4_total)
  dat_trans$l_ch4_diffusion = log1p(dat_trans$ch4_diffusion)
  dat_trans$l_ch4_ebullition = log1p(dat_trans$ch4_ebullition)
  
  # project to UTM
  dat_trans <- st_as_sf(
    dat_trans,
    coords = c("long", "lat"),
    crs = 4326
  ) %>%
    st_transform(crs = 5070) # Albers equal area conus
  
  formula_list <- modeling_vars_untrans %>%
    # Combine model variables into string with "+"
    # map_depth applies function to specified level of nested list
    map_depth(., .depth = 2, ~ paste0(., collapse = " + ")) %>%
    
    # simplify list structure
    unlist(., recursive = FALSE) %>%
    
    # Now add response variables -- use log-transformed version
    # within an imap function, .y is the name of the list element
    imap(., ~ if (grepl("diffusion", .y)) { # if element name contains diffusion
      paste0("l_ch4_diffusion ~ ", .) # then paste diffusion as response
    } else if (grepl("ebullition", .y)) {
      paste0("l_ch4_ebullition ~ ", .)
    } else if (grepl("total", .y)) {
      paste0("l_ch4_total ~ ", .)
    } 
    ) %>%
    map(., ~ as.formula(.)) # convert to formula
  
  
  tic() ## ~20 mins to run all random forest models (!!!)
  
  set.seed(102322)
  random_forest_object <- map(formula_list, ~ spmodel::splmRF(., 
                                                              data = dat_trans, 
                                                              random = ~ (1 | lake_id) + (1 | sample_month),
                                                              spcov_type = "exponential", # coords inherited from sf object
                                                              quantreg = T,
                                                              keep.inbag = T,
                                                              na.action = "na.omit",
                                                              importance = "permutation",
                                                              local = TRUE # faster performance, omit for final model
                                                              ) 
  )
  
  
  
  
  toc()
  
  return(random_forest_object)
  
}