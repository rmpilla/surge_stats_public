# FIT SPATIAL NESTED RANDOM FOREST FROM FINAL MODELING VARS

run_random_forest <- function(dat_trans,
                              modeling_vars_final){
  
  # transform response variables given skewness
  dat_trans$l_ch4_total = log1p(dat_trans$ch4_total)
  dat_trans$l_ch4_diffusion = log1p(dat_trans$ch4_diffusion)
  dat_trans$l_ch4_ebullition = log1p(dat_trans$ch4_ebullition)
  
  formula_list <- modeling_vars_final %>%
    # Combine model variables into string with "+"
    map(., ~ paste0(., collapse = " + ")) %>%
    
    # Now add response variables -- use log-transformed version
    map(., ~ paste0(c("l_ch4_total", "l_ch4_diffusion", "l_ch4_ebullition"), " ~ ", .)) %>%
    unlist(., use.names = TRUE) %>%  # remove nesting structure, retain names
    #flatten(.) %>%  
    map(., ~ as.formula(.)) # convert to formula
  
  
  tic() ## ~20 mins to run all random forest models (!!!)
  random_forest_object <- map(formula_list, ~ spmodel::splmRF(., 
                                                              data = dat_trans, 
                                                              random = ~ (1 | as.character(lake_id)),
                                                              xcoord = long, ycoord = lat,
                                                              spcov_type = "exponential",
                                                              quantreg = T,
                                                              keep.inbag = T,
                                                              importance = "impurity"))
  toc()
  
  return(random_forest_object)
  
}
