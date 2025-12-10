run_gamm <- function(modeling_vars_final,
                     dat_trans) {

# CREATE SOME GAM MODELS!

# interrogate modeling_vars_final
modeling_vars_final # a list of all possible combinations of predictor groups.

formula_list <- modeling_vars_final %>%
  # add smoother term s() and fit details. 
  # fx = FALSE, k = -1 instructs model to use cross validation to determine amount of smoothing
  # bs=cr means use cubic regression spline
  map(., ~ paste0("s(", ., ", fx = FALSE, k = -1, bs = \"cr\")", collapse = " + ")) %>%
  
  # CAN'T MODIFY CATEGORICAL VARIABLES WITH SMOOTHERS. REMOVE SMOOTHER
  # Note that the regex expressions require \ and \\ to escape the special 
  # meaning of ( and ". These are always confusing.
  # e_i_type
  map(., ~ str_replace_all(
    ., # current list element
    "s\\(e_i_type, fx = FALSE, k = -1, bs = \"cr\"\\)", # replace this. 
    "factor(e_i_type)" # replacement
  )) %>%
  # 
  map(., ~ str_replace_all(
    ., # current list element
    "s\\(ag_eco9_nm, fx = FALSE, k = -1, bs = \"cr\"\\)", # replace this.
    "factor(ag_eco9_nm)" # replacement
  )) %>%
  # Now add response variables
  map(., ~ paste0(c("ch4_total", "ch4_diffusion", "ch4_ebullition"), " ~ ", .)) %>%
  unlist(., use.names = TRUE) %>%  # remove nesting structure, retain names
  #flatten(.) %>%  
  map(., ~ as.formula(.)) # convert to formula

tic() #228 seconds to run all models
gam_object <- map(formula_list, ~ mgcv::gam(., data = dat_trans, family = gaussian()))
toc()

return(gam_object)
}