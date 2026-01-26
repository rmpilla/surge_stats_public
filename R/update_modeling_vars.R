# FUNCTION TO UPDATE LIST OF MODELING VARS TO REFLECT
# THOSE THAT HAVE BEEN TRANSFORMED. CODE ASSUMES
# NAMES OF ALL TRANSFORMED VARS HAVE BEEN APPENDED
# WITH "l_" PREFIX, INDICATING LOG TRANSFORMATION


update_modeling_vars <- function(dat_trans, modeling_vars) {
  
  # create vector of variables that have been transformed
  vars_update <- dat_trans %>% 
    colnames() %>% # colnames of main dataframe
    .[startsWith(., "l_")] %>% # select names with "l_" prefix
    sub(paste0("^", "l_"), "", .) # eliminate "l_" prefix
  
  # update list of vars to reflect those that have been transformed
  # map_depth applies function recursively to specified depth of
  # list. Our list has two levels.
  modeling_vars_update <- map_depth(modeling_vars, .depth = 2, function(x) {
    ifelse(x %in% vars_update, # if var needs to be updated
           paste0("l_", x), # then add suffix
           x) # else don't change
  }
  )
  
  return(modeling_vars_update)
}