get_data_file_paths <- function(parent_data_path) {
  fs::dir_ls(parent_data_path, 
             regexp = "\\.csv$", 
             recurse = FALSE, 
             type = "file") %>%
    .[!grepl("dictionary",.)] # exclude data dictionary files
}