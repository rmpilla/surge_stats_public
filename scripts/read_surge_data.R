# DATA WILL BE RETRIEVED AND READ FROM EDI REPOSITORY
# FOR BEAULIEU ET AL. SuRGE DATA PAPER.

# 7/30/2025 reading copies of files pulled from data_paper
# branch of SuRGE repo (https://github.com/USEPA/SuRGE).
# Will replace with files for EDI repo when paper is complete.

# Read .csv data files
get_data <- function(paths){
fs::dir_ls(path = paths, # "input_data"
           regexp = ".csv") %>% # file names containing this pattern
  .[!grepl("dictionary", .)] %>% # exclude data dictionaries
    # read each file in fs_path list generated above
    purrr::map(~readr::read_csv(.x)) 
                  
}
