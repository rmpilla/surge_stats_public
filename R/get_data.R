# DATA RETRIEVED FROM BEAULIEU ET AL. SuRGE DATA PAPER EDI REPOSITORY
# [retrieval date: ]

# 7/30/2025 reading copies of files pulled from data_paper
# branch of SuRGE repo (https://github.com/USEPA/SuRGE).
# Will replace with files for EDI repo when paper is complete.

# Read .csv data files
get_data <- function(data_file_paths){
  data_file_paths %>%
    purrr::map(~readr::read_csv(.x, 
                                guess_max = 5000) # ensure correct column type
               )
}
