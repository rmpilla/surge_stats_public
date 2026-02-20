get_solar_data <- function(solar_data_file){
  solar_data <- read_csv(solar_data_file)
  
  return(solar_data)
}