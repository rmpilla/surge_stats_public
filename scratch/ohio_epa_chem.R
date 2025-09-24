# load("input_data/ohio_epa_lake_data.rds")
ohio_epa_lake_data <- readRDS("input_data/external_data/ohio_epa_lake_data.rds") %>%
  as_tibble()

ohio_epa_lake_data %>%
  filter(
    grepl(c("kiser|knox|cowan|logan|milton|ladue|la due|waynoka|roaming|mohawk|apple"), station, ignore.case = TRUE),
    parameter %in% c("chla", "Depth", "so4", "doc", 'toc')
  ) %>%
  distinct(station)
  #filter(parameter == "chla") %>%
  #filter(parameter == "so4") %>%
  filter(parameter == "doc") %>%
  mutate(result = as.numeric(result)) %>%
  ggplot(aes(yr, result)) +
  geom_point() +
  facet_wrap(~station, scales = "free")
  distinct(station)
unique(ohio_epa_lake_data$station)
