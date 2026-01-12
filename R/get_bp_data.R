## Developing indices for barometric pressure for both diffusive and ebullitive emissions

get_bp_data <- function(bp_data_file, emission_rate_points_data) {
  dt <- emission_rate_points_data %>%
    mutate(ID = paste(lake_id, site_id, visit)) %>%
    select(ID, chamb_deply_date_time, trap_deply_date_time)

  bp <- read_csv(bp_data_file) %>%
    mutate(ID = paste(lake_id, site_id, visit))

  bp_join <- left_join(bp, dt, by = "ID")

  bp_chamber <- bp_join %>%
    mutate(chamb_deply_round = round_date(chamb_deply_date_time, "hour")) %>%
    filter(date_time <= chamb_deply_round)

  # write a loop to create linear models for each chamber deployment
  chamberID <- unique(bp_chamber$ID)
  delta_pressure <- NULL

  for (i in 1:length(chamberID)) {
    bp_c <- bp_chamber %>%
      filter(ID == chamberID[i])
    a <- lm(bp_c$barometric_pressure ~ bp_c$date_time)
    sloperr <- summary(a)$coef[[4]]
    slope <- coef(a)[["bp_c$date_time"]]
    delta_pressure[i] <- ifelse(abs(slope) > sloperr, slope, 0)
  }

  bp_chamber_d <- data.frame(chamberID, delta_pressure)
  colnames(bp_chamber_d) <- c("ID", "delta_bp_diff")

  bp_funnel <- bp_join %>%
    mutate(trap_deply_round = round_date(trap_deply_date_time, "hour") - hours(3)) %>%
    filter(date_time >= trap_deply_round)

  trapID <- unique(bp_funnel$ID)
  delta_tpressure <- NULL

  for (i in 1:length(trapID)) {
    bp_t <- bp_funnel %>%
      filter(ID == trapID[i])
    a <- lm(bp_t$barometric_pressure ~ bp_t$date_time)
    sloperr <- summary(a)$coef[[4]]
    slope <- coef(a)[["bp_t$date_time"]]
    delta_tpressure[i] <- ifelse(abs(slope) > sloperr, slope, 0)
  }

  bp_funnel_d <- data.frame(trapID, delta_tpressure)
  colnames(bp_funnel_d) <- c("ID", "delta_bp_ebu")

  j <- full_join(bp_funnel_d, bp_chamber_d, by = "ID")
  match <- bp %>%
    group_by(ID) %>%
    summarise(lake_id = lake_id[1], site_id = site_id[1], visit = visit[1])

  bp_data <- left_join(j, match, by = "ID") %>%
    select(-"ID") %>%
    janitor::clean_names()

  # tar_load_everything()


  return(bp_data)
}

# SCRIPT FOR MINIMUM AND TOTAL CHANGE IN PRESSURE
# bp_chamber_min <- bp_join %>%
#   mutate(chamb_deply_round = round_date(chamb_deply_date_time, "hour")) %>%
#   filter(date_time <= chamb_deply_round) %>%
#   group_by(ID) %>%
#   summarise(
#     min_bp_diff = min(barometric_pressure),
#     chg_bp_diff = max(barometric_pressure) - min(barometric_pressure),
#     lake_id = lake_id[1], site_id = site_id[1], visit = visit[1], ID = ID[1]
#   )
#
# bp_funnel_min <- bp_join %>%
#   mutate(trap_deply_round = round_date(trap_deply_date_time, "hour") - hours(3)) %>%
#   filter(date_time >= trap_deply_round) %>%
#   group_by(ID) %>%
#   summarise(
#     min_bp_ebu = min(barometric_pressure),
#     chg_bp_ebu = max(barometric_pressure) - min(barometric_pressure),
#     lake_id = lake_id[1], site_id = site_id[1], visit = visit[1], ID = ID[1]
#   )

 
