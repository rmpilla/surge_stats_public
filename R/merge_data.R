# CODE TO MERGE THE SEPARATE FILES FROM THE DATA PAPER
# INTO A SINGLE DATAFRAME IN WIDE FORMAT. THIS ESSENTIALLY
# REPRODUCES THE `dat` OBJECT FROM THE SuRGE REPOSITORY

merge_data <- function(
    site_descriptors_data,
    emission_rate_points_data,
    emissions_lake_data,
    site_data,
    lake_scale_data,
    phyto_data) {
  # pivot from long to wide where needed----
  ## site_data----
  ### pivot----
  site_data_w <- site_data %>%
    select(-mdl) %>% # don't need minimum detection limit
    tidyr::pivot_wider(
      names_from = c(name, sample_depth),
      names_glue = "{name}_{sample_depth}_{.value}",
      values_from = c(value, flags, units, comment)
    ) %>%
    # 411 columns, many are "_comments" columns that are all NA
    # and can be removed. There are likely "_flag" columns with
    # all NA values but we want to keep those.

    # exclude any column that has "comment" in the name &
    # all values are NA.
    select(!(contains("comment") & where(~ all(is.na(.))))) %>%
    # remove "_value" from column names
    rename_with(
      ~ gsub("_value", "", .x), # replace "_value" with ""
      ends_with("_value") # in columns that end with "_value"
    )

  ### fill chemistry----
  # vector of analytes to fill
  # expand.grid to create all combinations of two vectors
  analytes_to_fill <- expand.grid(c("_deep", "_shallow"),
    c(
      "nh4", "no2_3", "no2", "tn", "tp", "op", # nutrients
      "f", "cl", "br", "so4", # anions
      "doc", "toc", # organics
      "al", "as", "ba", "be", "ca", # metals
      "cd", "cr", "cu", "fe",
      "k", "li", "mg", "mn", "na",
      "ni", "pb", "p", "sb", "si",
      "sn", "sr", "s", "v", "zn"
    ),
    stringsAsFactors = FALSE
  ) %>%
    mutate(analytes_to_fill = paste0(Var2, Var1)) %>% # paste
    pull(analytes_to_fill) %>% # extract to vector
    c(., "chla_lab_shallow") # add lab algae (only shallow)

  # Fill
  site_data_w <- site_data_w %>%
    group_by(lake_id) %>% # for each lake
    # 2016 data have shallow chemistry at two sites but no deep chemistry. SuRGE
    # has deep and shallow at one site. For SuRGE lakes, 'fill` will populate all
    # NAs with the one observed value. For 2016 sites, values are filled by site_id,
    # so site_id values close the shallow site get the shallow site number and
    # id's close to the deep site number get that number.
    fill(all_of(analytes_to_fill), .direction = "downup") %>%
    ungroup()

  ### dups----
  dim(site_data_w) # 2365
  site_data_w %>%
    select(lake_id) %>%
    summarize(n = n_distinct(.)) # 146 lakes

  site_data_w %>% # none
    select(lake_id, site_id, visit) %>%
    janitor::get_dupes(.)

  ## lake_scale_data----
  ### pivot----
  lake_scale_data_w <- lake_scale_data %>%
    filter(!name %in% c(
      "nla17_site_id", "nla12_site_id", "nla07_site_id",
      "nhdplus_comid", "gnis_id", "lagoslakeid",
      "hylak_id", "grand_id", "nid_id", "nla22_site_id"
    )) %>%
    # pivot wider
    tidyr::pivot_wider(
      names_from = name,
      names_glue = "{name}_{.value}",
      values_from = c(value, units)
    ) %>%
    # remove "_value" from column names
    rename_with(
      ~ gsub("_value", "", .x), # replace "_value" with ""
      ends_with("_value") # in columns that end with "_value"
    )

  ### dups----
  dim(lake_scale_data_w) # 146
  lake_scale_data_w %>%
    select(lake_id) %>%
    summarize(n = n_distinct(.)) # 146 lakes

  lake_scale_data_w %>% # none
    select(lake_id) %>%
    janitor::get_dupes(.) %>%
    distinct()

  ## emissions_lake_data----
  ### main issue here is that we have values for each habitat type in
  ### lakes 69 and 70 (riverine, lacustrine, transitional). If we join
  ### on lake_id and visit, this creates duplicate rows for these lakes.
  ### lets address this by adding site_id for these observations.
  emissions_lake_data_missouri <- emissions_lake_data %>%
    filter(lake_id %in% c(69, 70)) %>%
    select(
      lake_id, visit, buoyf, buoyf_units,
      thermdep2, thermdep2_units
    ) %>%
    # add site_id for these observations
    mutate(row_id = row_number()) %>% # needed to distinguish records
    mutate(site_id = case_when(
      lake_id == 69 & is.na(thermdep2) & row_id == 1 ~ "12_transitional",
      lake_id == 69 & is.na(thermdep2) & row_id == 2 ~ "7_riverine",
      lake_id == 69 & !is.na(thermdep2) ~ "4_lacustrine",
      lake_id == 70 & is.na(thermdep2) ~ "6_riverine",
      lake_id == 70 & thermdep2 > 9 & thermdep2 < 10 ~ "7_transitional",
      lake_id == 70 & thermdep2 > 15 ~ "6_lacustrine",
      TRUE ~ "FLY YOU FOOLS!"
    )) %>% # ERROR CODE
    select(-row_id) %>% # remove these columns
    # bring in all site_id values for these lakes. Needed to fill values
    # by lake and reservoir zone
    full_join(
      site_descriptors_data %>%
        filter(lake_id %in% c(69, 70)) %>%
        select(lake_id, site_id, visit)
    ) %>%
    # create lake x zone grouping variable. paste lake_id and
    # all characters after _ in site_id
    mutate(group = paste(lake_id,
      sub(".*_", "", site_id),
      sep = "_"
    )) %>%
    arrange(group) %>%
    # fill by grouping variable
    group_by(group) %>%
    tidyr::fill(buoyf, buoyf_units, thermdep2, thermdep2_units,
      .direction = "updown"
    )

  ## phyto_data----
  ### aggregate----
  # need to aggregate taxa specific numbers
  phyto_data_agg <- phyto_data %>%
    group_by(lake_id, visit) %>%
    summarize(
      algal_density = sum(density),
      density_units = "cells_ml",
      cyano_density = sum(density[algal_group == "CYANOBACTERIA"])
    )


  # merge the dataframes----
  ## objects with lake_id, site_id, and visit first----
  dat <- dplyr::full_join(site_descriptors_data,
    emission_rate_points_data,
    # will naturally join on these, specify for clarity
    by = c("lake_id", "site_id", "visit")
  ) %>%
    dplyr::full_join(site_data_w,
      # will naturally join on these, specify for clarity
      by = c("lake_id", "site_id", "visit")
    ) %>%
    # added site_id to Missouri river stratification data (see above)
    dplyr::full_join(emissions_lake_data_missouri,
      # will naturally join on these, specify for clarity
      by = c("lake_id", "site_id", "visit")
    ) %>%
    ## now join with objects containing lake_id and visit----
    # stratification data for all lakes except 69 and 70.
    # the columns were created when the missouri river data was joined above,
    # just update the values for all other lakes (currently NA)
    dplyr::rows_update(
      emissions_lake_data %>%
        filter(!(lake_id %in% c(69, 70))) %>%
        select(
          lake_id, visit, buoyf, buoyf_units,
          thermdep2, thermdep2_units
        ),
      by = c("lake_id", "visit")
    ) %>%
    # phyto data next. Eliminated site_id field so join on lake_id
    # and visit. This eliminates the need to fill values across site_id.
    dplyr::full_join(phyto_data_agg,
      # will naturally join on these, specify for clarity
      by = c("lake_id", "visit")
    ) %>%
    ## now join with objects containing lake_id only----
    full_join(lake_scale_data_w)




  # inspect dat object
  if (dat %>%
    get_dupes(lake_id, site_id, visit) %>%
    nrow() > 0) {
    print("There are duplicate rows in the dat object. Check the merge_data function.")
  }
  # no dupes
  if (dat %>% distinct(lake_id) %>% nrow() != 148) {
    print("There are not 148 lakes in the dat object. Check the merge_data function.")
  } # 146 lakes


  # return the merged dataframe
  return(dat)
}







