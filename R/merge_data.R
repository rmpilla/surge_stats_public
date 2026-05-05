# CODE TO MERGE THE SEPARATE FILES FROM THE DATA PAPER
# INTO A SINGLE DATAFRAME IN WIDE FORMAT. THIS ESSENTIALLY
# REPRODUCES THE `dat` OBJECT FROM THE SuRGE REPOSITORY

merge_data <- function(
    site_descriptors_data,
    emission_rate_points_data,
    emissions_lake_data,
    site_data,
    lake_scale_data,
    phyto_data,
    missouri_river_strat_data,
    lake_cat_data,
    nla_chem,
    bp_data,
    national_morpho_data,
    solar_data) {
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
  analytes_to_fill <- expand_grid(
    var1 = c("_deep", "_shallow"),
    var2 = c(
      "nh4", "no2_3", "no2", "tn", "tp", "op", # nutrients
      "f", "cl", "br", "so4", # anions
      "doc", "toc", # organics
      "al", "as", "ba", "be", "ca", # metals
      "cd", "cr", "cu", "fe",
      "k", "li", "mg", "mn", "na",
      "ni", "pb", "p", "sb", "si",
      "sn", "sr", "s", "v", "zn"
    )
  ) %>%
    # combine analyte and depth
    mutate(analytes_to_fill = paste0(var2, var1)) %>%
    # extract to vector fro next expand_grid
    pull(analytes_to_fill) %>%
    # create flags and units variables
    expand_grid(
      var2 = .,
      var1 = c("_flags", "_units"),
    ) %>%
    # combine analyte_depth with flag or units
    mutate(var3 = paste0(var2, var1)) %>%
    select(var2, var3) %>%
    # collapse to one column
    pivot_longer(everything(),
      names_to = "original_column",
      values_to = "combined_values"
    ) %>%
    select(combined_values) %>%
    # sort for inspection
    arrange(combined_values) %>%
    # remove duplicates
    distinct(combined_values) %>%
    # add lab algae (only shallow)
    bind_rows(
      .,
      tibble(combined_values = c(
        "chla_lab_shallow",
        "chla_lab_shallow_flags",
        "chla_lab_shallow_units"
      ))
    ) %>%
    pull() # extract to vector


  # Fill
  site_data_w <- site_data_w %>%
    group_by(lake_id, visit) %>% # for each lake x visit combination
    # 2016 data have shallow chemistry at two sites but no deep chemistry. SuRGE
    # has deep and shallow at one site. For SuRGE lakes, 'fill` will populate all
    # NAs with the one observed value. For 2016 sites, values are filled by site_id,
    # so site_id values close the shallow site get the shallow site number and
    # id's close to the deep site number get that number.
    fill(all_of(analytes_to_fill), .direction = "downup") %>%
    ungroup()

  ### dups----
  dim(site_data_w) # 2367
  site_data_w %>%
    select(lake_id) %>%
    summarize(n = n_distinct(.)) # 146 lakes

  site_data_w %>% # none
    select(lake_id, site_id, visit) %>%
    janitor::get_dupes(.)

  ## lake_scale_data----
  ### pivot----
  lake_scale_data_w <- lake_scale_data %>%
    # retain nla17_site_id, nla22_site_id, and lagoslakeid
    filter(!name %in% c(
      "nla12_site_id", "nla07_site_id",
      "nhdplus_comid", "gnis_id",
      "hylak_id", "grand_id", "nid_id"
    )) %>%
    # must deal with lakes 231 and 69 that have two lagoslakeid values
    filter(
      # 69 (Oahe) includes two large lagos polygons. Omit both.
      !(lake_id == 69 & name == "lagoslakeid"),
      # 231 has one small and one large lagos polygon. Omit small.
      !(lake_id == 231 & name == "lagoslakeid" & value == 351297)
    ) %>%
    # pivot wider
    tidyr::pivot_wider(
      names_from = name,
      names_glue = "{name}_{.value}",
      values_from = c(value, units)
    ) %>%
    mutate(across(
      !c(
        contains("units"), e_i_type_value,
        ag_eco9_value, ag_eco9_nm_value,
        depth_cat_value, chla_cat_value,
        lab_value, study_value, nla17_site_id_value, nla22_site_id_value
      ),
      as.numeric
    )) %>% # convert to numeric
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
  dat_raw <- dplyr::full_join(
    site_descriptors_data,
    emission_rate_points_data,
    # will naturally join on these, specify for clarity
    by = c("lake_id", "site_id", "visit")
  ) %>%
    dplyr::full_join(
      site_data_w,
      # will naturally join on these, specify for clarity
      by = c("lake_id", "site_id", "visit")
    ) %>%
    dplyr::full_join(
      bp_data,
      # will naturally join on these, specify for clarity
      by = c("lake_id", "site_id", "visit")
    ) %>%
    dplyr::full_join(
      solar_data,
      # will naturally join on these, specify for clarity
      by = c("lake_id", "site_id", "visit")
    ) %>%
    # site/zone specific Missouri river stratification data (see above)
    dplyr::full_join(
      missouri_river_strat_data,
      # will naturally join on these, specify for clarity
      by = c("lake_id", "site_id", "visit")
    ) %>%
    # now fill stratification data for 69 and 70 by zone
    mutate(
      # define zones for grouping
      zone = case_when(
        grepl("riverine", site_id) ~ "riverine",
        grepl("transitional", site_id) ~ "transitional",
        grepl("lacustrine", site_id) ~ "lacustrine",
        TRUE ~ "placeholder"
      )
    ) %>%
    group_by(lake_id, zone, visit) %>%
    fill(contains(c("buoyf", "thermdep2")), .direction = "downup") %>%
    ungroup %>%
    select(-zone) %>% # no longer needed
    
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
  full_join(lake_scale_data_w) %>%
    full_join(lake_cat_data) %>%
    left_join(nla_chem$nla17_chem) %>%
    left_join(nla_chem$nla22_chem) %>%
    left_join(national_morpho_data) %>%
    ## derived quantities----
  # add sampling month, day of year (DOY), rda, and age
  mutate(
    sample_month = format(sample_start, "%B"),
    doy = lubridate::yday(sample_start),
    rda = wsareasqkm / (surface_area / 1e6), # m2 -> km2
    age = as.numeric(substr(sample_start, 1, 4)) - year_completed
  )
  

  # inspect dat object
  if (dat_raw %>%
    janitor::get_dupes(lake_id, site_id, visit) %>%
    nrow() > 0) {
    print("There are duplicate rows in the dat object. Check the merge_data function.")
  }
  # no dupes
  if (dat_raw %>% distinct(lake_id) %>% nrow() != 146) {
    print("There are not 146 lakes in the dat object. Check the merge_data function.")
  } # 146 lakes


  # return the merged dataframe
  return(dat_raw)

}
