impute_data <- function(dat_raw, lagos_chem, aquamatch_data,
                        usgs_chem, usgs_chla, williamson_doc,
                        usace_chem, ohio_epa_chem, ohio_wildlife_chem){
  
  # bring in external data sources
  dat_lagos <- left_join(
    lagos_chem, dat_raw, 
    relationship = "many-to-many"
    )
  dat_aquamatch <- left_join(
    aquamatch_data, dat_raw, by = "lake_id", 
    relationship = "many-to-many"
    )
  dat_usgs_chla <- left_join(
    usgs_chla, dat_raw, 
    relationship = "many-to-many"
    )
  dat_usgs_chem <- left_join(
    usgs_chem, dat_raw, 
    relationship = "many-to-many"
    )
  dat_williamson_doc <- left_join(
    williamson_doc, dat_raw, 
    relationship = "many-to-many"
    )
  dat_usace_chem <- left_join(
    usace_chem, dat_raw, 
    relationship = "many-to-many"
    )
  dat_ohio_epa_chem <- left_join(
    ohio_epa_chem, dat_raw, 
    relationship = "many-to-many"
    )
  dat_ohio_wildlife_chem <- left_join(
    ohio_wildlife_chem, dat_raw, 
    relationship = "many-to-many"
  )
  
  # IMPUTE TN-----
  # missing values are from 2022 SuRGE sampling. Replace with
  # NLA22 when available, else NLA17.
  dat_impute <- 
    dat_raw %>%
    mutate(tn_shallow = case_when(
      is.na(tn_shallow) & !is.na(nla22_ntl) ~ nla22_ntl,
      is.na(tn_shallow) & is.na(nla22_ntl) & !is.na(nla17_ntl) ~ nla17_ntl,
      TRUE ~ tn_shallow
    ))
  
  # IMPUTE CHLA----
  # have data for Acton Lake (1001) from Vanni's EDI repo
  # https://portal.edirepository.org/nis/mapbrowse?packageid=edi.256.3
  # input_data/external_data/ActonLakeIntegrated.csv
  dat_impute <- 
    dat_impute %>%
    mutate(chla_lab_shallow = case_when(
      # Acton Lake. 28.98 ug/L from Vanni for 5/31/2016
      lake_id == 1001 ~ 28.98,
      
      # Burr Oak from Tanner Williamson (input_data/external_data/Burr Oak chl data.csv)
      # on 2016-07-16 and 2016-07-23
      # Williamson, T. J. (2020). A multi-scale approach to understanding 
      # how environmental variability modulates structure and function in
      # reservoir ecosystems [Doctoral dissertation, Miami University].
      # OhioLINK Electronic Theses and Dissertations Center.
      # http://rave.ohiolink.edu/etdc/view?acc_num=miami1595521238756147
      lake_id == 1007 ~ mean(23.65, 34.45, 27.70, 10.10, 13.20, 7.50,
                             47.10, 50.05, 32.85, 22.50, 31.90, 28.50),
    )) %>%
    left_join(
      # Need to add the single estimate for Beaver Upground
      # provided by Ohio Division of Wildlife (ODOW). Creating
      # a small df to contain value that will be merged with greater
      # df. Then chla_lab_shallow will remain NA, but will be
      # populated with mean of all available value (ODOW and NLA)
      dat_impute %>%
        filter(lake_id == 204) %>%
        select(lake_id, site_id, visit) %>%
        mutate(odow_chla_shallow = 8.12)
    ) %>%
    left_join(
      dat_lagos %>%
        select(lake_id, lagos_chla_shallow) %>%
        distinct() %>%
        # exclude missing observations
        filter(!is.na(lagos_chla_shallow)) %>%
        # mean by lake
        group_by(lake_id) %>%
        summarize(lagos_chla_shallow = mean(lagos_chla_shallow))
    ) %>%
    left_join(
      dat_aquamatch %>%
        select(lake_id, aquamatch_chla_shallow) %>%
        distinct() %>%
        # exclude missing observations
        filter(!is.na(aquamatch_chla_shallow)) %>%
        # mean by lake
        group_by(lake_id) %>%
        summarize(aquamatch_chla_shallow = mean(aquamatch_chla_shallow))
    ) %>%
    left_join(
      dat_usgs_chla %>%
        select(lake_id, usgs_chla_lab_shallow) %>%
        distinct() %>%
        # exclude missing observations
        filter(!is.na(usgs_chla_lab_shallow)) %>%
        # mean by lake
        group_by(lake_id) %>%
        summarize(usgs_chla_shallow = mean(usgs_chla_lab_shallow))
    ) %>%
    left_join(
      dat_ohio_epa_chem %>%
        select(lake_id, ohio_epa_chla_shallow) %>%
        distinct() %>%
        # exclude missing observations
        filter(!is.na(ohio_epa_chla_shallow)) %>%
        # mean by lake
        group_by(lake_id) %>%
        summarize(ohio_epa_chla_shallow = mean(ohio_epa_chla_shallow))
    ) %>%
    left_join(
      dat_ohio_wildlife_chem %>%
        select(lake_id, ohio_wildlife_chla_shallow) %>%
        distinct() %>%
        # exclude missing observations
        filter(!is.na(ohio_wildlife_chla_shallow)) %>%
        # mean by lake
        group_by(lake_id) %>%
        summarize(ohio_wildlife_chla_shallow = mean(ohio_wildlife_chla_shallow))
    ) %>%
    # replace missing value with mean nla17, nla22, lagos, usgs, odow, ohio epa, 
    # ohio_wildlife. This is a rowwise operation
    rowwise() %>% 
    mutate(
      chla_lab_shallow = case_when(
        is.na(chla_lab_shallow) ~ mean(
          c(nla17_chla, nla22_chla, odow_chla_shallow,
            lagos_chla_shallow, aquamatch_chla_shallow,
            usgs_chla_shallow, ohio_epa_chla_shallow,
            ohio_wildlife_chla_shallow),
          na.rm = TRUE
        ), # close mean
        TRUE ~ chla_lab_shallow)
    ) %>%
    ungroup %>% # remove rowwise
    select(-odow_chla_shallow, -lagos_chla_shallow, 
           -aquamatch_chla_shallow, -usgs_chla_shallow,
           -ohio_epa_chla_shallow, -ohio_wildlife_chla_shallow)
  
  # IMPUTE SULFATE----
  # 1. Create so4 ~ cond model for Northern Appalachians.
  #    Model should be built with original observations, not imputed values.
  # 2. Impute missing values with NLA, LAGOS, USGS data to the maximum extent.
  # 3. Use model to impute remaining missing values for the two ecoregions.
  #    See nla_chem_comparison_report.Rmd for details.
  
  ## Define Models----
  so4_model_nap <- lm(so4_shallow ~ sp_cond_shallow, 
                      data = dat_raw %>%  
                        filter(
                          ag_eco9_nm == "Northern Appalachians"
                        )
  )
  
  
  ## Impute with external data----
  # none of the external data sources are from same year/month as SuRGE
  # observations, so just use mean
  dat_impute <- left_join(
    # 1st df
    dat_impute,
    # bring in lagos data
    dat_lagos %>%
      select(
        lake_id, lagos_so4_shallow
      ) %>%
      distinct() %>%
      # exclude missing observations
      filter(!is.na(lagos_so4_shallow)) %>%
      # mean by lake
      group_by(lake_id) %>%
      summarize(lagos_so4_shallow = mean(lagos_so4_shallow))
  ) %>%
    left_join(
      # Need to add the single estimate for Beaver Upground
      # provided by Ohio Division of Wildlife (ODOW). Creating
      # a small df to contain value that will be merged with greater
      # df. Then so4_shallow will remain NA, but will be
      # populated with mean of all available value (ODOW and NLA)
      dat_impute %>%
        filter(lake_id == 204) %>%
        select(lake_id, site_id, visit) %>%
        mutate(odow_so4_shallow = 22.9)
    ) %>%
    left_join(
      # Need to add the single estimate for Alum Cr. found in 
      # Leslie and Lyons (2013). Creating a small df to contain value
      # that will be merged with greater df. Then so4_shallow will 
      # remain NA, but will be populated with mean of all available values
      # (Leslie and Lyons, USGS)
      dat_impute %>%
        filter(lake_id == 1002) %>%
        select(lake_id, site_id, visit) %>%
        mutate(leslie_lyons_so4_shallow = 52.8)
    ) %>%
    left_join(
      dat_usgs_chem %>%
        select(lake_id, usgs_so4_shallow) %>%
        distinct() %>%
        # exclude missing observations
        filter(!is.na(usgs_so4_shallow)) %>%
        # mean by lake
        group_by(lake_id) %>%
        summarize(usgs_so4_shallow = mean(usgs_so4_shallow)) 
    ) %>%
    left_join(
      dat_usace_chem %>%
        select(lake_id, usace_so4_shallow) %>%
        distinct() %>%
        # exclude missing observations
        filter(!is.na(usace_so4_shallow)) %>%
        # mean by lake
        group_by(lake_id) %>%
        summarize(usace_so4_shallow = mean(usace_so4_shallow)) 
    ) %>%
    left_join(
      dat_ohio_epa_chem %>%
        select(lake_id, ohio_epa_so4_shallow) %>%
        distinct() %>%
        # exclude missing observations
        filter(!is.na(ohio_epa_so4_shallow)) %>%
        # mean by lake
        group_by(lake_id) %>%
        summarize(ohio_epa_so4_shallow = mean(ohio_epa_so4_shallow)) 
    ) %>%
    left_join(
      dat_ohio_wildlife_chem %>%
        select(lake_id, ohio_wildlife_so4_shallow) %>%
        distinct() %>%
        # exclude missing observations
        filter(!is.na(ohio_wildlife_so4_shallow)) %>%
        # mean by lake
        group_by(lake_id) %>%
        summarize(ohio_wildlife_so4_shallow = mean(ohio_wildlife_so4_shallow)) 
    ) %>%
    # replace missing value with mean of all available data
    # this is a rowwise operation
    rowwise() %>% 
    mutate(
      so4_shallow = case_when(
        is.na(so4_shallow) ~ mean(
          c(nla17_sulfate, nla22_sulfate, lagos_so4_shallow,
            usgs_so4_shallow, odow_so4_shallow, leslie_lyons_so4_shallow,
            usace_so4_shallow, ohio_epa_so4_shallow, ohio_wildlife_so4_shallow),
          na.rm = TRUE
        ), # close mean
        TRUE ~ so4_shallow
      )
    ) %>%
    ungroup %>% # remove rowwise
    select(-lagos_so4_shallow, -usgs_so4_shallow, 
           -odow_so4_shallow, -leslie_lyons_so4_shallow,
           -usace_so4_shallow, -ohio_epa_so4_shallow,
           -ohio_wildlife_so4_shallow)
  
  
  ## Impute with model----
  ### Predict missing values for NAP
  pred_so4_nap <- predict(so4_model_nap,
                          newdata = dat_impute %>%
                            filter(
                              ag_eco9_nm == "Northern Appalachians",
                              is.na(so4_shallow) # missing so4
                            )
  ) %>%
    as_tibble() %>% # to tibble
    rename(so4_shallow_predicted = value) %>% # rename parameter
    # bind with unique identifiers
    bind_cols(.,
              dat_impute %>%
                filter(
                  ag_eco9_nm == "Northern Appalachians",
                  is.na(so4_shallow)
                ) %>%
                select(lake_id, site_id, visit)
    )
  
  ### Join predicted data into main dataframe
  dat_impute <- dat_impute %>%
    left_join(pred_so4_nap) %>% # join predictions
    mutate(
      so4_shallow = coalesce( # replace missing with predicted values
        so4_shallow, 
        so4_shallow_predicted
      )
    ) %>%
    select(-so4_shallow_predicted) # remove predictions
  
  # IMPUTE DOC----
  dat_impute <- left_join(
    # 1st df
    dat_impute,
    # join aquamatch data!
    dat_aquamatch %>%
      select(lake_id, activity_start_date, aquamatch_doc_shallow) %>%
      distinct() %>%
      # exclude lake 83 values not wanted. Remaining values
      # are temporally matched (2020-07-27, 2020-08-28) to surge
      # sampling date (2020-08-10)
      filter(
        !(lake_id == 83 & 
            (activity_start_date <= as.Date("2020-06-25") | 
               activity_start_date >= as.Date("2020-09-23")))
      ) %>% # close filter
      # Omit observations from 1970s (all others are > 2009-01-01)
      filter(
        activity_start_date > as.Date("2009-01-01")
      ) %>%
      # calculate mean by lake (collapse time series into one value)
      group_by(lake_id) %>%
      summarize(
        aquamatch_doc_shallow = mean(aquamatch_doc_shallow, na.rm = TRUE)
      ) %>%
      filter(!is.na(aquamatch_doc_shallow), !is.nan(aquamatch_doc_shallow))
  ) %>% # close left_join
    left_join(
      # join lagos data
      dat_lagos %>%
        select(
          lake_id, lagos_doc_shallow
        ) %>%
        distinct() %>%
        # exclude missing observations
        filter(!is.na(lagos_doc_shallow)) %>%
        # mean by lake
        group_by(lake_id) %>%
        summarize(lagos_doc_shallow = mean(lagos_doc_shallow))
    ) %>% # close second join
    left_join(
      # Williamson, C.E. 2023. Limnological data from nearly 400 lakes across
      # the Americas and New Zealand with a focus on vertical profiles of
      # temperature, UV radiation, and optical properties ver 5. Environmental
      # Data Initiative.
      # https://doi.org/10.6073/pasta/40bb7dc870814ae90437875c506527bf
      # (Accessed 2025-09-18).
      williamson_doc %>%
        select(
          lake_id, williamson_doc_shallow
        ) %>%
        distinct() %>%
        # exclude missing observations
        filter(!is.na(williamson_doc_shallow)) %>%
        # mean by lake
        group_by(lake_id) %>%
        summarize(williamson_doc_shallow = mean(williamson_doc_shallow))
    ) %>%
    left_join(
      # join lagos data
      usgs_chem %>%
        select(
          lake_id, usgs_doc_shallow
        ) %>%
        distinct() %>%
        # exclude missing observations
        filter(!is.na(usgs_doc_shallow)) %>%
        # mean by lake
        group_by(lake_id) %>%
        summarize(usgs_doc_shallow = mean(usgs_doc_shallow))
    ) %>%
    left_join(
      # join usace data
      usace_chem %>%
        select(
          lake_id, usace_doc_shallow
        ) %>%
        distinct() %>%
        # exclude missing observations
        filter(!is.na(usace_doc_shallow)) %>%
        # mean by lake
        group_by(lake_id) %>%
        summarize(usace_doc_shallow = mean(usace_doc_shallow))
    ) %>%
    left_join(
      # join Ohio EPA chem
      ohio_epa_chem %>%
        select(
          lake_id, ohio_epa_doc_shallow
        ) %>%
        distinct() %>%
        # exclude missing observations
        filter(!is.na(ohio_epa_doc_shallow)) %>%
        # mean by lake
        group_by(lake_id) %>%
        summarize(ohio_epa_doc_shallow = mean(ohio_epa_doc_shallow))
    ) %>%
    # replace missing value with mean of all available data
    # this is a rowwise operation
    rowwise() %>% 
    mutate(
      doc_shallow = case_when(
        lake_id == 83 ~ aquamatch_doc_shallow, # sampling date match
        is.na(doc_shallow) ~ mean(
          c(nla17_doc, nla22_doc, lagos_doc_shallow, 
            aquamatch_doc_shallow, williamson_doc_shallow,
            usgs_doc_shallow, usace_doc_shallow, ohio_epa_doc_shallow),
          na.rm = TRUE
        ), # close mean
        TRUE ~ doc_shallow
      )
    ) %>%
    ungroup %>% # remove rowwise
    select(-lagos_doc_shallow, -aquamatch_doc_shallow,
           -williamson_doc_shallow, -usgs_doc_shallow,
           -usace_doc_shallow, -ohio_epa_doc_shallow)
  
  # IMPUTE BUOYF----
  # buoyf is the buoyancy frequency, a measure of the strength of thermal
  # stratification. No depth profile collected at lake == 71 (sampled
  # 2021-08-08) due to storms. This lake was sampled on July 23, 2017 for NLA 
  # and none of the variables (temp, DO, pH) showed evidence of stratification.
  # set buoyf to 0 indicating no stratification.
  dat_impute <- dat_impute %>%
    mutate(buoyf = case_when(lake_id == 71 ~ 0,
                             TRUE ~ buoyf))
  
  # IMPUTE TEMPERATURE----
  # Cave Run (1010) site 4 missing shallow temp. Assign mean value from
  # nearby sites
  temp_shallow_1010_4 <- dat_impute %>%
    filter(lake_id == 1010 & site_id %in% c(7, 11, 15)) %>% 
    summarize(s_temp = mean(temp_shallow)) %>% 
    pull()
 
  # Cowan (1012) site 5 missing shallow temp. Assign value from
  # nearby site.
  temp_shallow_1012_5 <- dat_impute %>%
    filter(lake_id == 1012 & site_id == 1) %>% 
    pull(temp_shallow)
  
  # ladue (1018) site 5 missing shallow temp. Assign mean value from
  # nearby site
  temp_shallow_1018_5 <- dat_impute %>%
    filter(lake_id == 1018 & site_id == 1) %>% 
    pull(temp_shallow)
  
  # (70) site 2_lacustrine missing deep temp. Shallow site (1.3m), assign 
  # shallow temp value
  temp_deep_70_2_lacustrine <- dat_impute %>%
    filter(lake_id == 70 & site_id == "2_lacustrine") %>% 
    pull(temp_shallow)
  
  # (75) site 12 missing deep temp. Shallow site (1.2m), assign shallow temp
  temp_deep_75_12 <- dat_impute %>%
    filter(lake_id == 75 & site_id == 12) %>% 
    pull(temp_shallow)
  
  dat_impute <- dat_impute %>%
    mutate(temp_shallow = case_when(lake_id == 1010 & site_id == 4 ~ temp_shallow_1010_4,
                                    lake_id == 1012 & site_id == 5 ~ temp_shallow_1012_5,
                                    lake_id == 1018 & site_id == 5 ~ temp_shallow_1018_5,
                                    TRUE ~ temp_shallow),
           temp_deep = case_when(lake_id == 70 & site_id == "2_lacustrine" ~ temp_deep_70_2_lacustrine,
                                 lake_id == 75 & site_id == 12 ~ temp_deep_75_12,
                                 TRUE ~ temp_deep),
           temp_shallow_flags = case_when(lake_id == 1010 & site_id == 4 ~ "I",
                                          lake_id == 1012 & site_id == 5 ~ "I",
                                          lake_id == 1018 & site_id == 5 ~ "I",
                                          TRUE ~ temp_shallow_flags),
           temp_shallow_units = case_when(lake_id == 1010 & site_id == 4 ~ "c",
                                          lake_id == 1012 & site_id == 5 ~ "c",
                                          lake_id == 1018 & site_id == 5 ~ "c",
                                          TRUE ~ temp_shallow_units),
           temp_deep_flags = case_when(lake_id == 70 & site_id == "2_lacustrine" ~ "I",
                                    lake_id == 75 & site_id == 12 ~ "I",
                                    TRUE ~ temp_deep_flags),
           temp_deep_units = case_when(lake_id == 70 & site_id == "2_lacustrine" ~ "c",
                                       lake_id == 75 & site_id == 12 ~ "c",
                                       TRUE ~ temp_deep_units)
    )
  
  return(dat_impute)
}
