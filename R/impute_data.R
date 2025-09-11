impute_data <- function(dat_raw, lagos_chem, aquamatch_data){
  
  # bring in LAGOS and aquamatch data
  dat_lagos <- left_join(lagos_chem, 
                         dat_raw, 
                         relationship = "many-to-many")
  dat_aquamatch <- left_join(aquamatch_data, 
                             dat_raw, 
                             by = "lake_id", 
                             relationship = "many-to-many")
  
  
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
  # only have data for Acton Lake (1001) from Vanni's EDI repo
  # https://portal.edirepository.org/nis/mapbrowse?packageid=edi.256.3
  # input_data/ActonLakeIntegrated.csv
  dat_impute <- 
    dat_impute %>%
    mutate(chla_lab_shallow = case_when(
      lake_id == 1001 ~ 28.98, # 28.98 ug/L from Vanni for 5/31/2016
      TRUE ~ chla_lab_shallow
    ))
  
  # IMPUTE SULFATE----
  # 1. Create so4 ~ cond model for Northern Appalachians.
  #    Model should be built with original observations, not imputed values.
  # 2. Impute missing values with NLA and LAGOS data to the maximum extent.
  # 3. Use model to impute remaining missing values for the two ecoregions.
  #    See nla_chem_comparison_report.Rmd for details.
  
  ## Define Models----
  so4_model_nap <- lm(so4_shallow ~ sp_cond_shallow, 
                      data = dat_impute %>%  
                        filter(
                          ag_eco9_nm == "Northern Appalachians"
                        )
  )

  
  
  ## Impute with lagos and/or NLA----
  # none of the lagos or NLA data are from same year as SuRGE
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
    # replace missing value with mean nla17, nla22, and lagos sulfate by lake.
    # this is a rowwise operation
    rowwise() %>% 
    mutate(
      so4_shallow = case_when(
        is.na(so4_shallow) ~ mean(
          c(nla17_sulfate, nla22_sulfate, lagos_so4_shallow),
          na.rm = TRUE
        ), # close mean
        TRUE ~ so4_shallow
      )
    ) %>%
    ungroup %>% # remove rowwise
    select(-lagos_so4_shallow)
  
  
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
        activity_start_date < as.Date("2009-01-01")
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
    # replace missing value with mean nla17, nla22, lagos, and aquamatch
    # DOC by lake.
    # this is a rowwise operation
    rowwise() %>% 
    mutate(
      doc_shallow = case_when(
        lake_id == 83 ~ aquamatch_doc_shallow, # sampling date match
        is.na(doc_shallow) ~ mean(
          c(nla17_doc, nla22_doc, lagos_doc_shallow, aquamatch_doc_shallow),
          na.rm = TRUE
        ), # close mean
        TRUE ~ so4_shallow
      )
    ) %>%
    ungroup %>% # remove rowwise
    select(-lagos_doc_shallow, -aquamatch_doc_shallow)
  
  
  return(dat_impute)
}
