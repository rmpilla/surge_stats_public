# Read NLA data

get_nla_chem <- function(nla17_water_chem_file,
                         nla17_sed_chem_file,
                         nla22_water_chem_file) {
  
  # NLA17 Chemistry----
  nla17_water_chem <- readr::read_csv(nla17_water_chem_file,
                                      na = c("", " "),
                                      show_col_types = FALSE) %>%
    # a few values in result column are character (i.e. < 0.03)
    # these are converted to numeric (NA) by read_csv with a 
    # warning. none of these cases are for SuRGE lakes, so suppressing
    # warning so as not to bring attention to a non-issue.
    suppressWarnings() %>%
    janitor::clean_names() %>%
    select(site_id, visit_no, analyte, result, result_units) %>% # select needed values
    dplyr::rename_with(~paste0("nla17_", .)) %>% # append nla17_ to all columns names
    # missing units on this observation causing problems when pivoting to wide
    # replace NA for units with correct units
    mutate(across(nla17_result_units, 
                  ~replace(
                    ., 
                    nla17_site_id == "NLA17_WI-10001" & nla17_analyte == "ALUMINUM", 
                    "MG/L"
                  )
    )) %>%
    # Need to aggregate across visits 1 and 2.  Group by nla_id and analyte
    dplyr::group_by(nla17_site_id, nla17_analyte) %>% # group to aggregate visits 1 and 2
    summarize(nla17_result = mean(nla17_result, na.rm = TRUE), # aggregate results across visits
              nla17_result_units = unique(nla17_result_units)) %>% # retain units
    mutate(across(nla17_result, ~replace(., is.nan(.), NA))) %>%
    rename(nla17_units = nla17_result_units) %>% 
    ungroup() 
  
  # NLA17 Sediment Chemistry----
  
  nla17_sed_chem <- readr::read_csv(nla17_sed_chem_file,
                                    na = c("", " "),
                                    show_col_types = FALSE) %>%
    janitor::clean_names()%>%
    select(site_id, visit_no, analyte, result, units) %>%
    dplyr::rename_with(~paste0("nla17_", .)) %>%
    filter(nla17_analyte == "SED_C")%>%
    # Need to aggregate across visits 1 and 2.  Group by nla_id and analyte
    dplyr::group_by(nla17_site_id, nla17_analyte) %>% # group to aggregate visits 1 and 2
    summarize(nla17_result = mean(nla17_result, na.rm = TRUE), # aggregate results across visits
              nla17_units = unique(nla17_units)) %>% # retain units
    mutate(across(nla17_result, ~replace(., is.nan(.), NA))) %>%
    ungroup()
  
  
  # vector of analytes to keep
  nla17_analytes <- c("ammonia_n", "chla", "doc", "sulfate",
                      "nitrate_n", "nitrite_n", "nitrate_nitrite_n",
                      "ntl", "ptl", # TN and TP
                      "sed_c",
                      "ph", "cond", "turb")
  
  # bind rows and pivot to wide
  nla17_chem <- bind_rows(nla17_water_chem, nla17_sed_chem) %>%
    rename(nla17 = nla17_result) %>% # simplify name
    pivot_wider(names_from = nla17_analyte, 
                values_from = c(nla17, nla17_units)) %>%
    janitor::clean_names() %>%
    select(nla17_site_id, 
           matches(paste(nla17_analytes, collapse = "|")))
  
  # NLA22 Chemistry----
  nla22_chem <- readr::read_csv(nla22_water_chem_file,
                                na = c("", " "),
                                show_col_types = FALSE) %>%
    # one "LRL" value in COND_QA_FLAG  column throws warning about column type.
    # This observation isn't for a SuRGE lake, so suppress warning and ignore.
    suppressWarnings() %>%
    janitor::clean_names() %>%
    select(
      site_id, visit_no, 
      matches(paste(nla17_analytes, collapse = "|"))
    ) %>%
    select(
      !matches(paste(c("date", "dilution", "holding", "volume", "extract",
                       "lab_sample", "sample_id", "mdl", "flag", "rl",
                       "ptl_diss", "ntl_diss"), # dissolved TN and TP 
                     collapse = "|"))
    ) %>% # close select
    dplyr::rename_with(~paste0("nla22_", .)) %>% # append nla22_ to all columns names
    rename_with(~gsub("_result", "", .)) %>% # remove _result from analyte names
    # remove NLA22_WI-10006 because it has a mix of NA and real values
    # for units which throws off unique below.
    filter(nla22_site_id != "NLA22_WI-10006") %>% 
    group_by(nla22_site_id) %>% # group to aggregate visits 1 and 2
    summarize(
      # aggregate results across visits
      across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
      # retain units
      across(where(is.character), \(x) unique(x))
    ) %>%
    select(-nla22_visit_no) 
  
  nla_chem <- lst(nla17_chem, nla22_chem)
  return(nla_chem)
  
} # close get_nla
