make_table3_object <- function(random_forest_object, table2_object, dat_trans) {
  
  # 1. Get number of reservoirs in each of our models----
  this_study_n <- c()
  for(i in 1:length(random_forest_object)){
    # extract response variable
    ## \\.: Matches a literal period (.). The backslash is escaped with another backslash for R.
    ## .*: Matches any character (except a newline) zero or more times.
    ## "": Replaces the matched pattern (the first . and everything after) with an empty string. 
    response = sub("\\..*", "", names(random_forest_object)[i])
    
    # extract model_var name
    ## Matches the start (^) followed by the fewest characters (.*?) 
    ## and a literal dot (\\.) and replaces them with an empty string ("")
    model_var_set = sub("^.*?\\.", "", names(random_forest_object)[i])
    
    # extract predictor variables
    predictor_vars = names(random_forest_object[[i]]$ranger$variable.importance)
    
    # use response and predictor names to extract the training data
    n_lake = dat_trans %>%
      select(
        all_of(response), all_of(predictor_vars), lake_id # vars in model
      ) %>%
      drop_na() %>% # omit any rows with NA
      # pull(lake_id) %>%
      # mutate(lake_id = as.numeric(lake_id)) %>%
      distinct(lake_id) %>%
      summarize(n=n())
    
    # get number of lakes in each model. exclude the "no_nla" observations.
    this_study_i <- if(grepl("no_nla", model_var_set)) {
      NULL
    } else {
      tibble(
        n_lake,
        response,
        model_var_set
      ) 
    }
    
    this_study_n <- bind_rows(this_study_i, this_study_n)
  }
  
  # 2. Get model r2 values for each of our models from table2_dat target----
  this_study_r2 <- table2_object$table2_dat %>%
    select(response, model_var_set, r2_log)
  
  # 3. merge n and r2 from our study, then format----
  this_study <- inner_join(
    this_study_n, 
    this_study_r2
    ) %>%
  mutate(
    Pathway = substr(response, 5, nchar(response)),
    Model = case_when(
      grepl("global", model_var_set) ~ "This study (national model)",
      grepl("local", model_var_set) ~ "This study (local model)",
      TRUE ~ "FLY YOU FOOLS!"
    ),
    Domain = "U.S.",
    System = "reservoir",
    Citation = NA
  ) %>%
    select(Model, Domain, System, n, Pathway, r2 = r2_log, Citation)
    
    
# 4. collate literature data and add data from this study----
  table3_object <- 
    tribble(
      ~Model,  ~Domain, ~System, ~n, ~Pathway, ~r2, ~Citation,
      "G-res", "global", "reservoir", 160, "diffusion", 0.51, "Prairie et al. 2021",
      "G-res", "global", "reservoir", 46, "ebullition", 0.26, "Prairie et al. 2021",
      "Size productivity weighted", "global", "reservoir & lakes", 423,  "diffusion", 0.29, "DelSontro et al. 2018",
      "Size productivity weighted", "global", "reservoir & lakes", 65, "ebullition", 0.32, "DelSontro et al. 2018",
      "Size productivity weighted", "global", "reservoir & lakes", 74, "total", 0.38, "DelSontro et al. 2018",
      "Multivariate regression model", "global", "reservoir & lakes", 167, "total", 0.44, "Deemer and Holgerson 2021",
      #"generalized additive model", "Spain", "reservoirs", 12, "total", 0.59, "Leon_Palmero et al. 2020",
      "Advanced Lake Biogeochemistry Model", "global", "reservoir & lakes", 106, "diffusion", 0.79, "Tan 2024",
      "Advanced Lake Biogeochemistry Model", "global", "reservoir & lakes", 106, "total", 0.88, "Tan 2024",
      "ResME", "global", "reservoir", 54, "total", 0.41, "Delwiche et al. 2022",
      "FLaMe-v1.0", "Europe", "reservoir and lakes", 47, "total", 0.52, "Maisonnier et al. 2025"
    ) %>%
    bind_rows(
      this_study %>%
        arrange(Model, Pathway)
      ) %>%
    # need to add footnote that n is number of waterbodies
    # G-Res is mean annualized over 100 years
    # Tan is comparing to mean annual
    flextable() %>%
    theme_booktabs() %>%
    # two digits in r2 column
    colformat_double(j = "r2", digits = 2) %>%
    # Add bottom border below each citation
    hline(i = c(2,5,6,8,9,10,13), border = fp_border(width = 1)) %>%
    # remove border line from top of table
    hline_top(
      border = fp_border_default(width = 0), 
      part = "header"
    ) %>%
    merge_v(
      j = c("Model", "Domain", "System", "Citation"), 
      target =  c("Model", "Domain", "System", "Citation"), 
      combine = TRUE
      ) %>%
    #valign(valign = c("Model", "Domain", "Citation"), part = "body") %>% # center vertically
    # format column title
    compose(
      part = "header",
      j = "r2",
      value = as_paragraph("r", as_sup("2"))
    ) %>%
    # center align numeric column titles and data
    align(
      j = 4:6,
      align = "center",
      part = "all"
    ) %>%
    # footnotes
    flextable::footnote(
      j = 4, # column
      value = as_paragraph(
        "Number of waterbodies in validation dataset. Models in this study are trained using multiple observations within each reservoir whereas the other studies aggregate multiple observations into a single value per waterbody for model training"
      ),
      ref_symbols = c("a"),
      part = "header"
    ) %>%
    flextable::footnote(
      j = 6, # column
      value = as_paragraph(
        "r", as_sup("2"), " values derived by comparing predicted vs observed in log space"
      ),
      ref_symbols = c("b"),
      part = "header"
    ) %>%
    flextable::footnote(
      i = c(1,2, 7,8), # rows
      j = 6, # column
      value = as_paragraph(
        " r", as_sup("2"), " calculated for mean annual rate"
      ),
      ref_symbols = c("c"),
      part = "body"
    ) %>%

    fontsize(size = 8, part = "all") %>%
    line_spacing(space = 1, part = "all") %>%   # single space
    autofit()
  
  return(table3_object)
}