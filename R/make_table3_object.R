make_table3_object <- function() {
  table3_object <- 
    tribble(
      ~Model,  ~spatial_domain, ~System, ~n, ~Pathway, ~r2, ~Citation,
      "G-res", "global", "reservoir", 160, "diffusion", 0.51, "Prairie et al. 2021",
      "G-res", "global", "reservoir", 46, "ebullition", 0.26, "Prairie et al. 2021",
      "Size productivity weighted", "global", "reservoir & lakes", 423,  "diffusion", 0.29, "DelSontro et al. 2018",
      "Size productivity weighted", "global", "reservoir & lakes", 65, "ebullition", 0.32, "DelSontro et al. 2018",
      "Size productivity weighted", "global", "reservoir & lakes", 74, "total", 0.38, "DelSontro et al. 2018",
      "Multivariate regression model", "global", "reservoir & lakes", 167, "total", 0.44, "Deemer and Holgerson 2021",
      #"generalized additive model", "Spain", "reservoirs", 12, "total", 0.59, "Leon_Palmero et al. 2020",
      "Advanced Lake Biogeochemistry Model", "global", "reservoir & lakes", 106, "diffusion", 0.79, "Tan 2024",
      "Advanced Lake Biogeochemistry Model", "global", "reservoir & lakes", 106, "total", 0.88, "Tan 2024"
    ) %>%
    select(-spatial_domain) %>% # all global
    # need to add footnote that n is number of waterbodies
    # G-Res is mean annualized over 100 years
    # Tan is comparing to mean annual
    flextable() %>%
    theme_booktabs() %>%
    
    # Add bottom border below each citation
    hline(i = c(2,5,6), border = fp_border(width = 1)) %>%
    merge_v(j = c("Model", "System"), target = c("Model", "System"), combine = TRUE) %>%
    merge_v("Citation") %>%
    valign(valign = c("Model", "Citation"), part = "body") %>% # center vertically
    # format column title
    compose(
      part = "header",
      j = "r2",
      value = as_paragraph("r", as_sup("2"))
    ) %>%
    # center align numeric column titles and data
    align(
      j = 3:5,
      align = "center",
      part = "all"
    ) %>%
    # remove border line from top of table
    hline_top(
      border = fp_border_default(width = 0), 
      part = "header"
    ) %>%
    # footnotes
    flextable::footnote(
      i = c(1,7), j = 6,
      value = as_paragraph(
        "r", as_sup("2"), " calculated for mean annual rate"
      ),
      ref_symbols = c("a"),
      part = "body"
    ) %>%
    fontsize(size = 8, part = "all") %>%
    line_spacing(space = 1, part = "all")    # Set line spacing to single space (1)
  
  
  return(table3_object)
}