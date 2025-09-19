# HARSHA LAKE CHEMISTRY DATA FROM NIETCH
# SEE 9/11/2025 EMAIL 

# WE ARE LOOKING FOR DOC AND SULFATE IN 2016

hl_oc <- readxl::read_xlsx("input_data/EastFork-HarshaLake_GrabSamplingAndDiscreteSondeData_Merged_CTN03242022.xlsx",
                  sheet = "SortMergedData2") %>%
  clean_names() %>%
  mutate(date = as.Date(date)) %>%
  # Shallow TOC and DOC from DAM site (best approximation of surge index site)
  filter(analyte %in% c("TOC", "DOC"),
         id == "DAM",
         depth_ft == 0) %>%
  select(date, analyte, value) %>%
  # A single pair of TOC measurements on same data, but different times
  # aggregate value
  group_by(date, analyte) %>%
  summarize(value = mean(value)) %>%
  ungroup()



# NO SULFATE IN DATASET
# ONLY TOC IN 2016, BUT TOC AND DOC IN PREVIOUS YEARS
# CAN WE ASSUME EQUIVALENCY BETWEEN DOC AND TOC?
# YES, PRETTY DARN CLOSE

hl_oc %>%
  pivot_wider(
    names_from = analyte,
    values_from = value
  ) %>%
  filter(
    !is.na(TOC) & !is.na(DOC)
  ) %>%
  ggplot(aes(TOC, DOC)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

# LOOK AT 2016 TOC
# Surge sampling date was 8/1/2016,
# we have 8/4/2016 from Nietch
hl_oc %>%
  mutate(year = format(date, "%Y"),
         month = format(date, "%m")) %>%
  filter(year == "2016",
         month %in% c("05", "06", "07", "08", "09"))

# The value is 6.72 mg_l
hl_oc %>%
  filter(date == as.Date("2016-08-04"))
