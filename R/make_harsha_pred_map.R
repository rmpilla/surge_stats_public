# SCRIPT FOR CREATING HARSHA LAKE PREDICTIONS MAPS
# spent many hours trying to integrate this into targets
# workflow but ran inot countless issues. The primary problem
# is that targets can't handle terra object. The wrapper library
# geotargets is supposed to address this, and I had limited success
# in a test repo, but couldn't get it to work in surge_stats. Resorting
# to writing image outside of targets, then reading into .qmd.

# setup
library(terra)
library(tidyterra)
library(tidyverse)
library(ranger)
library(patchwork)

# data
tar_load(dat_trans)
tar_load(modeling_vars_final)
tar_load(random_forest_object)

### get bathymetry data----


harsha_bathy_ras <- terra::rast("input_data/external_data/harsha/bathymetry.gdb", "high_res_bathy") %>%
  terra::project("ESRI:102008") # CONUS ALBERS
class(harsha_bathy_ras) # terra SpatRaster
names(harsha_bathy_ras) <- "site_depth" # fix attribute name in raster
harsha_bathy_ras$site_depth <- harsha_bathy_ras$site_depth * -1 # 
harsha_bathy_ras$id <- 1:ncell(harsha_bathy_ras) # ras cell ids. required for join below

# plot bathymetry data
plot(harsha_bathy_ras$site_depth)

# convert bathymetry raster to sf point object based on raster cell centers
# this will be used to krige other predictor values to grid.
harsha_grid <- harsha_bathy_ras %>% terra::as.points() %>% st_as_sf

### get chl, turbidity_shallow, and temp_shallow
pred_dat1 <- readxl::read_xlsx(
  path = "input_data/external_data/harsha/2014HarshaLakeFlyover_WaterChemistryDataUsedForChlorophyllStudy.xlsx",
  sheet = "WaterChemistryData ",
  skip = 1,
  na = c(".", "")
) %>%
  janitor::clean_names() %>%
  mutate(
    lat = case_when(
      !is.na(recorded_latitutde) ~ recorded_latitutde,
      is.na(recorded_latitutde) & !is.na(target_latitude) ~ target_latitude,
      is.na(recorded_latitutde) & is.na(target_latitude) ~ NA_real_,
      TRUE ~ 99999999999 # error flag
    ),
    lon = case_when(
      !is.na(recorded_longitude) ~ recorded_longitude,
      is.na(recorded_longitude) & !is.na(target_longitude) ~ target_longitude,
      is.na(recorded_longitude) & is.na(target_longitude) ~ NA_real_,
      TRUE ~ 99999999999 # error flag
    )
  ) %>%
  filter(
    !is.na(lat) & !is.na(lon) # no missing values for st_as_sf
  ) %>%
  select(
    lat, lon,
    chla_lab_shallow = sum_rchl_ug_l,
    turbidity_shallow = turbidity_ntu,
    temp_shallow = temp_c
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform("ESRI:102008") %>% # CONUS ALBERS
  # extract site_depth from bathymetry raster for each point
  terra::extract(harsha_bathy_ras$site_depth, ., bind=TRUE) %>% # SpatVector
  st_as_sf %>% # back to sf points
  filter(!is.na(site_depth)) # one point outside depth grid

# ensure spatial geometry of points and bathymetry agree
plot(harsha_bathy_ras$site_depth) # plot bathymetry data
plot(st_geometry(pred_dat1), add = TRUE, col = "red", pch = 16) # add points


### get do_deep and temp_deep from 2015 survey year----
# Beaulieu, J. J., et al. (2018). "Effects of an Experimental Water-level 
# Drawdown on Methane Emissions from a Eutrophic Reservoir." 
# Ecosystems 21(4): 657-674.
pred_dat2 <- readxl::read_xlsx(
  path = "input_data/external_data/harsha/2015drawdownMasterDataSheet.xlsx",
  sheet = "sondeData",
  na = "NA"
) %>%
  janitor::clean_names() %>%
  mutate(
    sample_date = as.Date(sample_date, "%Y-%m-%d"),
    sample_depth_cat = case_when(
      sample_depth_ft < 2 ~ "shallow",
      sample_depth_ft > 2 ~ "deep",
      TRUE ~ "FLY YOU FOOLS" # error code
    ), # close case_when
    site = case_when(
      site == "EUS 2" ~ "EUS2",
      site == "EUS 3" ~ "EUS3",
      TRUE ~ site
    ) # close case_when
  ) %>%
  # summer in Harsha
  filter(
    lake == "Harsha",
    sample_date > as.Date("2015-06-30") &
      sample_date < as.Date("2015-09-01")
  ) %>%
  select(
    site, sample_depth_cat, temp_c, do_mg_l
  ) %>%
  group_by(site, sample_depth_cat) %>%
  summarize(
    across(c(temp_c, do_mg_l), \(x) mean(x, na.rm = TRUE))
  ) %>%
  ungroup %>%
  pivot_wider(
    names_from = sample_depth_cat, 
    values_from = c(temp_c, do_mg_l)
  ) %>%
  rename(
    temp_shallow = temp_c_shallow,
    temp_deep = temp_c_deep,
    do_deep = do_mg_l_deep
  ) %>%
  select(-do_mg_l_shallow) %>%
  # now bring in latitude and longitude from different sheet in same file
  left_join(
    readxl::read_xlsx(
      path = "input_data/external_data/harsha/2015drawdownMasterDataSheet.xlsx",
      sheet = "masterDataSheet",
      skip = 1,
      na = "NA"
    ) %>%
      janitor::clean_names() %>%
      mutate(
        across(c(latitude, longitude), as.numeric)
      ) %>%
      group_by(lake, site) %>%
      summarize(
        across(c(latitude, longitude), \(x) mean(x, na.rm = TRUE))
      ) %>%
      ungroup %>%
      filter(lake == "Harsha") %>%
      select(-lake) %>%
      mutate(
        longitude = case_when(
          site == "EUS2" ~ -84.0905976, # measured values outside bathy raster
          TRUE ~ longitude
        )
      )
  ) %>%
  select(-site) %>%
  # convert to spatial
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform("ESRI:102008") %>% # CONUS ALBERS
  # extract site_depth from bathymetry raster for each point
  terra::extract(harsha_bathy_ras$site_depth, ., bind=TRUE) %>% # SpatVector
  st_as_sf # back to sf points

### get sediment organic chemistry data----
# Berberich, M. E., et al. (2019). "Spatial variability of sediment methane 
# production and methanogen communities within a eutrophic reservoir: 
# Importance of organic matter source and quantity." Limnology and Oceanography.
pred_dat3 <- readxl::read_xlsx(
  path = "input_data/external_data/harsha/sed_ea_isotopes_may.xlsx"
) %>%
  janitor::clean_names() %>%
  mutate(site_number = as.numeric(site_number)) %>%
  select(
    site_number,
    nla17_sed_c = c_org_percent
  ) %>%
  group_by(site_number) %>%
  summarize(site_number = unique(site_number),
            nla17_sed_c = mean(nla17_sed_c)) %>%
  # now bring in latitude and longitude from different sheet in same file
  left_join(
    readxl::read_xlsx(
      path = "input_data/external_data/harsha/sed_ea_site_information.xlsx"
    ) %>%
      janitor::clean_names() %>%
      select(
        site_number, latitude, longitude
      ) %>%
      mutate(longitude = longitude * -1)
  ) %>%
  select(-site_number) %>%
  # convert to spatial
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform("ESRI:102008") %>% # CONUS ALBERS
  # extract site_depth from bathymetry raster for each point
  terra::extract(harsha_bathy_ras$site_depth, ., bind=TRUE) %>% # SpatVector
  st_as_sf # back to sf points


### join pred_dat1, pred_dat2, and pred_dat3----
pred_dat4 <- bind_rows(pred_dat1, pred_dat2, pred_dat3) %>%
  # aggregate across duplicate geometries
  group_by(geometry) %>%
  summarise(
    across(everything(), \(x) mean(x, na.rm = TRUE))
  ) %>%
  # replace NaN with NA
  mutate(across(!geometry, ~ ifelse(is.nan(.), NA, .))) %>%
  pivot_longer(!c(geometry, site_depth)) %>%
  filter(!is.na(value)) %>%
  group_by(name) %>%
  # split into list element by grouping variable.
  # use set_names and group_keys to name each element
  {set_names(group_split(.), group_keys(.)[[1]])}



### create splm model for each predictor variable----
pred_mod <- map(
  pred_dat4, \(x)
  splm(value ~ site_depth, data = x, spcov_type = "exponential")
)

### predict values of predictors across the bathymetry grid----
library(furrr)
# Set up parallel workers (multisession is recommended for compatibility)
plan(multisession, workers = availableCores() - 1) 
tic()
pred_preds <- future_imap(
  pred_mod, \(x, y) # x is list, y is list element name
  predict(x, newdata = harsha_grid) %>%
    as_tibble %>%
    # use list element name to rename .fitted column
    # with name of variable (e.g. temp_shallow, etc)
    rename_with(., .fn = \(...) y, .cols = "value")
) %>%
  # The above produces a named list of tibbles. Each tibble has one
  # column that is named with the analyte it represents (temp_shallow).
  # next we used list_cbind to collapse the list into a single DF. First
  # we remove the list element name, else the column names of the new DF
  # will be a concatenation of the list element name + colum name
  unname %>%
  list_cbind() %>%
  # now bind harsha_grid (site_depth, id, and geometry)
  cbind(harsha_grid, .) # creates sf object
toc() # 1.5 hours
plan(sequential) # turn off furrr parallel processing

## remove large objects from environment
rm(harsha_grid)

### map predictor values across bathymetry grid----
# Get column names excluding the geometry column
cols_to_plot <- setdiff(names(pred_preds), c("geometry", "id"))

# Loop and save
for (col in cols_to_plot) {
  # Create filename
  filename <- paste0("input_data/external_data/harsha/", col, ".png") #width = 800, height = 600)
  
  # Start PNG device
  png(filename)
  
  # Plot attribute column
  plot(pred_preds[col], main = paste("Map of", col))
  
  # Close device
  dev.off()
}

### gather other predictors needed for model----
# these predictors are invariant across space
static_preds <- dat_trans %>% # all raw data
  filter(lake_id == 1031) %>% # 
  select(
    all_of(
      unname(
        modeling_vars_final$ch4_total$local_vars_all # prediction vars
      )
    ),
    -any_of(colnames(pred_preds)) # already in raster
  ) %>%
  # aggregate
  summarize(across(!c(ag_eco9_nm), mean),
            ag_eco9_nm = unique(ag_eco9_nm)) %>%
  # impute missing values
  mutate(
    e_i = 0.16, # mean SuRGE value
    residence_time = 0.59 # mean SuRGE value
  )
colnames(static_preds)

### join ALL predictors----
all_pred_grid <- bind_cols(pred_preds, static_preds)

### predict emissions----
# predict() generates a list
# could not allocate memory error
pred_ranger_harsha <- predict(
  object = random_forest_object$ch4_total.local_vars_all$ranger, 
  data = all_pred_grid %>%
    sf::st_drop_geometry(.),
  type = "se"
)

# place list components into DF, backtransform predictions from log to linear,
# join with DF containing raster cell ID, convert to spatraster for plotting.
# bias correction formula for log -> linear is exp(y^ + (residual variance / 2))
# residual var for CH4 total, local, is var(residuals). See spmodel github issue
# 39 for discussion of how to retrieve residuals. se is transformed by multiplying
# original se value by transformed value. 
resid_var <- random_forest_object$ch4_total.local_vars_all$splm$obdata$.ranger_resid %>%
  var

harsha_pred_ras <- tibble(
  ch4_tot_log =  pred_ranger_harsha$predictions,
  ch4_tot_corrected =  exp(pred_ranger_harsha$predictions + (resid_var/2)) - 1,
  ch4_tot_uncorrected =  expm1(pred_ranger_harsha$predictions),
  ch4_tot_se_log = pred_ranger_harsha$se,
  ch4_tot_se_uncorrected = pred_ranger_harsha$se * ch4_tot_uncorrected,
  ch4_tot_se_corrected = pred_ranger_harsha$se * ch4_tot_corrected, 
) %>%
  bind_cols(
    all_pred_grid %>%
      mutate(x = st_coordinates(.)[, 1],
             y = st_coordinates(.)[, 2]) %>%
      sf::st_drop_geometry(all_pred_grid)
  ) %>%
  # put coordinates in first two columns to allign with xycols arguments in as_spatraster
  relocate(x, y) %>%
  tidyterra::as_spatraster(xycols = 1:2, crs = crs("ESRI:102008")) # CONUS ALBERS


# linear plots
# corrected transformation
p.mean <- ggplot() +
  geom_spatraster(data = harsha_pred_ras$ch4_tot_corrected) +
  scale_fill_gradientn(
    colors = terra::map.pal("viridis", 100), # defalt terra::plot palette
    na.value = "transparent") +
  labs(fill = expression(CH[4] ~ "Total" ~ (mg ~ CH[4] ~ m^{-2} ~ d^{-1}))) +
  theme_bw() + # Sets white background
  theme(legend.position = "top",
        panel.grid = element_blank()) + # remove grid
  # 2. Place title above the guide and center it
  guides(fill = guide_colorbar(
    direction = "horizontal", # Bar is horizontal
    title.position = "top",   # Title is above the bar
    barwidth = unit(10, "cm") # width of color bar
    )) +
  theme(plot.tag.position = c(0.15, 0.76))
  

p.se <- ggplot() +
  geom_spatraster(data = harsha_pred_ras$ch4_tot_se_corrected) +
  scale_fill_gradientn(
    colors = terra::map.pal("viridis", 100), # defalt terra::plot palette
    na.value = "transparent") +
  labs(fill = expression(CH[4] ~ "Total SE" ~ (mg ~ CH[4] ~ m^{-2} ~ d^{-1}))) +
  theme_bw() + # Sets white background
  theme(legend.position = "top",
        axis.text.y = element_blank(), #  Removes tick labels
        panel.grid = element_blank()) + # remove grid
  # 2. Place title above the guide and center it
  guides(fill = guide_colorbar(
    direction = "horizontal", # Bar is horizontal
    title.position = "top",   # Title is above the bar
    barwidth = unit(10, "cm") # width of color bar
  )) +
  theme(plot.tag.position = c(0.05, 0.76))

combined <- p.mean + p.se +
   plot_annotation(tag_levels = 'A') 
ggsave("harsha_pred_map.png", combined, width = 10, height = 6, dpi = 300)

