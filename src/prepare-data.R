################################################################################
# Prepare data for consumption in the Shiny app
#
# Inputs:
#
# Outputs:
# data-model.rds
#
# Author: Stefan Schliebs
# Created: 2020-06-26 09:06:14
################################################################################


library(sf)
library(readr)
library(dplyr)
library(logging)



# Config ------------------------------------------------------------------

basicConfig()

# shape file obtained from 
# https://datafinder.stats.govt.nz/layer/92212-statistical-area-2-2018-generalised/ 
F_SA_SHAPEFILE <- "data/shapes/statistical-area-2-2018-generalised.shp"
SA_SHAPE_LAYER <- "statistical-area-2-2018-generalised"

# commute data obtained from 
# https://datafinder.stats.govt.nz/table/104720-2018-census-main-means-of-travel-to-work-by-statistical-area-2/
F_COMMUTE_WORK_DATA <- "data/2018-census-main-means-of-travel-to-work-by-statistical-area.csv"
F_COMMUTE_EDU_DATA <- "data/2018-census-main-means-of-travel-to-education-by-statistical.csv"

# geographic areas to map from SA to TA, data obtained from
# https://datafinder.stats.govt.nz/table/104285-geographic-areas-file-2020/data/
F_GEOGRAPHIC_AREAS <- "data/geographic-areas-file-2020.csv"
  
# file to export data model to
F_DATA_MODEL <- "data/data-model.rds"



# Prepare commute data ----------------------------------------------------

# load statistical areas and create a lookup table from SA2 to REG
d_areas <- read_csv(F_GEOGRAPHIC_AREAS) %>% 
  janitor::clean_names() %>% 
  select(sa22018_code, regc2020_name) %>%
  distinct()

# load commute data
d_commute_work <- read_csv(F_COMMUTE_WORK_DATA) %>% janitor::clean_names()
d_commute_edu <- read_csv(F_COMMUTE_EDU_DATA) %>% janitor::clean_names()

clean_commute_data <- function(d) {
  d %>% 
    # replace all occurrences of -999 with NA
    mutate(across(everything(), ~ifelse(.x == -999, NA, .x))) %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.x), 0, .x))) %>%
    
    # group some commuting modes into categories
    mutate(
      commute_car = 
        drive_a_private_car_truck_or_van + 
        drive_a_company_car_truck_or_van + 
        passenger_in_a_car_truck_van_or_company_bus,
      commute_public = public_bus + train + ferry + school_bus,
      commute_all = total - work_at_home
    ) %>%
    
    # join region names
    left_join(
      d_areas %>% rename(commute_from_region = regc2020_name), 
      by = c("sa2_code_usual_residence_address" = "sa22018_code")
    ) %>% 
    left_join(
      d_areas %>% rename(commute_to_region = regc2020_name), 
      by = c("sa2_code_workplace_address" = "sa22018_code")
    ) %>% 
    
    # remove the outside regions
    filter(commute_from_region != "Area Outside Region", commute_to_region != "Area Outside Region") %>%
    
    select(
      commute_from = sa2_name_usual_residence_address,
      commute_from_code = sa2_code_usual_residence_address,
      commute_from_region, 
      commute_to = sa2_name_workplace_address, 
      commute_to_code = sa2_code_workplace_address, 
      commute_to_region,
      work_at_home, 
      commute_car, 
      commute_bus = public_bus, 
      commute_train = train, 
      commute_bicycle = bicycle, 
      commute_walk_or_jog = walk_or_jog, 
      commute_ferry = ferry,
      commute_public,
      commute_other = other, 
      commute_all
    ) %>% 
    mutate(across(c(commute_from_code, commute_to_code), as.character))
}

d_commute_work_clean <- 
  d_commute_work %>% 
  mutate(school_bus = 0) %>% 
  clean_commute_data() %>% 
  mutate(source = "work")

d_commute_edu_clean <- 
  d_commute_edu %>% 
  mutate(
    drive_a_private_car_truck_or_van = drive_a_car_truck_or_van, 
    drive_a_company_car_truck_or_van = 0,
    passenger_in_a_car_truck_van_or_company_bus = passenger_in_a_car_truck_or_van,
    work_at_home = study_at_home
  ) %>% 
  rename(sa2_code_workplace_address = sa2_code_educational_address, sa2_name_workplace_address = sa2_name_educational_address) %>% 
  clean_commute_data() %>% 
  mutate(source = "edu")

d_commute_all_clean <- 
  bind_rows(d_commute_work_clean, d_commute_edu_clean) %>% 
  group_by(commute_from, commute_from_code, commute_from_region, commute_to, commute_to_code, commute_to_region) %>% 
  summarise(across(where(is.numeric), sum), .groups = "drop") %>% 
  mutate(source = "all")

d_commute_clean <- bind_rows(d_commute_work_clean, d_commute_edu_clean, d_commute_all_clean)


# area_code - name lookup ------------------------------------------------------

d_lookup <- 
  bind_rows(
    select(d_commute_clean, id = commute_from_code, name = commute_from),
    select(d_commute_clean, id = commute_to_code, name = commute_to)
  ) %>% 
  distinct()

d_lookup_region <- 
  bind_rows(
    select(d_commute_clean, name = commute_from, code = commute_from_code, region = commute_from_region),
    select(d_commute_clean, name = commute_to, code = commute_to_code, region = commute_to_region)
  ) %>% 
  distinct()


# Prepare map data --------------------------------------------------------

loginfo("Loading shape %s", F_SA_SHAPEFILE)
sh_sa <- st_read(F_SA_SHAPEFILE, layer = SA_SHAPE_LAYER)

loginfo("Simplifying polygons")
sh_sa_simple <- rmapshaper::ms_simplify(sh_sa, keep = 0.05)

loginfo("Removing water areas")
sh_sa_simple_nowater <- 
  sh_sa_simple[sh_sa_simple$LAND_AREA_ > 0,] %>% 
  left_join(d_areas %>% mutate(sa22018_code = as.character(sa22018_code)), by = c("SA22018_V1" = "sa22018_code")) %>% 
  filter(regc2020_name != "Area Outside Region") %>% 
  select(-LAND_AREA_, -AREA_SQ_KM, -Shape_Leng)
  
size_before <- as.numeric(object.size(sh_sa))
size_after <- as.numeric(object.size(sh_sa_simple_nowater))

loginfo("Shapefile size reduced by factor %.1f", size_before / size_after)
loginfo("Shapefile reduced size %.1f mb", size_after / 2^20)



# Export data model -------------------------------------------------------

data_model <- list(
  shapes = sh_sa_simple_nowater,
  d_commute = d_commute_clean,
  d_lookup = d_lookup,
  d_lookup_region = d_lookup_region
)

saveRDS(data_model, file = F_DATA_MODEL)
