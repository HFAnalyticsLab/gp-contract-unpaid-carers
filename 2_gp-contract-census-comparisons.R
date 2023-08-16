library(tidyverse)
library(aws.s3)
library(sf)
library(geojsonio)
library(broom)
library(mapproj)

# Set bucket
bucket <- "thf-dap-tier0-projects-ndl-f3b6da96-projectbucket-orxht6uldbv4"

# Load in census data
census_carers <- s3read_using(read.csv,
                                      object = '/Tom/GP-contract-unpaid-carers/Data/TS039-2021-1.csv',
                                        bucket = bucket)
census_carers <- census_carers %>%
  rename(LA_CODE = 1, LA_NAME = 2, UNPAID_CARE_CODE = 3, UNPAID_CARE_CATEGORY = 4, NO_OF_CARERS = 5) %>%
  mutate(CARER = case_when(UNPAID_CARE_CODE %in% c(2,3,4,5,6) ~ 1,
                           TRUE ~ 0)) %>%
  filter(CARER == 1) %>%
  group_by(LA_CODE, LA_NAME) %>%
  summarise(CENSUS_NO_OF_CARERS = sum(NO_OF_CARERS))

# Join mapped gp and census data 

gp_census_join <- full_join(gps_LAs_grouped, census_carers, by = "LA_CODE") %>%
  mutate(difference = CENSUS_NO_OF_CARERS - EST_CARERS_IN_LA) %>%
  mutate(percent_differenct = CENSUS_NO_OF_CARERS/EST_CARERS_IN_LA-1)

print(paste0("The census records ", sum(census_carers$CENSUS_NO_OF_CARERS), " unpaid carers, while the GP contract data records ", sum(gps_LAs_grouped$EST_CARERS_IN_LA)))

# Create map of LAs 

map <- s3read_using(geojson_read, what = "sp",
                              object = '/Tom/GP-contract-unpaid-carers/Data/Local_Authority_Districts_December_2022_UK_BUC_V2_-5963189729337928393.geojson',
                              bucket = bucket)
map_fortified <- st_as_sf(map) %>%
  rename(LA_CODE = LAD22CD)

map_data_join <- full_join(map_fortified, gp_census_join, by=c("LA_CODE"))

ggplot() +
  geom_polygon(data = map_data_join, aes(x = LONG, y = LAT, group = FID)) +
  theme_void() +
  coord_map()
