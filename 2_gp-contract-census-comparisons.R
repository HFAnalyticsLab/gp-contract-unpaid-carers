library(tidyverse)
library(aws.s3)
library(sf)
library(geojsonio)
library(broom)
library(mapproj)
library(plotly)

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
  summarise(CENSUS_NO_OF_CARERS = sum(NO_OF_CARERS)) %>%
  filter(str_detect(LA_CODE, "^E"))

# Join mapped gp and census data 

gp_census_join <- full_join(gps_LAs_grouped, census_carers, by = "LA_CODE") %>%
  mutate(difference = CENSUS_NO_OF_CARERS - EST_CARERS_IN_LA) %>%
  mutate(percent_coverage = EST_CARERS_IN_LA/CENSUS_NO_OF_CARERS)

# Identify greatest differences 

print(n = 20, gp_census_join[order(gp_census_join$percent_coverage), ])

print(n = 20, gp_census_join[order(gp_census_join$difference, decreasing = TRUE), ])

hist(log(gp_census_join$percent_coverage))

hist(gp_census_join$difference)

# Inspect worst LAs

inspect_LA_function <- function(x){gps_LAs %>%
    filter(LA_NAME == x)}

inspect_practice_function <- function(x){gps_LAs %>%
    filter(PRACTICE_CODE == x)}

inspect_LA_function("Haringey")

inspect_practice_function("A86021")

# Create map of LAs 

map <- s3read_using(read_sf,
                              object = '/Tom/GP-contract-unpaid-carers/Data/Local_Authority_Districts_December_2022_UK_BUC_V2_-5963189729337928393.geojson',
                              bucket = bucket) %>%
  rename(LA_CODE = LAD22CD) %>%
  filter(str_detect(LA_CODE, "^E"))

map_data_join <- full_join(map, gp_census_join, by=c("LA_CODE"))


# Make national map (number of carers)
ggplot() +
  geom_sf(data = map_data_join, aes(fill = CENSUS_NO_OF_CARERS)) +
  theme_void()

# Make national map (total difference)
ggplot() +
  geom_sf(data = map_data_join, aes(fill = difference)) +
  theme_void()

# Make national map (coverage)
ggplot() +
  geom_sf(data = map_data_join, aes(fill = percent_coverage)) +
  theme_void()

# Make London graph (coverage)
ggplot() +
  geom_sf(data = map_data_join, aes(fill = percent_coverage)) +
  theme_void()


print(paste0("The census records ", sum(census_carers$CENSUS_NO_OF_CARERS), " unpaid carers, while the GP contract data records ", sum(gps_LAs_grouped$EST_CARERS_IN_LA)))


