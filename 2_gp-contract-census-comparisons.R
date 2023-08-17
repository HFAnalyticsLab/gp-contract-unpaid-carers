library(tidyverse)
library(aws.s3)
library(sf)
library(plotly)

# Set bucket
bucket <- "thf-dap-tier0-projects-ndl-f3b6da96-projectbucket-orxht6uldbv4"


######################################
###### LOAD IN & PREPARE DATA ########
######################################

# Load in processed GP data

if (file.exists('gps_LAs.csv')){
  gps_LAs <- read.csv('gps_LAs.csv')
} else {
  source('1_gp-practice-LA-mapping.R')
  gps_LAs <- read.csv('gps_LAs.csv')
}

if (file.exists('gps_LAs_grouped.csv')){
  gps_LAs_grouped <- read.csv('gps_LAs_grouped.csv')
} else {
  source('1_gp-practice-LA-mapping.R')
  gps_LAs_grouped <- read.csv('gps_LAs_grouped.csv')
}


# Load in census data
census_carers <- s3read_using(read.csv,
                                      object = '/Tom/GP-contract-unpaid-carers/Data/TS039-2021-1.csv',
                                        bucket = bucket)

# Prepare census data for join - aggregate and isolate all carer lines, then filter for only England LAs

census_carers <- census_carers %>%
  rename(LA_CODE = 1, LA_NAME = 2, UNPAID_CARE_CODE = 3, UNPAID_CARE_CATEGORY = 4, NO_OF_CARERS = 5) %>%
  mutate(CARER = case_when(UNPAID_CARE_CODE %in% c(2,3,4,5,6) ~ 1,
                           TRUE ~ 0)) %>%
  filter(CARER == 1) %>%
  group_by(LA_CODE, LA_NAME) %>%
  summarise(CENSUS_NO_OF_CARERS = sum(NO_OF_CARERS)) %>%
  filter(str_detect(LA_CODE, "^E"))



#######################################
###### JOIN GP AND CENSUS DATA ########
#######################################

# Join mapped gp and census data 

gp_census_join <- full_join(gps_LAs_grouped, census_carers, by = "LA_CODE") %>%
  mutate(Difference = CENSUS_NO_OF_CARERS - EST_CARERS_IN_LA) %>%
  mutate(Coverage = EST_CARERS_IN_LA/CENSUS_NO_OF_CARERS)


####################################
########## COMPARISONS #############
####################################

# Examine distribution of coverage

ggplot()+
  geom_histogram(data = gp_census_join, aes(x=Coverage), color = 'darkblue', fill = 'lightblue') +
  theme_minimal()


# Identify greatest differences in coverage

print(n = 20, gp_census_join[order(gp_census_join$Coverage), ])

print(n = 20, gp_census_join[order(gp_census_join$Coverage, decreasing = TRUE), ])

print(n = 20, gp_census_join[order(gp_census_join$Difference), ])

print(n = 20, gp_census_join[order(gp_census_join$Difference, decreasing = TRUE), ])


########################################
########## CREATE LA MAP ###############
########################################

# Load in map of local authorities 

map <- s3read_using(read_sf,
                              object = '/Tom/GP-contract-unpaid-carers/Data/Local_Authority_Districts_December_2022_UK_BUC_V2_-5963189729337928393.geojson',
                              bucket = bucket) %>%
  rename(LA_CODE = LAD22CD) %>%
  filter(str_detect(LA_CODE, "^E"))

# Join map to gp/census joined data
map_data_join <- full_join(map, gp_census_join, by="LA_CODE")

# Make national map (coverage)
ggplot() +
  geom_sf(data = map_data_join, aes(fill = Coverage)) +
  theme_void() +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43")

# Filter to create London map

london_join <- map_data_join %>%
  filter(str_detect(LA_CODE, "^E09"))

# Make London graph (coverage)
ggplot() +
  geom_sf(data = london_join, aes(fill = Coverage)) +
  theme_void() +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43")


# Overarching statements

print(paste0("The census records ", sum(census_carers$CENSUS_NO_OF_CARERS), " unpaid carers, while the GP contract data records ", sum(gps_LAs_grouped$EST_CARERS_IN_LA)))

print(paste0('Mean coverage of unpaid carers in the GP contract data is ', round(mean(gp_census_join$Coverage), 2), '% per LA compared to the census.'))


## Write joined census data to csv for later use

write.csv(census_data_join, 'census_data_join.csv')