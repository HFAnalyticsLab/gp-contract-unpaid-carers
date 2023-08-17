library(tidyverse)
library(aws.s3)

# Set bucket
bucket <- "thf-dap-tier0-projects-ndl-f3b6da96-projectbucket-orxht6uldbv4"

####################################
##### LOAD IN PROCESSED DATA #######
####################################

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


if (file.exists('gp_census_join.csv')){
  gp_census_join <- read.csv('gp_census_join.csv')
} else {
  source('2_gp-contract-census-comparisons.R')
  gp_census_join <- read.csv('gp_census_join.csv')
}


##################################################
######### INVESTIGATE MISSING PRQCTICES ##########
##################################################

# Isolate missing practices
missing_practices <- gps_LAs[is.na(gps_LAs$TOTAL_UNPAID_CARERS),] %>%
  group_by(PRACTICE_CODE) %>%
  summarise(TOTAL_PRACTICE_PATIENTS = median(TOTAL_PRACTICE_PATIENTS))

# Add graph of GP catchment areas 

gp_practices_map <- s3read_using(read_sf,
                                 object = '/Tom/GP-contract-unpaid-carers/Data/GP_catchment_areas_(England).geojson',
                                 bucket = bucket)

# Identify locations of missing practices

missing_practices_locations <- left_join(missing_practices, gp_practices_map, by =c("PRACTICE_CODE"="PracticeCd"))


# Check if patterns exist in missing practice location

gp_bypractice <- gps_LAs %>%
  group_by(PRACTICE_CODE) %>%
  summarise(TOTAL_PRACTICE_PATIENTS = median(TOTAL_PRACTICE_PATIENTS), TOTAL_UNPAID_CARERS = median(TOTAL_UNPAID_CARERS))

all_practice_locations <- full_join(gp_bypractice, gp_practices_map, by =c("PRACTICE_CODE"="PracticeCd"))

# Final statements

print(paste0(nrow(missing_practices), ' practices are missing from the GP contract data, accounting for ', sum(missing_practices$TOTAL_PRACTICE_PATIENTS), ' patients in England.'))

