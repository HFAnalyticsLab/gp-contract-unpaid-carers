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

if (file.exists('epraccur.csv')){
    
    gp_postcodes <- read.csv('epraccur.csv', header = FALSE) %>%
      select(1:10)
    
    names(gp_postcodes) <- c('PRACTICE_CODE', 'NAME', 'GROUPING', 'GEOG', 'ADDRESS_1', 
                           'ADDRESS_2', 'ADDRESS_3', 'ADDRESS_4', 'ADDRESS_5', 'POSTCODE')
    
  } else {
    
    
    zip_link <- 'https://files.digital.nhs.uk/assets/ods/current/epraccur.zip'
    
    download.file(zip_link, destfile = 'epraccur.zip')
    
    unzip('epraccur.zip', 'epraccur.csv')
    
    gp_postcodes <- read.csv('epraccur.csv', header = FALSE) %>%
      select(1:10)
    
    names(gp_postcodes) <- c('PRACTICE_CODE', 'NAME', 'GROUPING', 'GEOG', 'ADDRESS_1', 
                             'ADDRESS_2', 'ADDRESS_3', 'ADDRESS_4', 'ADDRESS_5', 'POSTCODE')
  }



if (file.exists('pcd11_par11_wd11_lad11_ew_lu.csv')){
  
  LA_postcodes <- read.csv('pcd11_par11_wd11_lad11_ew_lu.csv')
  
} else {
  
  
  zip_link <- 'https://www.arcgis.com/sharing/rest/content/items/ac40a104a37243649326629b7a1c72c1/data'
  
  download.file(zip_link, destfile = 'pcd11_par11_wd11_lad11_ew_lu.zip')
  
  unzip('pcd11_par11_wd11_lad11_ew_lu.zip', 'pcd11_par11_wd11_lad11_ew_lu.csv')
  
  LA_postcodes <- read.csv('pcd11_par11_wd11_lad11_ew_lu.csv')
}

##################################################
######### INVESTIGATE MISSING PRACTICES ##########
##################################################

# Isolate missing practices
missing_practices <- gps_LAs[is.na(gps_LAs$TOTAL_UNPAID_CARERS),] %>%
  group_by(PRACTICE_CODE) %>%
  summarise(TOTAL_PRACTICE_PATIENTS = median(TOTAL_PRACTICE_PATIENTS))

# Add graph of GP catchment areas 

gp_practices_map <- s3read_using(read_sf,
                                 object = '/Tom/GP-contract-unpaid-carers/Data/GP_catchment_areas_(England).geojson',
                                 bucket = bucket)

# Identify locations of all practices

gp_bypractice <- gps_LAs %>%
  group_by(PRACTICE_CODE) %>%
  summarise(TOTAL_PRACTICE_PATIENTS = median(TOTAL_PRACTICE_PATIENTS), TOTAL_UNPAID_CARERS = median(TOTAL_UNPAID_CARERS))

all_practice_postcodes <- left_join(gp_bypractice, gp_postcodes, by =c("PRACTICE_CODE")) %>%
  select(1:4, 12)

all_practice_locations <- left_join(all_practice_postcodes, LA_postcodes, by = c('POSTCODE'='pcds')) %>%
  select(1:5, 14, 15)

# Identify locations of missing practices

missing_practices_locations <- left_join(missing_practices, gp_practices_map, by =c("PRACTICE_CODE"="PracticeCd"))


# Final statements

print(paste0(nrow(missing_practices), ' practices are missing from the GP contract data, accounting for ', sum(missing_practices$TOTAL_PRACTICE_PATIENTS), ' patients in England.'))

