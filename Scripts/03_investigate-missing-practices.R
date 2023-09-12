

# Set bucket
bucket <- "thf-dap-tier0-projects-ndl-f3b6da96-projectbucket-orxht6uldbv4"

####################################
##### LOAD IN PROCESSED DATA #######
####################################

if (file.exists('Processed_data/gps_LAs.rds')){
  gps_LAs <- readRDS('Processed_data/gps_LAs.rds')
} else {
  source('1_gp-practice-LA-mapping.R')
  gps_LAs <- readRDS('Processed_data/gps_LAs.rds')
}

if (file.exists('Processed_data/gps_LAs_grouped.rds')){
  gps_LAs_grouped <- readRDS('Processed_data/gps_LAs_grouped.rds')
} else {
  source('1_gp-practice-LA-mapping.R')
  gps_LAs_grouped <- readRDS('Processed_data/gps_LAs_grouped.rds')
}

if (file.exists('Processed_data/gp_census_join.rds')){
  gp_census_join <- readRDS('Processed_data/gp_census_join.rds')
} else {
  source('2_gp-contract-census-comparisons.R')
  gp_census_join <- readRDS('Processed_data/gp_census_join.rds')
}


####################################
##### LOAD IN NEW RAW DATA #########
####################################

# Load in data on postcodes of GP practices
if (file.exists('gp_practice_addresses.rds')){
    
    gp_postcodes <- read.csv('gp_practice_addresses.rds')
    
  } else {
    
    temp <- tempfile()
    
    download.file('https://files.digital.nhs.uk/assets/ods/current/epraccur.zip', temp)
    
    
    gp_postcodes <- read.csv(unz(temp, 'epraccur.csv'), header = FALSE) %>%
      select(1:10)
    
    names(gp_postcodes) <- c('PRACTICE_CODE', 'NAME', 'GROUPING', 'GEOG', 'ADDRESS_1', 
                             'ADDRESS_2', 'ADDRESS_3', 'ADDRESS_4', 'ADDRESS_5', 'POSTCODE')
    
    saveRDS(gp_postcodes, 'Raw_data/gp_practice_addresses.rds')
    
    unlink(temp)
  }

# Load in lookup data for postcodes -> LSOA. Warning, very large dataset
if (file.exists('Raw_data/postcode_lookup_may_23.rds')){
  
  postcode_locations <- readRDS('Raw_data/postcode_lookup_may_23.rds')
  
} else {
  
  temp <- tempfile()
  
  download.file('https://www.arcgis.com/sharing/rest/content/items/b86748732a054592bcf0218e86a43870/data', temp)
  
  postcode_locations <- read.csv(unz(temp, 'Data/NSPL21_MAY_2023_UK.csv')) %>%
    select(pcds, lsoa21)
  
  saveRDS(postcode_locations, "Raw_data/postcode_lookup_may_23.rds")
  
  unlink(temp)
  
  
}

LA_mapping <- s3read_using(read.csv,
                           object = '/Tom/GP-contract-unpaid-carers/Data/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales_(Version_2) (1).csv',
                           bucket = bucket) 


##################################################
######### INVESTIGATE MISSING PRACTICES ##########
##################################################

# Isolate missing practices

missing_practices <- gps_LAs[is.na(gps_LAs$TOTAL_UNPAID_CARERS),] %>%
  group_by(PRACTICE_CODE) %>%
  summarise(TOTAL_PRACTICE_PATIENTS = median(TOTAL_PRACTICE_PATIENTS))

# Identify locations of all practices

gp_bypractice <- gps_LAs %>%
  group_by(PRACTICE_CODE) %>%
  summarise(TOTAL_PRACTICE_PATIENTS = median(TOTAL_PRACTICE_PATIENTS), TOTAL_UNPAID_CARERS = median(TOTAL_UNPAID_CARERS))

all_practice_postcodes <- left_join(gp_bypractice, gp_postcodes, by =c("PRACTICE_CODE")) %>%
  select(PRACTICE_CODE, TOTAL_PRACTICE_PATIENTS, TOTAL_UNPAID_CARERS, NAME, POSTCODE)

all_practice_lsoas <- left_join(all_practice_postcodes, postcode_locations, by = c('POSTCODE'='pcds'))
  
rm(postcode_locations, all_practice_postcodes, gp_postcodes)

LA_mapping <- LA_mapping %>%
  group_by(LSOA21CD, LSOA21NM) %>%
  summarise(LA_CODE = first(LAD22CD), LA_NAME = first(LAD22NM))

all_practice_LAs <- left_join(all_practice_lsoas, LA_mapping, by = c('lsoa21'='LSOA21CD'))

rm(all_practice_lsoas)

# Identify locations of missing practices

missing_practices_locations <- left_join(missing_practices, all_practice_LAs, by="PRACTICE_CODE")

missing_practices_grouped <- missing_practices_locations %>%
  group_by(LA_CODE, LA_NAME) %>%
  summarise(MISSING_PRACTICES = n_distinct(PRACTICE_CODE), MISSING_PATIENTS = sum(TOTAL_PRACTICE_PATIENTS.x))

# Join data on missing practices to gp_census data

missing_comparison <- left_join(gp_census_join, missing_practices_grouped, by='LA_CODE')

# Remove NAs and graph missing patients against coverage
missing_comparison$MISSING_PATIENTS[is.na(missing_comparison$MISSING_PATIENTS)] <- 0

ggplot()+
  geom_point(data=missing_comparison, aes(x = Coverage, y = MISSING_PATIENTS), color = 'darkblue') +
  theme_minimal() +
  ylab('Patients registered to missing practices') +
  theme(axis.title = element_text(size = 10))


# Prepare final table for output

final_table <- missing_comparison %>%
  arrange(Coverage, decreasing = FALSE)

write.csv(final_table, 'Outputs/final_table.csv')

# Final statements

print(paste0(nrow(missing_practices), ' practices are missing from the GP contract data, accounting for ', sum(missing_practices$TOTAL_PRACTICE_PATIENTS), ' patients in England.'))

