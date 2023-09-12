
# Set bucket
bucket <- "thf-dap-tier0-projects-ndl-f3b6da96-projectbucket-orxht6uldbv4"


######################################
###### LOAD IN PROCESSED DATA ########
######################################

# Load in processed GP data

if (file.exists('Processed_data/gps_LAs.rds')){
  gps_LAs <- readRDS('Processed_data/gps_LAs.rds')
} else {
  source('1_gp-practice-LA-mapping.R')
  gps_LAs <- read.csv('Processed_data/gps_LAs.rds')
}

if (file.exists('Processed_data/gps_LAs_grouped.rds')){
  gps_LAs_grouped <- readRDS('Processed_data/gps_LAs_grouped.rds')
} else {
  source('1_gp-practice-LA-mapping.R')
  gps_LAs_grouped <- readRDS('Processed_data/gps_LAs_grouped.rds')
}


################################################
######### LOAD IN & PREPARE RAW DATA ###########
################################################


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
  theme_minimal() +
  ylab('Count of LAs')


# Overarching statements

print(paste0("The census records ", sum(census_carers$CENSUS_NO_OF_CARERS), " unpaid carers, while the GP contract data records ", sum(gps_LAs_grouped$EST_CARERS_IN_LA)))

print(paste0('Mean coverage of unpaid carers in the GP contract data is ', round(mean(gp_census_join$Coverage), 2), '% per LA compared to the census, with a standard deviation of ', round(sd(gp_census_join$Coverage)*100,2), ' percentage points.'))


## Write joined census data to csv for later use

saveRDS(gp_census_join, 'Processed_data/gp_census_join.rds')
