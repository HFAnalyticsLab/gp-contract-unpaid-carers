#############################################################

# Written in R 4.0.2

###########################
###### LOAD IN DATA #######
###########################
  
gp_patients_data <- read_csv('Raw_data/gp-reg-pat-prac-lsoa-all.csv')

gp_contract_data <- read_csv('Raw_data/Core GP Contract Q2 2023-24.csv')
  

# Load in LSOA -> LA mapping data

#LA_mapping <- read_csv('Raw_data/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales_(Version_2).csv')

LA_mapping <- readRDS('Resources/LA_to_LSOA_mapping.rds')

# Load in census data

census_carers <- readRDS('Resources/carers_2021_census.rds')

#####################################
####### CLEAN & JOIN GP DATA ########
#####################################


# Prepare contract data for join
gp_contract_data <- gp_contract_data %>%
  filter(IND_CODE == "CGPCMI01" & ACH_DATE == "2023-09-30")

gp_contract_data$VALUE <- as.numeric(gp_contract_data$VALUE)


# The GP patients by LSOA data 

gp_patients_data <- gp_patients_data %>%
  filter(str_detect(LSOA_CODE, "^E")|str_detect(LSOA_CODE, "^N"))

patients_noLSOA <- gp_patients_data %>%
  filter(LSOA_CODE == "NO2011")

fixed_gp_patients <- gp_patients_data %>%
  group_by(PRACTICE_CODE) %>%
  mutate(MAX_LSOA = LSOA_CODE[which.max(NUMBER_OF_PATIENTS)]) %>%
  dplyr::ungroup() %>%
  mutate(LSOA_CODE_FIXED = case_when(LSOA_CODE == "NO2011" ~ MAX_LSOA,
                                 TRUE ~ LSOA_CODE))

fixed_gp_patients$LSOA_CODE <- fixed_gp_patients$LSOA_CODE_FIXED

# Join gp datasets
gp_join <- full_join(fixed_gp_patients, gp_contract_data, by = "PRACTICE_CODE") %>%
  select(PRACTICE_CODE, LSOA_CODE, NUMBER_OF_PATIENTS, VALUE) %>%
  rename(TOTAL_UNPAID_CARERS = VALUE) %>%
  group_by(PRACTICE_CODE) %>%
  mutate(TOTAL_PRACTICE_PATIENTS = sum(NUMBER_OF_PATIENTS)) %>%
  dplyr::ungroup() %>%
  mutate(PATIENT_PROPORTION = NUMBER_OF_PATIENTS/TOTAL_PRACTICE_PATIENTS) %>%
  mutate(CARERS_IN_LSOA = TOTAL_UNPAID_CARERS*PATIENT_PROPORTION)


############################################################
##### JOIN WITH LA DATA AND ESTIMATE CARERS PER LA #########
############################################################

# Get rid of duplicates in the LA mapping data - these appear when a 201 LSOA code has been discontinued
LA_mapping <- LA_mapping %>%
  group_by(LSOA11CD, LSOA11NM) %>%
  summarise(LA_CODE = first(LAD22CD), LA_NAME = first(LAD22NM)) %>%
  rename(LSOA_CODE = LSOA11CD)

# Join GP and LA data
gps_LAs <- left_join(gp_join, LA_mapping, by = "LSOA_CODE")

# Check for LSOAs that have not been successfully mapped to an LA
unmatched_LSOAs <- gps_LAs %>%
  filter(is.na(LA_NAME))

gps_LAs <- gps_LAs %>%
  filter(!(is.na(LA_NAME)))

# Aggregate to an LA level
gps_LAs_grouped <- gps_LAs %>%
  mutate_at(vars("CARERS_IN_LSOA"), ~replace_na(.,0)) %>%
  group_by(LA_CODE, LA_NAME) %>%
  summarise(EST_CARERS_IN_LA = sum(CARERS_IN_LSOA))


# Summaries and error checks

print(paste0("In the raw GP patients data, there are ",sum(patients_noLSOA$NUMBER_OF_PATIENTS), " patients with no associated LSOA code, out of ", sum(gp_patients_data$NUMBER_OF_PATIENTS), " registered patients total."))

print(paste0("This accounts for ", round(sum(patients_noLSOA$NUMBER_OF_PATIENTS)/sum(gp_patients_data$NUMBER_OF_PATIENTS)*100, 3), "% of patients included in the Patients Registered at a GP Practice data"))

print(paste0("After processing, there are now ", nrow(unmatched_LSOAs), " unmatched LSOA codes"))


# Clear large unnecessary data from the workspace
rm(gp_contract_data, gp_patients_data, fixed_gp_patients, gp_join, patients_noLSOA, unmatched_LSOAs)


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
############# OUTPUTS ##############
####################################

write.csv(gps_LAs, 'Outputs/GP_unpaid_carers_by_LSOA.csv')

write.csv(gp_census_join, 'Outputs/GP_contract_to_census_LA_comparison.csv')

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


