library(tidyverse)
library(aws.s3)
library(here)
library(readxl)

# Set bucket
bucket <- "thf-dap-tier0-projects-ndl-f3b6da96-projectbucket-orxht6uldbv4"

###########################
###### LOAD IN DATA #######
###########################


# Load in GP patients by practice and lsoa data

if (file.exists('gp-reg-pat-prac-lsoa-all.csv')){
  
  gp_patients_data <- read.csv('gp-reg-pat-prac-lsoa-all.csv')
  
} else {
  
  temp <- tempfile()
  
  download.file('https://files.digital.nhs.uk/E3/7F080B/gp-reg-pat-prac-lsoa-male-female-July-23.zip', temp)
  
  gp_patients_data <- read.csv(unz(temp, 'gp-reg-pat-prac-lsoa-all.csv'))
  
  unlink(temp)
  
}

# Load in GP contract data

if (file.exists('Core GP Contract Q1 2023-24.csv')){
  
  gp_contract_data <- read.csv('Core GP Contract Q1 2023-24.csv')
  
} else {
  
  temp <- tempfile()
  
  download.file('https://files.digital.nhs.uk/C2/54C677/Core%20GP%20Contract_2324%20csv%20files.zip', 
                temp)
  
  gp_contract_data <- read.csv(unz(temp, 'Core GP Contract Q1 2023-24.csv'))
  
  unlink(temp)
  
}


# Load in LSOA -> LA mapping data

LA_mapping <- s3read_using(read.csv,
                           object = '/Tom/GP-contract-unpaid-carers/Data/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales_(Version_2) (1).csv',
                           bucket = bucket) 

#####################################
####### CLEAN & JOIN GP DATA ########
#####################################


# Prepare contract data for join
gp_contract_data <- gp_contract_data %>%
  filter(IND_CODE == "CGPCMI01" & ACH_DATE == "30/06/2023")

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


## Write the gp_join and gps_LA_grouped to csv for later use

write.csv(gps_LAs, 'Processed_data/gps_LAs.csv')
write.csv(gps_LAs_grouped, 'Processed_data/gps_LAs_grouped.csv')
