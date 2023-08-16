library(tidyverse)
library(aws.s3)

# Set bucket
bucket <- "thf-dap-tier0-projects-ndl-f3b6da96-projectbucket-orxht6uldbv4"

# Load in data

gp_patients_data <- s3read_using(read.csv,
                                  object = '/Tom/GP-contract-unpaid-carers/Data/gp-reg-pat-prac-lsoa-all.csv',
                                  bucket = bucket)

gp_contract_data <- s3read_using(read.csv,
                                 object = '/Tom/GP-contract-unpaid-carers/Data/Core GP Contract Q4 2022-23.csv',
                                 bucket = bucket)

# Prepare contract data for join
gp_contract_data <- gp_contract_data %>%
  filter(IND_CODE == "CGPCMI01" & ACH_DATE == "2023-03-31")

gp_contract_data$VALUE <- as.numeric(gp_contract_data$VALUE)

# Scope and fix LA problem in gp patients data

patients_noLSOA <- gp_patients_data %>%
  filter(LSOA_CODE == "NO2011")

print(paste0("There are ",sum(patients_noLSOA$NUMBER_OF_PATIENTS), " patients with no associated LSOA code."))

print(paste0("This accounts for ", round(sum(patients_noLSOA$NUMBER_OF_PATIENTS)/sum(gp_patients_data$NUMBER_OF_PATIENTS)*100, 3), "% of patients included in the Patients Registered at a GP Practice data"))



fixed_gp_patients <- gp_patients_data %>%
  group_by(PRACTICE_CODE) %>%
  mutate(MAX_LSOA = LSOA_CODE[which.max(NUMBER_OF_PATIENTS)]) %>%
  dplyr::ungroup() %>%
  mutate(LSOA_CODE_FIXED = case_when(LSOA_CODE == "NO2011" ~ MAX_LSOA,
                                 TRUE ~ LSOA_CODE))

fixed_gp_patients$LSOA_CODE <- fixed_gp_patients$LSOA_CODE_FIXED

# Join gp datasets
gp_join <- left_join(fixed_gp_patients, gp_contract_data, by = "PRACTICE_CODE") %>%
  select(PRACTICE_CODE, LSOA_CODE, NUMBER_OF_PATIENTS, VALUE) %>%
  rename(TOTAL_UNPAID_CARERS = VALUE) %>%
  group_by(PRACTICE_CODE) %>%
  mutate(TOTAL_PRACTICE_PATIENTS = sum(NUMBER_OF_PATIENTS)) %>%
  dplyr::ungroup() %>%
  mutate(PATIENT_PROPORTION = NUMBER_OF_PATIENTS/TOTAL_PRACTICE_PATIENTS) %>%
  mutate(CARERS_IN_LSOA = TOTAL_UNPAID_CARERS*PATIENT_PROPORTION)


# Bring in LA mapping data
LA_mapping <- s3read_using(read.csv,
             object = '/Tom/GP-contract-unpaid-carers/Data/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales_(Version_2) (1).csv',
             bucket = bucket) 

LA_mapping <- LA_mapping %>%
  group_by(LSOA11CD, LSOA11NM) %>%
  summarise(LA_CODE = first(LAD22CD), LA_NAME = first(LAD22NM)) %>%
  rename(LSOA_CODE = LSOA11CD)

gps_LAs <- left_join(gp_join, LA_mapping, by = "LSOA_CODE")

unmatched_LSOAs <- gps_LAs %>%
  filter(is.na(LA_NAME))

print(paste0("There are ", nrow(unmatched_LSOAs), " unmatched LSOA codes"))

gps_LAs_grouped <- gps_LAs %>%
  mutate_at(vars("CARERS_IN_LSOA"), ~replace_na(.,0)) %>%
  group_by(LA_CODE, LA_NAME) %>%
  summarise(EST_CARERS_IN_LA = sum(CARERS_IN_LSOA))
