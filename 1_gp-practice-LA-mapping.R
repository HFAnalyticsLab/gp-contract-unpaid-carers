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

# Prepare data for join

gp_contract_data <- gp_contract_data %>%
  filter(IND_CODE == "CGPCMI01" & ACH_DATE == "2023-03-31")

gp_contract_data$VALUE <- as.numeric(gp_contract_data$VALUE)

# Join gp datasets
gp_join <- left_join(gp_patients_data, gp_contract_data, by = "PRACTICE_CODE") %>%
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
  select(LSOA11CD, LSOA21CD, LAD22CD, LAD22NM) %>%
  rename(LSOA_CODE = LSOA21CD)

gps_LAs <- left_join(gp_join, LA_mapping, by = "LSOA_CODE")

unmatched_LSOAs <- gps_LAs %>%
  filter(is.na(LAD22CD))


gps_LAs_grouped <- gps_LAs %>%
  mutate_at(vars("CARERS_IN_LSOA"), ~replace_na(.,0)) %>%
  group_by(LAD22CD, LAD22NM) %>%
  summarise(EST_CARERS_IN_LA = sum(CARERS_IN_LSOA)) %>%
  rename(LA_CODE = LAD22CD, LA_NAME = LAD22NM)
