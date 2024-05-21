
# Download other data
if (file.exists('Raw_data/all_releases/Core GP Contract Q1 2023-24.csv')){
  
  print('GP contract data 23/24 already downloaded.')
  
} else {
  
  temp <- tempfile()
  
  download.file('https://files.digital.nhs.uk/37/E77B1F/Core_GP_Contract_2324_csv_files.zip', 
                temp)
  
  unzip(temp, exdir = 'Raw_data/all_releases')
  
  unlink(temp)
  
}

if (file.exists('Raw_data/all_releases/Core GP Contract Q1 2022-23.csv')){
  
  print('GP contract data 22/23 already downloaded.')
  
} else {
  
  temp <- tempfile()
  
  download.file('https://files.digital.nhs.uk/68/993E73/Core%20GP%20Contract%202022-23%20csv%20files.zip', 
                temp)
  
  unzip(temp, exdir = 'Raw_data/all_releases')
  
  unlink(temp)
  
}

# Patients registered in GP practice, by quarter
Q1_22_23_link <- 'https://files.digital.nhs.uk/0E/59E17A/gp-reg-pat-prac-lsoa-male-female-July-2022.zip'
Q2_22_23_link <- 'https://files.digital.nhs.uk/AA/AE3EDC/gp-reg-pat-prac-lsoa-male-female-oct-22..zip'
Q3_22_23_link <- 'https://files.digital.nhs.uk/FB/567A47/gp-reg-pat-prac-lsoa-male-female-Jan-23.zip'
Q4_22_23_link <- 'https://files.digital.nhs.uk/AA/B3CF39/gp-reg-pat-prac-lsoa-male-female-April-23.zip'
Q1_23_24_link <- 'https://files.digital.nhs.uk/E3/7F080B/gp-reg-pat-prac-lsoa-male-female-July-23.zip'
Q2_23_24_link <- 'https://files.digital.nhs.uk/9D/7C8EBC/gp-reg-pat-prac-lsoa-male-female-Oct-23.zip'
Q3_23_24_link <- 'https://files.digital.nhs.uk/A0/3479F2/gp-reg-pat-prac-lsoa-male-female-Jan-24.zip'
Q4_23_24_link <- 'https://files.digital.nhs.uk/5C/704155/gp-reg-pat-prac-lsoa-male-female-Apr-24.zip'

quarters <- c('Q1_22_23', 'Q2_22_23', 'Q3_22_23', 'Q4_22_23', 'Q1_23_24', 'Q2_23_24', 'Q3_23_24', 'Q4_23_24')

links <- list(Q1_22_23_link, Q2_22_23_link, Q3_22_23_link, Q4_22_23_link, Q1_23_24_link, Q2_23_24_link, Q3_23_24_link, Q4_23_24_link)

for(i in 1:length(links)){
 
 if (file.exists(paste0('Raw_data/all_releases/gp-reg-pat-prac-lsoa-all_', quarters[[i]], '.csv'))){
  
   print(paste0('GP patients by LSOA data already downloaded for ', quarters[[i]]))
  
 } else {
  
   temp <- tempfile()
  
   download.file(links[[i]], temp)
  
   unzip(temp, files = c('gp-reg-pat-prac-lsoa-all.csv'), exdir = 'Raw_data/all_releases' )
  
   unlink(temp)
   
   file.rename('Raw_data/all_releases/gp-reg-pat-prac-lsoa-all.csv', paste0('Raw_data/all_releases/gp-reg-pat-prac-lsoa-all_', quarters[[i]], '.csv'))
   
 }
}

quarters_restated <- c('Q1 2022-23', 'Q2 2022-23', 'Q3 2022-23', 'Q4 2022-23', 'Q1 2023-24', 'Q2 2023-24', 'Q3 2023-24', 'Q4 2023-24')

dates <- c("2022-06-30", "2022-09-30", "2022-12-31", "2023-03-31", "2023-06-30", "2023-09-30", "2023-12-31", "2024-03-31")

test <- read_csv(paste0('Raw_data/all_releases/Core GP Contract Q1 2023-24.csv'))

test$ACH_DATE <- as_date(test$ACH_DATE)  


all_joins <- lapply(1:length(quarters), function(i){ 

  gp_patients_data <- read_csv(paste0('Raw_data/all_releases/gp-reg-pat-prac-lsoa-all_', quarters[[i]],'.csv'))
  
  gp_contract_data <- read_csv(paste0('Raw_data/all_releases/Core GP Contract ', quarters_restated[[i]], '.csv'))
  
  date <- dates[[i]]  
  
# Prepare contract data for join

if (date == '2023-06-30'){
gp_contract_data <- gp_contract_data %>%
  filter(IND_CODE == "CGPCMI01" & ACH_DATE == '30/06/2023')
} else {
  gp_contract_data <- gp_contract_data %>%
    filter(IND_CODE == "CGPCMI01" & ACH_DATE == date)
}

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



# Summaries and error checks

print(paste0("In the raw GP patients data, there are ",sum(patients_noLSOA$NUMBER_OF_PATIENTS), " patients with no associated LSOA code, out of ", sum(gp_patients_data$NUMBER_OF_PATIENTS), " registered patients total."))

print(paste0("This accounts for ", round(sum(patients_noLSOA$NUMBER_OF_PATIENTS)/sum(gp_patients_data$NUMBER_OF_PATIENTS)*100, 3), "% of patients included in the Patients Registered at a GP Practice data"))

return(gp_join)
})

all_missing_practices <- lapply(1:length(all_joins), function(i){ 
  
 missing_practices <- all_joins[[i]][is.na(all_joins[[i]]$TOTAL_UNPAID_CARERS),] %>%
  group_by(PRACTICE_CODE) %>%
  summarise(TOTAL_PRACTICE_PATIENTS = median(TOTAL_PRACTICE_PATIENTS)) %>%

 return(missing_practices)
 
})

no_missing_practices <- sapply(1:length(all_missing_practices), function(i){ 
  n_distinct(all_missing_practices[[i]]$PRACTICE_CODE)
  })

no_missing_patients <- sapply(1:length(all_missing_practices), function(i){ 
  sum(all_missing_practices[[i]]$TOTAL_PRACTICE_PATIENTS)
})

missingness_df <- data.frame(as_date(dates), no_missing_practices, no_missing_patients) %>%
  rename(date = 1, practices = 2, patients = 3)

ggplot() +
  geom_area(data = missingness_df, aes(y = patients, x = date)) +
  geom_line(data = missingness_df, aes(y = practices, x = date)) + 
  scale_y_continuous(sec.axis = sec_axis(name =  "no_missing_practices", ~.))
