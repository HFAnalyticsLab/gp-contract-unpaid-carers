bucket <- "thf-dap-tier0-projects-ndl-f3b6da96-projectbucket-orxht6uldbv4"


england_carers <- s3read_using(read_excel, sheet = 'Table 5', skip = 3,
                               object = '/Tom/GP-contract-unpaid-carers/Data/sc012021reftablesengland1.xlsx',
                               bucket = bucket)

head(england_carers)


# Comparison by proportion of female carers

england_carers %>%
  filter(`Unpaid Carer Status` == 'Unpaid carer') %>%
  group_by(`Local Authority`, `Area Code`, Sex) %>%
  summarise(Count = sum(Count)) %>%
  pivot_wider(names_from = Sex, values_from = Count) %>%
  mutate(prop_female = Female/Persons) %>%
  left_join(IMD_rural_join, by = c('Area Code' = 'LA_CODE')) %>%
  ggplot()+
  geom_point(aes(y = Coverage, x = prop_female), color = 'darkblue') +
  theme_minimal() +
  xlab('Proportion of female carers') +
  theme(axis.title = element_text(size = 10))
  
  
# Comparison by proportion of carers over 65


england_carers %>%
  filter(`Unpaid Carer Status` == 'Unpaid carer' & Sex == 'Persons') %>%
  mutate(over_65 = case_when(Age %in% c('65 to 69', '70 to 74', '75 to 79', '80 to 84', '85 to 89', '90+') ~ 'over_65',
                             TRUE ~ 'under_65')) %>%
  group_by(`Local Authority`, `Area Code`, over_65) %>%
  summarise(Count = sum(Count)) %>%
  pivot_wider(names_from = over_65, values_from = Count) %>%
  mutate(prop_over65 = over_65/(under_65 + over_65)) %>%
  left_join(IMD_rural_join, by = c('Area Code' = 'LA_CODE')) %>%
  ggplot()+
  geom_point(aes(y = Coverage, x = prop_over65), color = 'darkblue') +
  theme_minimal() +
  xlab('Proportion of carers over 65') +
  theme(axis.title = element_text(size = 10))

  
# Comparison by proportion of carers over 50

england_carers %>%
  filter(`Unpaid Carer Status` == 'Unpaid carer' & Sex == 'Persons') %>%
  mutate(over_50 = case_when(Age %in% c('50 to 54','55 to 59', '60 to 64','65 to 69', '70 to 74', '75 to 79', '80 to 84', '85 to 89', '90+') ~ 'over_50',
                             TRUE ~ 'under_50')) %>%
  group_by(`Local Authority`, `Area Code`, over_50) %>%
  summarise(Count = sum(Count)) %>%
  pivot_wider(names_from = over_50, values_from = Count) %>%
  mutate(prop_over50 = over_50/(under_50 + over_50)) %>%
  left_join(IMD_rural_join, by = c('Area Code' = 'LA_CODE')) %>%
  ggplot()+
  geom_point(aes(y = Coverage, x = prop_over50), color = 'darkblue') +
  theme_minimal() +
  xlab('Proportion of carers over 50') +
  theme(axis.title = element_text(size = 10))

  

# Comparison by proportion of carers under 25
  
england_carers %>%
  filter(`Unpaid Carer Status` == 'Unpaid carer' & Sex == 'Persons') %>%
  mutate(under_25 = case_when(Age %in% c('05 to 17','18 to 24') ~ 'under_25',
                             TRUE ~ 'over_25')) %>%
  group_by(`Local Authority`, `Area Code`, under_25) %>%
  summarise(Count = sum(Count)) %>%
  pivot_wider(names_from = under_25, values_from = Count) %>%
  mutate(prop_under25 = under_25/(over_25 + under_25)) %>%
  left_join(IMD_rural_join, by = c('Area Code' = 'LA_CODE')) %>%
  ggplot()+
  geom_point(aes(y = Coverage, x = prop_under25), color = 'darkblue') +
  theme_minimal() +
  xlab('Proportion of carers under 25') +
  theme(axis.title = element_text(size = 10))

## Intensity of care

england_carers_intensity <- s3read_using(read_excel, sheet = 'Table 18', skip = 3,
                               object = '/Tom/GP-contract-unpaid-carers/Data/sc012021reftablesengland1.xlsx',
                               bucket = bucket)

head(england_carers_intensity)

# Only 50+ hours

england_carers_intensity$Count <- as.numeric(england_carers_intensity$Count)

england_carers_intensity$Count[is.na(england_carers_intensity$Count)] <- 0

england_carers_intensity %>%
  filter(Sex == 'Persons') %>%
  group_by(`Local Authority`, `Area Code`, `Unpaid Carer Status`) %>%
  summarise(Count = sum(Count)) %>%
  filter(`Unpaid Carer Status` != 'Non-carer') %>%
  pivot_wider(names_from = `Unpaid Carer Status`, values_from = Count) %>%
  mutate(prop_over50 = `50 or more hours`/(`9 hours or less` + `10 to 19 hours` + `20 to 34 hours` + `35 to 49 hours` + `50 or more hours`)) %>%
  left_join(IMD_rural_join, by = c('Area Code' = 'LA_CODE')) %>%
  ggplot()+
  geom_point(aes(y = Coverage, x = prop_over50), color = 'darkblue') +
  theme_minimal() +
  xlab('Proportion of carers with over 50 hours of care') +
  theme(axis.title = element_text(size = 10))


# 19 hours or less

england_carers_intensity %>%
  filter(Sex == 'Persons') %>%
  group_by(`Local Authority`, `Area Code`, `Unpaid Carer Status`) %>%
  summarise(Count = sum(Count)) %>%
  filter(`Unpaid Carer Status` != 'Non-carer') %>%
  pivot_wider(names_from = `Unpaid Carer Status`, values_from = Count) %>%
  mutate(prop_lowintensity = `9 hours or less`/(`9 hours or less` + `10 to 19 hours` + `20 to 34 hours` + `35 to 49 hours` + `50 or more hours`)) %>%
  left_join(IMD_rural_join, by = c('Area Code' = 'LA_CODE')) %>%
  ggplot()+
  geom_point(aes(y = Coverage, x = prop_lowintensity), color = 'darkblue') +
  theme_minimal() +
  xlab('Proportion of carers with under 9 hours of care') +
  theme(axis.title = element_text(size = 10))


#####################################
############# JOINS #################
#####################################

prop_female_carers <- england_carers %>%
  filter(`Unpaid Carer Status` == 'Unpaid carer') %>%
  replace_na(list(Count = 0)) %>%
  group_by(`Area Code`, Sex) %>%
  summarise(Count = sum(Count)) %>%
  pivot_wider(names_from = Sex, values_from = Count) %>%
  mutate(prop_female = Female/Persons) %>%
  select(`Area Code`, prop_female)


over_65 <- england_carers %>%
  filter(`Unpaid Carer Status` == 'Unpaid carer' & Sex == 'Persons') %>%
  mutate(over_65 = case_when(Age %in% c('65 to 69', '70 to 74', '75 to 79', '80 to 84', '85 to 89', '90+') ~ 'over_65',
                             TRUE ~ 'under_65')) %>%
  replace_na(list(Count = 0)) %>%
  group_by(`Area Code`, over_65) %>%
  summarise(Count = sum(Count)) %>%
  pivot_wider(names_from = over_65, values_from = Count) %>%
  mutate(prop_over65 = over_65/(under_65 + over_65)) %>%
  select(`Area Code`, prop_over65)
  


under_25 <- england_carers %>%
  filter(`Unpaid Carer Status` == 'Unpaid carer' & Sex == 'Persons') %>%
  mutate(under_25 = case_when(Age %in% c('05 to 17','18 to 24') ~ 'under_25',
                              TRUE ~ 'over_25')) %>%
  replace_na(list(Count = 0)) %>%
  group_by(`Area Code`, under_25) %>%
  summarise(Count = sum(Count)) %>%
  pivot_wider(names_from = under_25, values_from = Count) %>%
  mutate(prop_under25 = under_25/(over_25 + under_25)) %>%
  select(`Area Code`, prop_under25)


over_50hours <- england_carers_intensity %>%
  filter(Sex == 'Persons') %>%
  replace_na(list(Count = 0)) %>%
  group_by(`Area Code`, `Unpaid Carer Status`) %>%
  summarise(Count = sum(Count)) %>%
  filter(`Unpaid Carer Status` != 'Non-carer') %>%
  pivot_wider(names_from = `Unpaid Carer Status`, values_from = Count) %>%
  mutate(prop_over50hours = `50 or more hours`/(`9 hours or less` + `10 to 19 hours` + `20 to 34 hours` + `35 to 49 hours` + `50 or more hours`)) %>%
  select(`Area Code`, prop_over50hours)


below_19hours <- england_carers_intensity %>%
  filter(Sex == 'Persons') %>%
  replace_na(list(Count = 0)) %>%
  group_by(`Area Code`, `Unpaid Carer Status`) %>%
  summarise(Count = sum(Count)) %>%
  filter(`Unpaid Carer Status` != 'Non-carer') %>%
  pivot_wider(names_from = `Unpaid Carer Status`, values_from = Count) %>%
  mutate(prop_lowintensity = `9 hours or less`/(`9 hours or less` + `10 to 19 hours` + `20 to 34 hours` + `35 to 49 hours` + `50 or more hours`)) %>%
  select(`Area Code`, prop_lowintensity)

all_variables_joined <- IMD_rural_join %>%
  left_join(prop_female_carers, by = c('LA_CODE' = 'Area Code')) %>%
  left_join(over_65, by = c('LA_CODE' = 'Area Code')) %>%
  left_join(under_25, by = c('LA_CODE' = 'Area Code')) %>%
  left_join(over_50hours, by = c('LA_CODE' = 'Area Code')) %>%
  left_join(below_19hours, by = c('LA_CODE' = 'Area Code')) %>%
  select(LA_CODE, LA_NAME, EST_CARERS_IN_LA, CENSUS_NO_OF_CARERS, Difference, Coverage, MISSING_PRACTICES, MISSING_PATIENTS, 
         maj_ICB, maj_ICB_code, rural_urban_classification, broad_rural_urban_classification, population_2020, PERCENT_DEPRIVATION, 
         prop_female, prop_over65, prop_under25, prop_over50hours, prop_lowintensity)

all_variables_filtered <- all_variables_joined %>%
  filter(!(is.na(prop_female)))

cor(all_variables_filtered$Coverage, all_variables_filtered$prop_female)
cor(all_variables_filtered$Coverage, all_variables_filtered$prop_over65)
cor(all_variables_filtered$Coverage, all_variables_filtered$prop_under25)
cor(all_variables_filtered$Coverage, all_variables_filtered$prop_over50hours)
cor(all_variables_filtered$Coverage, all_variables_filtered$prop_lowintensity)

s3write_using(all_variables_joined,
              write.csv,
              object = '/Tom/GP-contract-unpaid-carers/Outputs/all_variables_joined.csv',
              bucket = bucket)

