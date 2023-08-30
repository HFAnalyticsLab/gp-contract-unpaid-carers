bucket <- "thf-dap-tier0-projects-ndl-f3b6da96-projectbucket-orxht6uldbv4"

england_carers <- s3read_using(read_excel, sheet = 'Table 5', skip = 3,
                               object = '/Tom/GP-contract-unpaid-carers/Data/sc012021reftablesengland1.xlsx',
                               bucket = bucket)

head(england_carers)


# Comparison by proportion of female carers

england_carers %>%
  filter(`Unpaid Carer Status` == 'Unpaid carer') %>%
  group_by(`Local Authority`, Sex) %>%
  summarise(Count = sum(Count)) %>%
  pivot_wider(names_from = Sex, values_from = Count) %>%
  mutate(prop_female = Female/Persons) %>%
  left_join(IMD_rural_join, by = c('Local Authority' = 'LAD22NM')) %>%
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
  group_by(`Local Authority`, over_65) %>%
  summarise(Count = sum(Count)) %>%
  pivot_wider(names_from = over_65, values_from = Count) %>%
  mutate(prop_over65 = over_65/(under_65 + over_65)) %>%
  left_join(IMD_rural_join, by = c('Local Authority' = 'LAD22NM')) %>%
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
  group_by(`Local Authority`, over_50) %>%
  summarise(Count = sum(Count)) %>%
  pivot_wider(names_from = over_50, values_from = Count) %>%
  mutate(prop_over50 = over_50/(under_50 + over_50)) %>%
  left_join(IMD_rural_join, by = c('Local Authority' = 'LAD22NM')) %>%
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
  group_by(`Local Authority`, under_25) %>%
  summarise(Count = sum(Count)) %>%
  pivot_wider(names_from = under_25, values_from = Count) %>%
  mutate(prop_under25 = under_25/(over_25 + under_25)) %>%
  left_join(IMD_rural_join, by = c('Local Authority' = 'LAD22NM')) %>%
  ggplot()+
  geom_point(aes(y = Coverage, x = prop_under25), color = 'darkblue') +
  theme_minimal() +
  xlab('Proportion of carers under 25') +
  theme(axis.title = element_text(size = 10))



england_carers_intensity <- s3read_using(read_excel, sheet = 'Table 11', skip = 3,
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
  mutate(prop_over50 = `50 or more hours`/(`19 hours or less` + `20 to 49 hours` + `50 or more hours`)) %>%
  left_join(IMD_rural_join, by = c('Area Code' = 'LA_CODE')) %>%
  ggplot()+
  geom_point(aes(y = Coverage, x = prop_over50), color = 'darkblue') +
  theme_minimal() +
  xlab('Proportion of carers with over 50 hours of care') +
  theme(axis.title = element_text(size = 10))


# Only 50+ hours

england_carers_intensity %>%
  filter(Sex == 'Persons') %>%
  group_by(`Local Authority`, `Area Code`, `Unpaid Carer Status`) %>%
  summarise(Count = sum(Count)) %>%
  filter(`Unpaid Carer Status` != 'Non-carer') %>%
  pivot_wider(names_from = `Unpaid Carer Status`, values_from = Count) %>%
  mutate(prop_lowintensity = `19 hours or less`/(`19 hours or less` + `20 to 49 hours` + `50 or more hours`)) %>%
  left_join(IMD_rural_join, by = c('Area Code' = 'LA_CODE')) %>%
  ggplot()+
  geom_point(aes(y = Coverage, x = prop_lowintensity), color = 'darkblue') +
  theme_minimal() +
  xlab('Proportion of carers with under 19 hours of care') +
  theme(axis.title = element_text(size = 10))


