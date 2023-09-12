bucket <- "thf-dap-tier0-projects-ndl-f3b6da96-projectbucket-orxht6uldbv4"

england_carers <- s3read_using(read_excel, sheet = 'Table 5', skip = 3,
                              object = '/Tom/GP-contract-unpaid-carers/Data/sc012021reftablesengland1.xlsx',
                              bucket = bucket)

wales_carers <- s3read_using(read_excel, sheet = 'Table 3', skip = 3,
                               object = '/Tom/GP-contract-unpaid-carers/Data/sc012021reftableswales1.xlsx',
                               bucket = bucket)


england_carers_ndl <- england_carers %>%
  filter(`Local Authority` %in% c('Leeds', 'Liverpool','Wirral') & Sex == 'Persons' & `Unpaid Carer Status` == 'Unpaid carer') %>%
  filter(Age != '05 to 17') %>%
  mutate(age_brackets = case_when(Age %in% c('18 to 24', '25 to 29') ~ '18-29',
                                  Age %in% c('30 to 34', '35 to 39') ~ '30-39',
                                  Age %in% c('40 to 44', '45 to 49') ~ '40-49',
                                  Age %in% c('50 to 54', '55 to 59') ~ '50-59',
                                  Age %in% c('60 to 64', '65 to 69') ~ '60-69',
                                  Age %in% c('70 to 74', '75 to 79') ~ '70-79',
                                  Age %in% c('80 to 84', '85 to 89', '90+') ~ '80+',
                                  TRUE ~ 'Under 18')) %>%
  mutate(final_LA = case_when(`Local Authority` %in% c('Liverpool', 'Wirral') ~ 'Liverpool and Wirral',
         TRUE ~ `Local Authority`)) %>%
  group_by(final_LA, age_brackets) %>%
  summarise(Count = sum(Count), Population = sum(Population)) %>%
  group_by(final_LA) %>%
  mutate(proportion_in_age_band = Count/sum(Count)) %>%
  dplyr::ungroup()

wales_carers_ndl <- wales_carers %>%
  filter(`Local Authority` %in% c('Neath Port Talbot', 'Swansea') & Sex == 'Persons' & `Unpaid Carer Status` == 'Unpaid carer') %>%
  mutate(age_brackets = case_when(Age %in% c('40 to 44', '45 to 49') ~ '40-49',
                                  Age %in% c('50 to 54', '55 to 59') ~ '50-59',
                                  Age %in% c('60 to 64', '65 to 69') ~ '60-69',
                                  Age %in% c('70 to 74', '75 to 79') ~ '70-79',
                                  Age %in% c('80 to 84', '85 to 89', '90+') ~ '80+',
                                  TRUE ~ 'Under 40')) %>%
  group_by(`Local Authority`, age_brackets) %>%
  summarise(Count = sum(as.numeric(Count)), Population = sum(as.numeric(Population))) %>%
  rename(final_LA = `Local Authority`) %>%
  mutate(proportion_in_age_band = Count/sum(Count)) %>%
  group_by(final_LA) %>%
  mutate(proportion_in_age_band = Count/sum(Count)) %>%
  dplyr::ungroup()

all_carers_ndl_LAs <- rbind(england_carers_ndl, wales_carers_ndl) 

s3write_using(all_carers_ndl_LAs,
              write.csv,
              object = '/Tom/census_carers_ndl_LAs.csv',
              bucket = bucket)

