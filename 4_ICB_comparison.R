library(readxl)
library(readODS)

####################################
##### LOAD IN NEW RAW DATA #########
####################################

# Load in ICB mapping data
if(file.exists('Raw_data/ICB_mapping.xlsx')){
  
  ICB_mapping <- read_excel('Raw_data/ICB_mapping.xlsx')
  
} else {
  
  download.file('https://www.arcgis.com/sharing/rest/content/items/1ac8547e9a8945478f0b5ea7ffe1a6b1/data', 'Raw_data/ICB_mapping.xlsx')
  
  ICB_mapping <- read_excel('Raw_data/ICB_mapping.xlsx')

}

# Load in rural/urban data

if(file.exists('Raw_data/DEFRA_rural_urban.ods')){
  
  rural_urban <- read_ods('Raw_data/DEFRA_rural_urban.ods', sheet = '1', skip = 3)
  
} else {
  
  download.file('https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1100178/07_Digest_local_authority_data_tables_Rural_population_August_2022_edition.ods', 'Raw_data/DEFRA_rural_urban.ods')
  
  rural_urban <- read_ods('Raw_data/DEFRA_rural_urban.ods', sheet = '1', skip = 3)
  
}

# Load in IMD data

IMD <- s3read_using(read.csv,
                           object = '/Tom/GP-contract-unpaid-carers/Data/TS011-2021-4.csv',
                           bucket = bucket) 

#####################################
##### PREPARE DATA FOR JOIN #########
#####################################

# Identify LAs administered by more than one ICB
multiple_ICBs <- ICB_mapping %>%
  group_by(LAD22NM) %>%
  summarise(ICBs = n_distinct(ICB22CD)) %>%
 filter(ICBs > 1)

# Assign ICBs to LAs based on the ICB the majority of their LSOAs are administered by
maj_ICBs <- ICB_mapping %>%
  count(LAD22NM, LAD22CD, ICB22NM, ICB22CD) %>%
  group_by(LAD22NM, LAD22CD) %>%
  summarise(maj_ICB = ICB22NM[1], maj_ICB_code = ICB22CD[1])


################################################
###### JOIN ICB DATA TO GP_CENSUS DATA #########
################################################


ICB_join <- left_join(final_table, maj_ICBs, by = c('LA_CODE'='LAD22CD'))

ICB_grouped <- ICB_join %>%
  group_by(maj_ICB) %>%
  summarise(mean(Coverage), sd(Coverage))


#######################################################
#### CLEAN AND JOIN ON IMD AND RURAL/URBAN DATA #######
#######################################################

rural_urban_cleaned <- rural_urban %>%
  select(1:5) %>%
  rename(LA_CODE = 1, LA_NAME = 2, rural_urban_classification = 3, broad_rural_urban_classification = 4, population_2020 = 5)

IMD_cleaned <- IMD %>%
  rename(LA_CODE = 1, LA_NAME = 2, DEP_CATEGORY_CODE = 3, DEP_CATEGORY_NAME = 4, OBSERVATION = 5) %>%
  filter(DEP_CATEGORY_CODE != '-8') %>%
  mutate(DEP = case_when(DEP_CATEGORY_CODE %in% c(2:5) ~ 'DEP_DIMENSION',
                         TRUE ~ 'NO_DEP_DIMENSION')) %>%
  group_by(DEP, LA_CODE, LA_NAME) %>%
  summarise(OBSERVATION = sum(OBSERVATION)) %>%
  pivot_wider(names_from = DEP, values_from = OBSERVATION) %>%
  mutate(PERCENT_NO_DEPRIVATION = NO_DEP_DIMENSION/(DEP_DIMENSION + NO_DEP_DIMENSION))

IMD_rural_join <- left_join((left_join(ICB_join, rural_urban_cleaned, by = c('LA_CODE'))), IMD_cleaned, by = c('LA_CODE'))



#######################################################
############# GRAPH COMPARISONS #######################
#######################################################

ggplot()+
  geom_boxplot(data = IMD_rural_join, aes(x = Coverage, y = broad_rural_urban_classification)) 

ggplot()+
  geom_point(data=IMD_rural_join, aes(x = PERCENT_NO_DEPRIVATION, y = Coverage), color = 'darkblue') +
  theme_minimal() +
  ylab('% of population with no dimensions of deprivation') +
  theme(axis.title = element_text(size = 10))







