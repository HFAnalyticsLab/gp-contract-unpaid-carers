packages <- c('tidyverse', 'here', 'readxl')

installed_packages <- packages %in% row.names(installed.packages())

if (any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)


## Set up folder structure

ifelse(!dir.exists(file.path(here('Raw_data/'))), dir.create(file.path(here('Raw_data/'))), print('Raw data directory already exists'))  

ifelse(!dir.exists(file.path(here('Outputs/'))), dir.create(file.path(here('Outputs/'))), print('Outputs directory already exists'))




### Download raw data


# Load in GP patients by practice and lsoa data

if (file.exists('Raw_data/gp_practice_lsoa_july-23.csv')){
  
  print('GP patients by LSOA data already downloaded.')
  
} else {
  
  temp <- tempfile()
  
  download.file('https://files.digital.nhs.uk/E3/7F080B/gp-reg-pat-prac-lsoa-male-female-July-23.zip', temp)
  
  unzip(temp, files = c('gp-reg-pat-prac-lsoa-all.csv'), exdir = 'Raw_data/' )
  
  unlink(temp)
}


# Load in GP contract data

if (file.exists('Raw_data/core_gp_contract_2023_24_Q1.csv')){
  
  print('GP contract data already downloaded.')
  
} else {
  
  temp <- tempfile()
  
  download.file('https://files.digital.nhs.uk/D3/176AFA/Core_GP_Contract_2324_csv_files.zip', 
                temp)
  
  unzip(temp, files = c('Core GP Contract Q2 2023-24.csv'), exdir = 'Raw_data/')
  
  unlink(temp)
  
}

# Download LSOA -> LA mapping data

#download.file('https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Lookup_for_England_and_Wales/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=false&returnCountOnly=true&outSR=4326&f=json',
 #             destfile = 'Raw_data/LSOA_to_LA_mapping.geojson')

