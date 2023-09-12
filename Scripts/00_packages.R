packages <- c('tidyverse','aws.s3', 'readxl', 'readODS', 'sf', 'plotly', 'geogrid', 'jsonlite', 'leaflet', 'geodata')

installed_packages <- packages %in% row.names(installed.packages())

if (any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)

