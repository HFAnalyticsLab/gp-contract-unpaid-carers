library(sf)
library(plotly)
install.packages('geogrid')
library(geogrid)
library(devtools)
install.packages('devtools')
library(devtools)
install_github("psychemedia/htmlwidget-hexjson")
library(hexjsonwidget)

IHT_bucket <- "s3://thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp"
ASC_subfolder <- "ASC and Finance Report"

########################################
########## CREATE LA MAP ###############
########################################

# Load in map of local authorities 

map <- s3read_using(read_sf,
                    object = '/Tom/GP-contract-unpaid-carers/Data/Local_Authority_Districts_December_2022_UK_BUC_V2_-5963189729337928393.geojson',
                    bucket = bucket) %>%
  rename(LA_CODE = LAD22CD) %>%
  filter(str_detect(LA_CODE, "^E"))

# Join map to gp/census joined data
map_data_join <- full_join(map, gp_census_join, by="LA_CODE")

hexed_map <- calculate_grid(map_data_join, grid_type = 'hexagonal')

res_map <- assign_polygons(map_data_join, hexed_map)

# Make national map (coverage)
ggplot() +
  geom_sf(data = res_map, aes(fill = Coverage)) +
  theme_void() +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43")

# Filter to create London map

london_join <- map_data_join %>%
  filter(str_detect(LA_CODE, "^E09"))

# Make London graph (coverage)
ggplot() +
  geom_sf(data = london_join, aes(fill = Coverage)) +
  theme_void() +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43")

#############################
######### HEX MAP ###########
#############################

utla_hex_template <- s3read_using(read_excel,
                                  object = paste0(ASC_subfolder,"/hexmap-lad-template.xlsx"), # File to open
                                  bucket = IHT_bucket,
                                  sheet=1)

hours_hex_map_data <- utla_hex_template %>%
  left_join(.,hours_hex_map_data,by=c("lacode"="area_code"))
rm(utla_hex_template)

hex_LA <- s3read_using(fromJSON,
                    object = '/Tom/GP-contract-unpaid-carers/Data/uk-local-authority-districts-2023.hexjson',
                    bucket = bucket)


hex_data_join <- full_join(IMD_rural_join, hex_LA, by=c('LA_CODE'='id'))


### HTML widget version


download.file('https://github.com/odileeds/hexmaps/blob/6efe7925e104f8e082d848e6648d4a93bea99c39/maps/uk-local-authority-districts-2023.hexjson', destfile = 'Raw_data/LA_2023.hexjson')

test_hex <- jsonlite::fromJSON('Raw_data/LA_2023.hexjson')

hex_LA_file <- 'Raw_data/LA_2023.hexjson'

hexjsonwidget(jsonbase =  test_hex)

