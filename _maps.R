library(sf)
library(plotly)
library(geogrid)
library(devtools)
install_github("psychemedia/htmlwidget-hexjson")
library(hexjsonwidget)
library(jsonlite)

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

res_map$tooltips <- paste0(res_map$LAD22NM, '\nCoverage: ', round(res_map$Coverage, 2))

testmap <- ggplot() +
  geom_sf(data = res_map, aes(fill = Coverage, label = tooltips), color = 'black') +
  theme_void() +
  scale_fill_viridis_b()

testmap

ggplotly(testmap, tooltip = 'label')

# Make national map (coverage by ICB)

res_map$tooltips_ICB <- paste0(res_map$LAD22NM, '\nCoverage: ', round(res_map$Coverage, 2))

testmap <- ggplot() +
  geom_sf(data = res_map, aes(fill = Coverage, label = tooltips), color = 'black') +
  theme_void() +
  scale_fill_viridis_b()

ggplotly(testmap, tooltip = 'label')



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


test <- s3read_using(fromJSON,
                       object = '/Sebastien/GitHub/hexmaps/maps/uk-local-authority-districts-2020.hexjson',
                       bucket = bucket)


hex_data_join <- full_join(IMD_rural_join, hex_LA, by=c('LA_CODE'='id'))


### HTML widget version


download.file('https://github.com/odileeds/hexmaps/blob/6efe7925e104f8e082d848e6648d4a93bea99c39/maps/uk-local-authority-districts-2023.hexjson', destfile = 'Raw_data/LA_2023.hexjson')

test_hex <- jsonlite::fromJSON('Raw_data/LA_2023.hexjson')

hex_LA_file <- 'Raw_data/LA_2023.hexjson'

hexjsonwidget(hex_LA, label = NA)

