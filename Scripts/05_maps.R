
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
map_data_full_join <- full_join(map, all_variables_joined, by="LA_CODE")



###########################################
########### REGULAR MAP: LEAFLET ##########
###########################################

map_transformed <- st_transform(map_data_full_join, "+init=epsg:4326")

colours_viridis <- colorNumeric(palette = 'viridis', domain = NULL)

leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(
    data = map_transformed,
    label = ~LAD22NM,
    fillColor = colours_viridis(map_transformed$Coverage),
    fillOpacity = 0.8,
    weight = 2,
    color = 'black',
    highlightOptions = highlightOptions(color = 'red')
  ) %>% 
  addLegend(
    pal = colours_viridis,
    values = map_transformed$Coverage,
    title = 'Coverage'
  )



#######################################
######### HEX MAP: LEAFLET ############
#######################################

hexed_map <- calculate_grid(map_data_full_join, grid_type = 'hexagonal')

res_map <- assign_polygons(map_data_full_join, hexed_map)

saveRDS(res_map, file = 'Resources/res_map.rds')

res_map <- readRDS('Resources/res_map.rds')

res_map_transformed <- st_transform(res_map, "+init=epsg:4326")

leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(
    data = res_map_test,
    label = res_map_test$LAD22NM,
    fillColor = colours_viridis(res_map_test$Coverage),
    fillOpacity = 0.8,
    weight = 2,
    color = 'black',
    highlightOptions = highlightOptions(color = 'red')
  ) %>% 
  addLegend(
    pal = colours_viridis,
    values = res_map_test$Coverage,
    title = 'Coverage'
  )



#############################################
########## REGULAR MAP: GGPLOTLY ############
#############################################

map_data_full_join$tooltips <- paste0(map_data_full_join$LAD22NM, '\nCoverage: ', round(map_data_full_join$Coverage, 2))

ggplotly(ggplot() +
           geom_sf(data = map_data_full_join, aes(fill = Coverage, label = tooltips), color = 'black') +
           theme_void() +
           scale_fill_viridis_b(),
         tooltip = 'label')

#########################################
########## HEX MAP: GGPLOTLY ############
#########################################

# Make national map (coverage)

res_map$tooltips <- paste0(res_map$LAD22NM, '\nCoverage: ', round(res_map$Coverage, 2))

#ggplotly(
  ggplot() +
           geom_sf(data = res_map, aes(fill = Coverage, label = tooltips), color = 'black') +
           theme_void() +
           scale_fill_viridis_b()
 # , tooltip = 'label')

s3write_using(map_transformed,
              write_sf,
              object = '/Tom/GP-contract-unpaid-carers/Outputs/map_transformed.geojson',
              bucket = bucket)

s3write_using(res_map_transformed,
              write_sf,
              object = '/Tom/GP-contract-unpaid-carers/Outputs/hex_map_transformed.geojson',
              bucket = bucket)