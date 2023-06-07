# uploading data
shootings_06_12 <- read.csv('data/output/NYC-precinct-shootings_mean_2006_2012.csv')
precinct.shp <- st_read("data/input/Police\ Precincts/geo_export_ece0487d-79ca-4c55-898e-8333b6d2b0bc.shp")
precinct.shp <- st_transform(precinct.shp,'+proj=longlat +datum=WGS84')


# just precincts in cure 
program_dates <- read_excel("data/output/cure_programs.xlsx") 
precincts_in_cure <- unique(program_dates$Precinct) 

# joining shootings per person data with precinct shape file
shootings_06_12.shp <- shootings_06_12 %>% 
  left_join(precinct.shp, by=c("precinct"))
shootings_06_12.shp <- shootings_06_12.shp %>% 
  st_as_sf() %>% st_transform('+proj=longlat +datum=WGS84')

# converting NaN to 0
shootings_06_12.shp[is.na(shootings_06_12.shp)] <- 0

# check column distribution
plot(density(shootings_06_12.shp$shootings_per_100K, na.rm = T))
hist(shootings_06_12.shp$shootings_per_100K, breaks = 20)

# will use jenks for bins
int_prct <- classIntervals(shootings_06_12.shp$shootings_per_100K, 
                           n = 5, style = 'jenks')

blue_pal <- c('#e7eefb', '#cdd4f5',  '#98a3ea', '#5675dd', '#1d5fd6')

# creating color palete
palPct = colorBin(
  palette = blue_pal, #nycc_pal("cool")(5),
  bins = int_prct$brks,
  domain = shootings_06_12.shp$shootings_per_100K, 
  na.color = "#E6E6E6", 
  reverse = FALSE
)

# labels when clicking on the map
map_labels <- paste0("<b>Precinct: </b>",
                     shootings_06_12.shp$precinct,
                     "<br>","<b>Shootings Per 100K: </b>",
                     round(shootings_06_12.shp$shootings_per_100K, 2))

# finding the centroids of all of the precincts for labeling
shootings_06_12.shp <- shootings_06_12.shp %>%
  mutate(lab_lat = st_coordinates(st_centroid(geometry))[,1],
         lab_lon = st_coordinates(st_centroid(geometry))[,2])


# creating the choropleth  
m <- leaflet(options = leafletOptions(minZoom = 11, maxZoom = 13,
                                      zoomControl = FALSE,
                                      dragging = T)) %>% 
  htmlwidgets::onRender("function(el, x) { 
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>% # moving zoom control to the right
  addPolygons(data = shootings_06_12.shp, # adding shootings by precinct
              weight = ~ ifelse(shootings_06_12.shp$precinct %in% precincts_in_cure, 1.5,1),
              fillColor = ~palPct(shootings_per_100K),
              #color="#cdd9f1",
              color= ~ ifelse(shootings_06_12.shp$precinct %in% precincts_in_cure, "red","
                              white"),
              stroke = TRUE,
              fillOpacity = 1,
              popup = ~map_labels) %>%
  addLegend_decreasing(position ="topleft", # adding a legend
                       pal= palPct,
                       values = shootings_06_12.shp$shootings_per_100K,
                       title = "Shootings Per 100K",
                       labFormat = labelFormat(digits = 1),
                       opacity = 1,
                       decreasing=TRUE) %>%
  addLabelOnlyMarkers(lat = shootings_06_12.shp$lab_lon, # adding precinct labels
                      lng = shootings_06_12.shp$lab_lat,
                      label = shootings_06_12.shp$precinct,
                      labelOptions = labelOptions(permanent = TRUE,
                                                  noHide = TRUE,
                                                  textOnly = TRUE,
                                                  textsize = 4,
                                                  direction = "center",
                                                  style = list(color = "#800000",
                                                              "font-family" = 'sans serif',
                                                              "font-weight" = "bold"))) %>%
  leaflet.extras::setMapWidgetStyle(list(background = "white"))# white background

#saveWidget(m, file = "visuals/avg_shootings_map.html")
