# run 00_read_data.Rmd

# making map on categories of cohorts
# too many cohorts when grouping by month & year
length(unique(program_dates$date_text))
unique(programs_20$year)

# join to precinct

cohort_shp <- left_join(precinct.shp,
                        programs_20 %>%
                          select(1:6, 9:11)) %>%
  mutate(year = paste0(rep(20,nrow(.)),year),
         year = case_when(year=="20NA" ~ NA,
                          TRUE ~ year),
         lab_lat = st_coordinates(st_centroid(geometry))[,1],
         lab_lon = st_coordinates(st_centroid(geometry))[,2]
         ) %>%  #fixing year & labels for map
  filter(!grepl("2019", NA)) # remove 2019 precincts but keep NA

label_shp <- cohort_shp %>% filter(!is.na(year))
# no_pop_shp <- cohort_shp %>% filter(is.na(year)) - no longer needed precinct.shp used to have no missing precincts when subsetting to cohort year in map

# Create color palete
pal = colorFactor(
  palette = c("#666666", "#AF6D46", "#B3B3FF","#1F3A70", "#BA9F64", "#1850B5","#660000"), # nycc_pal interpolates...
  domain = label_shp$year,
  na.color = "#F9F9F9",
  reverse = T
)

# Labels when clicking on the map
map_labels <- paste0("<h3>Precinct: ", label_shp$precinct,
                    "  |  <em>", label_shp$program_name, "</em></h3>",
                    "<br>","<b>Council District(s): </b>",
                    label_shp$council_district,
                    "<br>","<b>Neighborhood Area Serviced: </b>",
                    label_shp$neighborhood_area_serviced,
                    "<br>","<b>Rounds in Program: </b>",
                   label_shp$waves,
                   "<br>","<b>Year Entering: </b>",
                   label_shp$year)
# Source legend
source_legend <- HTML('<small> Source: NYC Open Data, Fill in sources </small>')

# map
m <- leaflet(options = leafletOptions(minZoom = 11, maxZoom = 13,
                                      zoomControl = FALSE,
                                      dragging = T)) %>%
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
 # addCouncilStyle() %>% not working
  setMapWidgetStyle(list(background= "white")) %>%
  addPolygons(data = precinct.shp,
              weight = 1,
              fillColor = "#F9F9F9",
              fillOpacity = 1,
              stroke = T,
              color = "#CACACA") %>%
  addPolygons(data = label_shp, group = "All Cure Precincts",
              weight = 1,
              fillColor = ~pal(year),
              fillOpacity = 1,
              stroke = T,
              color = "#CACACA",
              popup = ~lapply(map_labels, HTML)) %>%
  addPolygons(data = label_shp[label_shp$year=="2012",], group = "Cohort 2012",
              weight = 1,
              fillColor = ~pal(year),
              fillOpacity = 1,
              stroke = T,
              color = "#CACACA",
              popup = ~lapply(map_labels, HTML)) %>%
  addPolygons(data = label_shp[label_shp$year=="2013",], group = "Cohort 2013",
              weight = 1,
              fillColor = ~pal(year),
              fillOpacity = 1,
              stroke = T,
              color = "#CACACA",
              popup = ~lapply(map_labels, HTML)) %>%
  addPolygons(data = label_shp[label_shp$year=="2014",], group = "Cohort 2014",
              weight = 1,
              fillColor = ~pal(year),
              fillOpacity = 1,
              stroke = T,
              color = "#CACACA",
              popup = ~lapply(map_labels, HTML)) %>%
  addPolygons(data = label_shp[label_shp$year=="2015",], group = "Cohort 2015",
              weight = 1,
              fillColor = ~pal(year),
              fillOpacity = 1,
              stroke = T,
              color = "#CACACA",
              popup = ~lapply(map_labels, HTML)) %>%
  addPolygons(data = label_shp[label_shp$year=="2016",], group = "Cohort 2016",
              weight = 1,
              fillColor = ~pal(year),
              fillOpacity = 1,
              stroke = T,
              color = "#CACACA",
              popup = ~lapply(map_labels, HTML)) %>%
  addPolygons(data = label_shp[label_shp$year=="2019",], group = "Cohort 2019",
              weight = 1,
              fillColor = ~pal(year),
              fillOpacity = 1,
              stroke = T,
              color = "#CACACA",
              popup = ~lapply(map_labels, HTML)) %>%
  addLabelOnlyMarkers(lat = label_shp$lab_lon,
                      lng = label_shp$lab_lat,
                      label = label_shp$precinct,
                      labelOptions = labelOptions(permanent = TRUE,
                                                  noHide = TRUE,
                                                  textOnly = TRUE,
                                                  textsize = 10,
                                                  direction = "center",
                                 style = list(color = "#FFFFFF"))) %>%
  addLegendFactor(data = label_shp, position = "topleft",
                  pal= pal,
                  shape = "rect",
                  orientation = "horizontal",
                  values = label_shp$year,
                  title = "Precincts Entering Cure by Cohort Year",
                  opacity = 1) %>%
  addLayersControl(options = layersControlOptions(collapsed = T),
                   position = "topleft",
                   baseGroups = c("All Cure Precincts",
                                  "Cohort 2012", "Cohort 2013","Cohort 2014",
                                  "Cohort 2015", "Cohort 2016", "Cohort 2019")) %>%
  addSourceText(source_legend)

saveWidget(m, file = "visuals/cohort_map.html")
