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
no_pop_shp <- cohort_shp %>% filter(is.na(year))

# Create color palete
pal = colorFactor(
  palette = nycc_pal("main", reverse = T)(5),
  domain = label_shp$year,
  na.color = "#F9F9F9"
)

# Labels when clicking on the map
map_labels <- paste0("<h3>Precinct: ", label_shp$precinct,
                    "  |  <em>", label_shp$program_name, "</em></h3>",
                    "<br>","<b>Council District(s): </b>",
                    label_shp$council_district,
                    "<br>","<b>Neighborhood Area Serviced: </b>",
                    label_shp$neighborhood_area_serviced,
                    "<br>","<b>Rounds in Program: </b>",
                   label_shp$waves)
# Source legend
source_legend <- HTML('<small> Source: NYC Open Data, Fill in sources </small>')

# map
m <- leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15,
                                      zoomControl = FALSE,
                                      dragging = F)) %>%
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
 # addCouncilStyle() %>% not working
  setMapWidgetStyle(list(background= "white")) %>%
  addPolygons(data = no_pop_shp,
              weight = 1,
              fillColor = ~pal(year),
              fillOpacity = 1,
              stroke = T,
              color = "#CACACA") %>%
  addPolygons(data = label_shp,
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
                                                  textsize = 12,
                                                  direction = "center",
                                 style = list(color = "#FFFFFF"))) %>%
  addLegendFactor(data = label_shp,
                  pal= pal,
                  shape = "rect",
                  orientation = "horizontal",
                  values = label_shp$year,
                  title = "Precincts Entering Cure by Cohort Year",
                  opacity = 1) %>%
  addSourceText(source_legend)

saveWidget(m, file = "visuals/cohort_map.html")
