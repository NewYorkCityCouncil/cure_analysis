# run 00_read_data.Rmd

# making map on categories of cohorts
# too many cohorts when grouping by month & year
length(unique(program_dates$date_text))
unique(programs_20$year)

# join to precinct

cohort_shp <- left_join(precinct.shp,
                        programs_20 %>%
                          select(1:6, 9:11))

# Create color palete
pal = colorFactor(
  palette = nycc_pal("main")(6),
  domain = cohort_shp$year,
  na.color = "#F9F9F9",
  reverse = TRUE
)

# map
m <- leaflet(options = leafletOptions(minZoom = 10, maxZoom = 15,
                                      zoomControl = FALSE,
                                      dragging = F)) %>%
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
 # addCouncilStyle() %>% not working
  setMapWidgetStyle(list(background= "white")) %>%
  addPolygons(data = cohort_shp,
              weight = 1,
              fillColor = ~pal(year),
              fillOpacity = 1,
              stroke = T,
              color = "#CACACA") %>%
  addLegendFactor(pal= pal,
                  shape = "rect",
                  orientation = "horizontal",
                  values = cohort_shp$year,
                  title = "Precincts Entering Cure by Cohort Year",
                  opacity = 1)

saveWidget(m, file = "visuals/cohort_map.html")
