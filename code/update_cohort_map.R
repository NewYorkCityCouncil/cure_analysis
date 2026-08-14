# ==========================================================================
# STEP 2: Data cleaning + layer logic (Rules 1-3)
# Builds on the existing 00_read_data.Rmd pipeline. Assumes precinct.shp
# is already loaded, same as the original script.
# ==========================================================================

# ---- Read updated program data --------------------------------------------
program_updates <- read.csv("/Users/GCampa/Downloads/clean_update26.csv", stringsAsFactors = FALSE)

# Rows that can't be mapped to a precinct: blank, "citywide", or any other
# non-numeric value (e.g. borough names like "Queens", "Queens, Brooklyn").
# Set aside for now, handle separately later.
is_unmapped <- suppressWarnings(is.na(as.numeric(program_updates$precinct)))
 
unmapped_programs <- program_updates %>%
  filter(is_unmapped)
 
program_updates <- program_updates %>%
  filter(!is_unmapped)
 
# Rows with no new_num value at all are fully excluded (Rule 2)
program_updates <- program_updates %>%
  filter(new_num != "")
 
# Clean up types
program_updates <- program_updates %>%
  mutate(precinct   = as.numeric(precinct),
         new_num    = as.numeric(new_num),
         type_num   = as.numeric(type_num),
         start_year = as.numeric(start_year),
         end_year   = as.numeric(end_year))
 
# ==========================================================================
# RULE 1: New precincts layer (new_num == 2)
# ==========================================================================
 
new_precincts_data <- program_updates %>%
  filter(new_num == 2) %>%
  arrange(precinct, start_year) %>%
  distinct(precinct, .keep_all = TRUE)  # keeps earliest start_year per precinct
 
new_precincts_shp <- left_join(precinct.shp %>% select(precinct, geometry),
                                new_precincts_data, by = "precinct") %>%
  filter(!is.na(new_num)) %>%
  mutate(year = as.character(start_year))
 
# ==========================================================================
# RULE 2: Updated "All Cure Precincts" layer
#   - includes new_num 1 and 2
#   - colored by the earliest start_year per precinct
#   - new_num == 0 rows are excluded from this layer entirely
#     (they only appear in the relevant yearly layers, see Rule 3 below)
# ==========================================================================
 
all_cure_data <- program_updates %>%
  filter(new_num %in% c(1, 2)) %>%
  arrange(precinct, start_year) %>%
  distinct(precinct, .keep_all = TRUE)  # keeps earliest start_year per precinct
 
label_shp_unique <- left_join(precinct.shp %>% select(precinct, geometry),
                               all_cure_data, by = "precinct") %>%
  filter(!is.na(new_num)) %>%
  mutate(year = as.character(start_year),
         lab_lat = st_coordinates(st_point_on_surface(geometry))[, 1],
         lab_lon = st_coordinates(st_point_on_surface(geometry))[, 2])
 
# ==========================================================================
# RULE 3: Yearly layers
#   - existing years from the original map: 2012, 2013, 2014, 2015, 2016, 2019, 2021
#   - new years to add: 2017, 2020, 2022, 2023, 2024, 2025
#   - only include precincts where type_num is 1, 2, or 3
#   - a new_num == 0 row is present starting at start_year and drops out
#     starting at end_year; new_num 1/2 rows have no end_year so they
#     stay present in every year at/after their start_year
# ==========================================================================
 
all_years <- c(2012, 2013, 2014, 2015, 2016, 2017, 2019,
                2020, 2021, 2022, 2023, 2024, 2025)
 
# Helper to build the precinct-level sf data for a given year.
# Kept as a function (rather than 13 copy-pasted blocks like the original's
# 7) purely to avoid unmanageable duplication now that there are 13 years -
# happy to expand it back out into explicit blocks per year if you'd rather
# match the original's literal structure.
build_year_layer <- function(yr) {
  program_updates %>%
    filter(type_num %in% c(1, 2, 3),
           new_num %in% c(0, 1, 2),
           start_year <= yr,
           is.na(end_year) | end_year > yr) %>%
    distinct(precinct, .keep_all = TRUE) %>%
    left_join(precinct.shp %>% select(precinct, geometry), ., by = "precinct") %>%
    filter(!is.na(new_num)) %>%
    mutate(year = as.character(yr))
}
 
year_layers <- setNames(lapply(all_years, build_year_layer),
                         paste0("label_shp_", all_years))
 
# access individual years the same way as the original, e.g.:
# label_shp_2012 <- year_layers[["label_shp_2012"]]
# label_shp_2022 <- year_layers[["label_shp_2022"]]
list2env(year_layers, envir = .GlobalEnv)

# in addPolygons(), use: popup = ~lapply(popup_html, HTML)

# ==========================================================================
# STEP 3: Popup content (Rule 4)
# Builds one popup per precinct (reused across every layer it appears in).
# Run this after 01_layers_update.R.
# ==========================================================================

# simple monochrome line-icons (inline SVG, no extra package needed)
type_symbol <- c(
  "1" = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="12" height="12" style="vertical-align:middle;"><circle cx="12" cy="3" r="2" fill="#000000"/><path d="M12 5 L12 13 M12 8 L8 6 M12 8 L16 10 M12 13 L9 20 M12 13 L15 19" stroke="#000000" stroke-width="2" fill="none" stroke-linecap="round" stroke-linejoin="round"/></svg>', # walking man -> Neighborhood-based / walking programs
  "2" = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="12" height="12" style="vertical-align:middle;"><rect x="9" y="2" width="6" height="20" rx="1" fill="#4d4d4d"/><rect x="2" y="9" width="20" height="6" rx="1" fill="#4d4d4d"/></svg>', # medical cross -> Hospital-based programs
  "3" = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="12" height="12" style="vertical-align:middle;"><path d="M2 5 C5 3, 9 3, 12 5 C15 3, 19 3, 22 5 L22 18 C19 16, 15 16, 12 18 C9 16, 5 16, 2 18 Z" fill="none" stroke="#000000" stroke-width="1.5" stroke-linejoin="round"/><line x1="12" y1="5" x2="12" y2="18" stroke="#000000" stroke-width="1.5"/></svg>' # open book -> School-linked programs
)

type_label <- c("1" = "Neighborhood-based Violence Intervention",
                "2" = "Hospital-based Violence Intervention",
                "3" = "School-linked Violence Intervention")

# Popup builder takes an already-filtered set of "active" rows for a given
# context (see below) rather than always using the same global set - this
# is what lets a new_num == 0 program (e.g. GMACC) show up in the popup for
# the years it was actually active, and disappear from the popup starting
# at its end_year, matching how it disappears from the yearly polygon
# layers themselves.
build_precinct_popup <- function(precinct_id, rows) {

  precinct_rows <- rows %>% filter(precinct == precinct_id)

  # a) heading
  html <- paste0('<h4 style="margin:0 0 4px 0;">Precinct ', precinct_id,
                 '</h4><hr style="margin:4px 0;">')

  # b) + c) organizations with type_num 1/2/3 programs
  core_rows <- precinct_rows %>% filter(type_num %in% c(1, 2, 3))

  if (nrow(core_rows) > 0) {
    orgs <- unique(core_rows$organization)

    for (org in orgs) {
      org_rows <- core_rows %>% filter(organization == org)

      establish_vals <- unique(org_rows$program_establish[org_rows$program_establish != ""])
      establish <- if (length(establish_vals) > 0) establish_vals[1] else NA

      org_line <- if (!is.na(establish)) {
        paste0("<b>", org, "- CVI established ", establish, "</b><br>")
      } else {
        paste0("<b>", org, "</b><br>")
      }
      html <- paste0(html, org_line)

      for (i in seq_len(nrow(org_rows))) {
        r <- org_rows[i, ]
        symbol <- type_symbol[as.character(r$type_num)]
        program_line <- paste0(
          "&nbsp;&nbsp;&nbsp;", symbol, " ", r$program_name, ": Added to CMS in ",
          r$start_year, " serving ", r$neighborhood,
          ". Relevant council district(s): ", r$council_dist, "<br>"
        )
        html <- paste0(html, program_line)
      }
    }
  }

  # d) "Other CMS Services" (type_num == 4)
  other_rows <- precinct_rows %>% filter(type_num == 4)

  if (nrow(other_rows) > 0) {
    other_entries <- paste0(other_rows$organization, " - ", other_rows$program_type,
                            " (", other_rows$start_year, ")")
    other_line <- paste0('<br><b>Other CMS Services: </b>',
                          paste(other_entries, collapse = "; "))
    html <- paste0(html, other_line)
  }

  return(html)
}

# helper: build a named lookup of precinct -> popup html from a given row set
build_popup_lookup <- function(rows) {
  ids <- sort(unique(rows$precinct))
  setNames(sapply(ids, build_precinct_popup, rows = rows), ids)
}

# ---- Popups for "All Cure Precincts" and "New Precincts" -------------------
# No year window - only currently active programs (new_num 1 or 2);
# new_num == 0 (ended) programs never appear here, same as before.
current_popup_data <- program_updates %>% filter(new_num %in% c(1, 2))
current_popups <- build_popup_lookup(current_popup_data)

label_shp_unique$popup_html  <- current_popups[as.character(label_shp_unique$precinct)]
new_precincts_shp$popup_html <- current_popups[as.character(new_precincts_shp$precinct)]

# ---- Popups for each yearly layer ------------------------------------------
# Uses the same start_year/end_year window as the yearly polygon layers
# themselves (build_year_layer in 01_layers_update.R), so a new_num == 0
# program like GMACC shows up in the popup for every year it was active
# and drops out of the popup starting at its end_year.
build_year_popup_data <- function(yr) {
  program_updates %>%
    filter(new_num %in% c(0, 1, 2),
           start_year <= yr,
           is.na(end_year) | end_year > yr)
}

attach_year_popup <- function(shp, yr) {
  popups <- build_popup_lookup(build_year_popup_data(yr))
  shp$popup_html <- popups[as.character(shp$precinct)]
  shp
}

year_layers <- Map(attach_year_popup, year_layers, all_years)
list2env(year_layers, envir = .GlobalEnv)

# in addPolygons(), use: popup = ~lapply(popup_html, HTML)

# ==========================================================================
# STEP 6: Citywide layer for unmapped programs
# Run this after 01_layers_update.R and 02_popups.R (uses unmapped_programs
# and type_symbol-free popup styling). Assumes `boro` is already loaded:
#
# boro <- read_sf("https://data.cityofnewyork.us/api/geospatial/gthc-hcne?method=export&format=GeoJSON") %>%
#   st_transform("+proj=longlat +datum=WGS84") %>%
#   st_simplify(dTolerance = .00001)
# ==========================================================================

# ---- Dissolve the 5 boroughs into a single citywide polygon ---------------
citywide_shp <- boro %>%
  summarise(geometry = st_union(geometry))

# ---- Build a clean "areas served" label for each unmapped program ---------
# Turns the raw precinct/neighborhood text into a clear citywide-or-borough
# label: "citywide" -> "Citywide"; values that are already borough names
# (e.g. "Queens", "Queens, Brooklyn") are used as-is; the one row with a
# blank precinct (Not Another Child, Inc. - East NY) is mapped to its
# borough by hand since it's a one-off. Extend this lookup if more
# neighborhood-only rows show up in future data updates.
neighborhood_to_borough <- c("East NY" = "Brooklyn")

unmapped_display <- unmapped_programs %>%
  mutate(
    raw_area = trimws(precinct),
    area_served = case_when(
      tolower(raw_area) == "citywide" ~ "Citywide",
      raw_area != "" ~ raw_area,
      neighborhood %in% names(neighborhood_to_borough) ~ neighborhood_to_borough[neighborhood],
      TRUE ~ neighborhood
    )
  )

# ---- Build the single citywide popup (same style, no icons) ---------------
# Each program gets its own indented bulleted line, same indentation
# convention used for the program lines in the main precinct popups.
citywide_entries <- paste0(
  "&nbsp;&nbsp;&nbsp;\u2022 ", unmapped_display$organization, " - ",
  unmapped_display$program_type, " (", unmapped_display$start_year, "): ",
  unmapped_display$area_served, "<br>"
)

citywide_popup <- paste0(
  '<h4 style="margin:0 0 4px 0;">Citywide &amp; Multi-Area Programs</h4><hr style="margin:4px 0;">',
  '<b>Programs Not Tied to a Single Precinct:</b><br>',
  paste(citywide_entries, collapse = "")
)

citywide_shp$popup_html <- citywide_popup

# in addPolygons(): light grey, transparent fill with a visible mid-grey
# outline so the layer reads as present even without hovering. Shares the
# "All Cure Precincts" group so it shows/hides together with that layer,
# and must be added to the map BEFORE label_shp_unique so it renders
# beneath it:
#
#   addPolygons(data = citywide_shp, group = "All Cure Precincts",
#               weight = 2.5, stroke = TRUE, color = "#999999",
#               fillColor = "#D9D9D9", fillOpacity = 0.25,
#               popup = ~lapply(popup_html, HTML)) %>%
#   addPolygons(data = label_shp_unique, group = "All Cure Precincts", ...)
# ==========================================================================
# STEP 4 + 5: Final map assembly (all layers + popups) and the legend
# Run this after 01_layers_update.R and 02_popups.R
# ==========================================================================

# ---- Extended color palette --------------------------------------------
# Original 8 colors, kept exactly as-is, plus 5 new colors added in the
# same muted/desaturated style to cover the additional cohort years
# (2017, 2020, 2022, 2023, 2024, 2025) now present in the data.
pal_colors <- c("#666666", "#AF6D46", "#B3B3FF", "#1F3A70", "#BA9F64",
                 "#1850B5", "#660000", "darkgreen",
                 "#9B4F6E", "#4C8577", "#8C8C3D", "#5C5470", "#B5651D")

pal_domain <- as.character(sort(unique(c(all_cure_data$start_year, all_years))))

pal <- colorFactor(
  palette = pal_colors,
  domain = pal_domain,
  na.color = "#F9F9F9",
  reverse = TRUE
)

# ---- Legend (Rule 5) ------------------------------------------------------
# Reuses the same three icons used in the popups
legend_html <- paste0(
  '<div style="background:white; padding:8px 10px; border-radius:6px; ',
  'box-shadow:0 1px 5px rgba(0,0,0,0.4); font-size:12px; line-height:1.6;">',
  '<b>Crisis Management System Program Type</b><br>',
  type_symbol["1"], ' Neighborhood-based Violence Intervention Program (Cure Violence)<br>',
  type_symbol["2"], ' Hospital-based Violence Intervention Program (HVIP)<br>',
  type_symbol["3"], ' School-based Violence Intervention Program',
  '</div>'
)

# ---- Map --------------------------------------------------------------
m <- leaflet(options = leafletOptions(minZoom = 11, maxZoom = 13,
                                      zoomControl = FALSE,
                                      dragging = T)) %>%
  htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
  setMapWidgetStyle(list(background = "white")) %>%
  addPolygons(data = precinct.shp,
              weight = 1,
              fillColor = "#F9F9F9",
              fillOpacity = 1,
              stroke = T,
              color = "#CACACA") %>%
  addPolygons(data = citywide_shp, group = "All Cure Precincts",
              weight = 2.5,
              stroke = TRUE,
              color = "#999999",
              fillColor = "#D9D9D9",
              fillOpacity = 0.25,
              popup = ~lapply(popup_html, HTML)) %>%
  addPolygons(data = label_shp_unique, group = "All Cure Precincts",
              weight = 1,
              fillColor = ~pal(year),
              fillOpacity = 1,
              stroke = T,
              color = "#CACACA",
              popup = ~lapply(popup_html, HTML)) %>%
  addPolygons(data = new_precincts_shp, group = "New Precincts",
              weight = 1,
              fillColor = ~pal(year),
              fillOpacity = 1,
              stroke = T,
              color = "#CACACA",
              popup = ~lapply(popup_html, HTML))

# yearly layers (2012, 2013, 2014, 2015, 2016, 2017, 2019, 2020, 2021,
# 2022, 2023, 2024, 2025) - looped rather than 13 copy-pasted addPolygons
# blocks, but each call is identical in style/args to the original's
# per-year addPolygons blocks
for (yr in all_years) {
  yr_data <- year_layers[[paste0("label_shp_", yr)]]
  m <- m %>%
    addPolygons(data = yr_data,
                group = as.character(yr),
                weight = 1,
                fillColor = ~pal(year),
                fillOpacity = 1,
                stroke = T,
                color = "#CACACA",
                popup = ~lapply(popup_html, HTML))
}

m <- m %>%
  addLabelOnlyMarkers(lat = label_shp_unique$lab_lon,
                      lng = label_shp_unique$lab_lat,
                      label = label_shp_unique$precinct,
                      labelOptions = labelOptions(permanent = TRUE,
                                                  noHide = TRUE,
                                                  textOnly = TRUE,
                                                  textsize = 10,
                                                  direction = "center",
                                 style = list(color = "#FFFFFF"))) %>%
  addControl(html = legend_html, position = "bottomright") %>%
  addLayersControl(options = layersControlOptions(collapsed = T),
                   position = "topleft",
                   baseGroups = c("All Cure Precincts", "New Precincts",
                                  as.character(all_years)))

m

saveWidget(m, file = "visuals/cohort_map.html")