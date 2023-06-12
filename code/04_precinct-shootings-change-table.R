# run 00_read_data.Rmd
# read in cleaned shootings data -----------
cure_data <- read.csv("data/output/shootings-change-from-start-year_by-precinct.csv") %>%
  replace(is.na(.), "")  %>% # remove NAs
  mutate(across(2:9, as.numeric)) # make all numeric

# clean column names
names(cure_data)[2:11] <- seq(2011, 2020, 1)

# check values' distribution for color binning-classification
# c <- unlist(list(cure_data$`2011`,cure_data$`2012`, cure_data$`2013`, cure_data$`2014`, cure_data$`2015`, cure_data$`2016`, cure_data$`2017`, cure_data$`2018`, cure_data$`2019`, cure_data$`2020`))
# hist(c, breaks = 30)
# hist(c, breaks = 7)
###### gt table ---------

gt_table <- cure_data %>%
  gt(rowname_col = "precinct", groupname_col = "groupname") %>%
  # gtExtras::gt_plt_sparkline(sparkline) %>%
  tab_header(title = "% Change in Shootings from Year Before Precincts entered Cure Violence Program",
        subtitle = "In Order by Year of Entry") %>%
  gt_theme_nytimes() %>%
  sub_missing() %>%
  data_color(columns = starts_with("20"),
             rows = everything(),
             method = "numeric",
             colors =
               scales::col_bin(
                 bins = c(-100,-50,-10,-1,0,1,20,100,150,350),
                 na.color = "transparent",
                 palette = nycc_pal("diverging", reverse = T)(7)),
             contrast_algo = "wcag") %>%
  fmt_percent(columns = starts_with("20"),
              decimals = 0, scale_values = F) %>%
  tab_style(style = cell_borders(color = "#23417D"),
            locations = cells_body(rows = c(1:2),
                         columns = as.character(seq(2012, 2019, 1)))) %>%
  tab_style(style = cell_borders(color = "#23417D"),
            locations = cells_body(rows = c(3:4),
                         columns = as.character(seq(2013, 2019, 1)))) %>%
  tab_style(style = cell_borders(color = "#23417D"),
            locations = cells_body(rows = c(5),
                         columns = as.character(seq(2014, 2019, 1)))) %>%
  tab_style(style = cell_borders(color = "#23417D"),
            locations = cells_body(rows = c(6:12),
                        columns = as.character(seq(2015, 2019, 1)))) %>%
  tab_style(style = cell_borders(color = "#23417D"),
            locations = cells_body(rows = c(13:18),
                       columns = as.character(seq(2016, 2019, 1))))  %>%
  # tab_row_group(label = "2016 Entry", rows = 13:18) %>%
  #   tab_row_group(label = "2015 Entry", rows = 6:12) %>%
  #   tab_row_group(label = "2014 Entry", rows = 5) %>%
  #   tab_row_group(label = "2013 Entry", rows = 3:4) %>%
  #   tab_row_group(label = "2012 Entry", rows = 1:2) %>%
  # tab_style(style = cell_text(align = "right"),
  #           locations = cells_row_groups())

gtsave(gt_table, "visuals/precinct-shootings-change-table.html")


write.csv(cure_data_lags, "data/output/cure_before-in-after.csv", row.names = F)

# tab_row_group(label = "2016 Entry", rows = 13:18) %>%
#   tab_row_group(label = "2015 Entry", rows = 6:12) %>%
#   tab_row_group(label = "2014 Entry", rows = 5) %>%
#   tab_row_group(label = "2013 Entry", rows = 3:4) %>%
#   tab_row_group(label = "2012 Entry", rows = 1:2) %>%
########

cohort_shp <- programs_20 %>%
  select(1:6, 9:11)) %>%
  mutate(year = paste0(rep(20,nrow(.)),year),
         year = case_when(year=="20NA" ~ NA,
                          TRUE ~ year)) %>%
  left_join(cure_data, by= c('precinct', 'year')) %>%

