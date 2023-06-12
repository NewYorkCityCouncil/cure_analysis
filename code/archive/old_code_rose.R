# run 00_read_data.Rmd
# read in shootings data -----------
cure_data <- read.csv("data/output/cure_data.csv") %>%
  select("precinct", "shootings_count", "shootings_per_person" , "year", "cure")
cure_data$cure<- as.character(cure_data$cure)

# calculate lag percent change -----------
cure_data_lags <- cure_data %>%
  group_by(precinct) %>%
  filter(year == max(year)| # get the last year in cure
           year >= 2010 |# look behind two years from earliest program (2012)
           (cure != "0" & lead(cure) == "1") ) %>% # entering cure & look ahead
  group_by(precinct) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(pct_change = (shootings_per_person/lag(shootings_per_person) - 1) * 100) %>%
  filter(year != 2010) %>% # remove na
  select(!c(shootings_count, shootings_per_person, cure)) %>%
  mutate(pct_change = round(pct_change)) %>%
  pivot_wider(names_from = year,
              values_from = pct_change) %>%  # table format
  left_join(programs_20 %>% select(precinct, year)) %>% # add year entered cure
  arrange(year) %>%
  # rowwise() %>%  # add sparkline to chart
  # mutate(sparkline = list(c_across(`2011`:`2019`)) ),
  #        across(`2011`:`2019`, scales::percent(.x,accuracy = 2, scale = 1 )) )


  ###### gt table ---------
gt_table <- cure_data_lags %>% select(-year) %>%
  gt(rowname_col = "precinct", groupname_col = "groupname") %>%
  # gtExtras::gt_plt_sparkline(sparkline) %>%
  tab_header(title = "Percent Change in the Number of Shootings from Previous Year",
             subtitle = "For Precincts Participating in CURE Sorted by Year of Entry") %>%
  gt_theme_nytimes() %>%
  data_color(columns = starts_with("20"),
             rows = everything(),
             colors = scales::col_bin(
               bins = c(-Inf, -5,5,Inf),
               palette = c("#cedaf1", "#F9F9F9","#F9F9F9","#E6E6E6")),
             contrast_algo = "wcag") %>%
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

