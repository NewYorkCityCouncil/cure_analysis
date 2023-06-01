# run 00_read_data.Rmd
# read in shootings data
cure_data <- read.csv("data/output/cure_data.csv") %>%
  select("precinct", "shootings_count", "shootings_per_person" , "year", "cure")
cure_data$cure<- as.character(cure_data$cure)

cure_data_lags <- cure_data %>%
  group_by(precinct) %>%
  filter( lag(cure) == "0" & lead(cure) == "1" |
            (cure != "0" & lead(cure,2) == "1")) %>% #look in front & behind
  filter(year == max(year))

write.csv(cure_data_lags, "cure_before-in-after.csv", row.names = F)


########

cohort_shp <- programs_20 %>%
  select(1:6, 9:11)) %>%
  mutate(year = paste0(rep(20,nrow(.)),year),
         year = case_when(year=="20NA" ~ NA,
                          TRUE ~ year)) %>%
  left_join(cure_data, by= c('precinct', 'year')) %>%

