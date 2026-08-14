# run_map_pipeline.R
   # Run from project root: source("run_map_pipeline.R")

   source("code/00_load_dependencies.R")
   rmarkdown::render("code/00_read_data.Rmd", envir = globalenv())
   source("code/01_cohort_map.R", echo = TRUE)