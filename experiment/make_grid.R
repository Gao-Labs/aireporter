# make_grid.R

# This script details the grid setup
# for a factorial design experiment
# for quality checking report_v5

# We will make a table called experiment.csv,
# and we will continuously add rows to it,
# recording quality checking results.

# We will make a folder called 'reports'

# Set working directory
setwd(paste0(rstudioapi::getActiveProject(), "/report_v5"))

# If it doesn't already exist, create this folder for containing reports
if(!file.exists("experiment/reports")){ dir.create("experiment/reports") }

# Make file id number
# id = runif(n = 1, min = 0, max = 100000000) %>%
#   floor() %>%
#   stringr::str_pad(width = nchar("100000000"), pad = "0", side = "left")


options = read_csv("options.csv")

# load("experiment/choices_geoid_counties.rda")
# choices_geoid_counties %>% purrr::map_dfr(~tibble(geoid = unname(.), county = names(.)), .id = "state") %>%
#   saveRDS("experiment/options_geoid.rds")


# Steps...
# Pick a geoid
# Pick a year
# Pick a pollutant (maybe not?)
# Pick 10 graphs, including...

pollutants = read_rds("experiment/choices_pollutant.rds") %>%
  .[. %in% c("98", "87", "31", "3", "2", "110", "100")] %>%
  as.integer()

# Create a sampling frame
read_rds("experiment/options_geoid.rds") %>%
  filter(state != "Puerto Rico Municipios") %>%
  group_by(state) %>%
  sample_n(size = 4, replace = TRUE) %>%
  ungroup() %>%
  mutate(year = sample(x =  seq(from = 2000, to = 2060, by = 5), size = n(), replace = TRUE)) %>%
  mutate(pollutant = sample(x = pollutants, size = n(), replace = TRUE)) %>%
  mutate(report_id = runif(n = n(), min = 0, max = 100000000) %>%
           floor() %>%
           stringr::str_pad(width = nchar("100000000"), pad = "0", side = "left")) %>%
  write_csv("experiment/grid_geoid_year_pollutant.csv")


remove(pollutants)

# Randomly select 10 varieties
options = read_csv("options.csv") %>%
  filter(graph != "profile")

grid = read_csv("experiment/grid_geoid_year_pollutant.csv")
tibble(report_id = grid$report_id) %>%
  split(.$report_id) %>%
  purrr::map_dfr(.f = ~options %>%
                   group_by(graph) %>%
                   sample_n(size = 2),
                 .id = "report_id") %>%
  write_csv("experiment/grid_graphs.csv")

read_csv("experiment/grid_graphs.csv") %>%
  group_by(report_id) %>%
  mutate(key_id = str_pad(1:n(), width = 2, side = "left", pad = "0") ) %>%
  left_join(by = "report_id", y = grid) %>%
  ungroup() %>%
  mutate(id = paste0(report_id, key_id)) %>%
  saveRDS("experiment/grid.rds")

# read_rds("experiment/grid.rds") %>%
#   saveRDS("experiment/grid.rds")

# bind_rows(
#   read_rds("experiment/grid.rds"),
#   read_rds("experiment/grid2.rds")
# ) %>%
#   saveRDS("experiment/grid.rds")

rm(list = ls())
