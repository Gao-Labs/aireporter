# experiment.R

# This script details a factorial design experiment
# for quality checking report_v5

# We will make a table called experiment.csv,
# and we will continuously add rows to it,
# recording quality checking results.

# We will make a folder called 'reports'

# Set working directory
setwd(paste0(rstudioapi::getActiveProject(), "/report_v5"))

# If it doesn't already exist, create this folder for containing reports
if(!file.exists("experiment/reports")){ dir.create("experiment/reports") }

# We will store...
# for file name xxx

# Load libraries
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
library(readr, warn.conflicts = FALSE, quietly = TRUE)
library(stringr, warn.conflicts = FALSE, quietly = TRUE)
library(purrr, warn.conflicts = FALSE, quietly = TRUE)

library(officer, warn.conflicts = FALSE, quietly = TRUE)
library(magrittr, warn.conflicts = FALSE, quietly = TRUE)
library(catviz, warn.conflicts = FALSE, quietly = TRUE)
# Install catviz, if you haven't already
# install.packages("packages/catviz_2.0.tar.gz", type = "source")
library(catviz, warn.conflicts = FALSE, quietly = TRUE) # for making CAT VISUALIZER visuals and tables
library(dplyr, warn.conflicts = FALSE, quietly = TRUE) # for data wrangling
library(knitr, warn.conflicts = FALSE, quietly = TRUE) # for writing markdown tables
# Extra packages you might need - not sure, so best to load them anyway
library(readr, warn.conflicts = FALSE, quietly = TRUE)
library(stringr, warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE)
library(plotly, warn.conflicts = FALSE, quietly = TRUE)
library(viridis, warn.conflicts = FALSE, quietly = TRUE)
library(httr, warn.conflicts = FALSE, quietly = TRUE)
library(jsonlite, warn.conflicts = FALSE, quietly = TRUE)

library(scales, warn.conflicts = FALSE, quietly = TRUE)
library(shadowtext, warn.conflicts = FALSE, quietly = TRUE)
library(ggtext, warn.conflicts = FALSE, quietly = TRUE)
library(colorspace, warn.conflicts = FALSE, quietly = TRUE)
library(sf, warn.conflicts = FALSE, quietly = TRUE)
library(ggspatial, warn.conflicts = FALSE, quietly = TRUE)


# Load files and functions
source("functions.R")
readRenviron(".env")


runs = readRDS("experiment/grid.rds") %>%
  select(report_id, geoid, year, pollutant, key) %>%
  group_by(report_id) %>%
  tidyr::nest(data = key)

# Get ids of completed reports
completed = str_remove(dir("experiment/reports", pattern = ".rds"), "[.]rds")

# Get just the reports remaining
remaining = runs %>%
  filter(!report_id %in% completed)

n = nrow(remaining)

for(i in 1:n){
  # Print start message
  cat("\n", i, "/", n, " starting... -------------------------\n")
  # Build parameters
  params = list(
    table = paste0("granddata.d", remaining$geoid[i]),
    year = remaining$year[i],
    pollutant = remaining$pollutant[i],
    graph = remaining$data[[i]]$key
  )
  # Get file path
  file = paste0("experiment/reports/", remaining$report_id[i], ".docx")
  # Render report
  purrr::possibly(
    .f = ~get_report(params = params, default_cat = FALSE, default_gpt = FALSE, toc = FALSE, file = file, save_data = TRUE ),
    otherwise = NULL)()
  # Print completing message
  cat("\n", i, "/", n, " completed... -------------------------\n")

}

# readRDS("experiment/grid.rds")

# randomly pick a metric then a graph then an aggregation
rm(list = ls())
