# Step 02 - Flight Network Mapping
# Alec Stashevsky
# January 5, 2024


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(readxl)
library(openxlsx)
library(maps)
# library(maptools)
library(geosphere)
library(plyr)


# Import  -----------------------------------------------------------------
path.in <-"~/Git/ASRS-climate-research/Data/"

geounique <- readRDS(paste0(path.in, "geounqiue.rds"))
conventions <- readRDS(paste0(path.in, "geocoded_conventions.rds"))
