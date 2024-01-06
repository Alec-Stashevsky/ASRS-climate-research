# Step 01 - Data Preparation
# Alec Stashevsky
# January 3, 2024


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(readxl)
library(countrycode)
library(ggmap)

path.in <- "~/Git/ASRS-climate-research/Data/Raw/"
path.out <- "~/Git/ASRS-climate-research/Data/"


# Import AAO Data ----------------------------------------------------------
raw.sheets <- excel_sheets(paste0(path.in, "AM Raw Formatted.xlsx"))
raw.conventions <- read_excel(path = paste0(path.in, "Conventions.xlsx")) %>% setDT()

# Initialize
raw.list <- vector(mode = "list", length(raw.sheets))
names(raw.list) <- raw.sheets

i <- 1

# Import each sheet in raw excel file
for (sheet in raw.sheets) {

  raw.list[[i]] <- read_excel(
    path = paste0(path.in, "AM Raw Formatted.xlsx"),
    sheet = sheet
  ) %>% setDT()

  i <- i + 1
}


# Decode Country Names -----------------------------------------------------
country.names <- raw.list
names(country.names) <- raw.sheets

i <- 1
for (meeting in raw.list) {
  country.names[[i]]$`Country Name` <- countrycode(meeting$Country, "iso2c", 'country.name', nomatch = NULL)
  i <- i + 1
}

# There are some issues with multiple versions of the same country names,
#  but I think its good enough to feed to the Google Maps API this way.

# Get all the unique values in the `states` column for every data.table in the country.name list

unique_states <- vector(mode = "list", length(raw.sheets))
unique_country <- vector(mode = "list", length(raw.sheets))
unique_country_name <- vector(mode = "list", length(raw.sheets))
unique_postal <- vector(mode = "list", length(raw.sheets))
names(unique_states) <- raw.sheets
names(unique_country) <- raw.sheets
names(unique_country_name) <- raw.sheets
names(unique_postal) <- raw.sheets


i <- 1
for (meeting in country.names) {
  # Combine into a single vector
  unique_states[[i]] <- unique(tolower(meeting$State))
  unique_country[[i]] <- unique(tolower(meeting$Country))
  unique_country_name[[i]] <- unique(tolower(meeting$`Country Name`))
  unique_postal[[i]] <- unique(tolower(meeting$`Postal code`))
  i <- i + 1
}

# Combine list into a single vector, with lower case
unique_states <- unique(unlist(unique_states))
unique_country <- unique(unlist(unique_country))
unique_country_name <- unique(unlist(unique_country_name))
unique_postal <- unique(unlist(unique_postal))


# Prep Geocode Requests ---------------------------------------------------
data.to.geocode <- vector(mode = "list", length(raw.sheets))
names(data.to.geocode) <- raw.sheets

i <- 1
for (meeting in raw.list) {
  data.to.geocode[[i]] <- country.names[[i]][, .(City, State, `Postal code`, Country, `Country Name`)] %>% unite(
    col = "location",
    c(City, State, `Postal code`, `Country Name`),
    sep=", ",
    remove = FALSE,
    na.rm = TRUE
  )
  i <- i + 1
}


# Geocode with Google Maps API --------------------------------------------
geocoded <- vector(mode = "list", length(raw.sheets))
names(geocoded) <- raw.sheets

i <- 1
for (data in data.to.geocode) {
  geocoded[[i]] <- mutate_geocode(data, location = location) %>% setDT()
  i <- i + 1
}


# Check if any lon, lat values are NA, if so print the location name
i <- 1
for (meeting in geocoded) {
  if (any(is.na(meeting$lon))) {
    print(paste(raw.sheets[i], meeting$location[is.na(meeting$lon)], sep = ": "))
  }
  i <- i + 1
}


# Did some Googling to impute the missing values
# "Cali, 11001, Columbia" -->  3.4226601209885166, -76.52085626223646

# Impute the missing values where location == "Cali, 11001, Columbia"
geocoded[["AM 2016"]][location == "Cali, 11001, Columbia", `lon` := -76.52085626223646]
geocoded[["AM 2016"]][location == "Cali, 11001, Columbia", `lat` := 3.4226601209885166]


# Geocode conventions
conventions.geocoded <- mutate_geocode(raw.conventions, location = location) %>% setDT()


# Convert to Geo Unique Format --------------------------------------------
geounique <- vector(mode = "list", length(geocoded))
names(geounique) <- names(geocoded)
i <- 1
for (i in 1:length(geocoded)) {
  geounique[[i]] <- unique(geocoded[[i]][, Frequency := .N, by = .(lon, lat)],
                              by = c('lon', 'lat'))
  i <- i + 1
}

# Make sure the sum of the frequencies is the same as the original data
i <- 1
for (i in 1:length(geocoded)) {
  if (sum(geounique[[i]]$Frequency) != nrow(geocoded[[i]])) {
    print(paste0("Error in ", names[geounique[[i]]]))
  }
  i <- i + 1
}


# Save --------------------------------------------------------------------

saveRDS(geocoded, file = paste0(path.out, "geocoded.rds"))
saveRDS(conventions.geocoded, file = paste0(path.out, "geocoded_conventions.rds"))
saveRDS(geounique, file = paste0(path.out, "geounique.rds"))
