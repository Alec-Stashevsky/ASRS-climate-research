# Step 05 - Query Flight Emissions
# Alec Stashevsky
# January 6, 2024


# Setup -------------------------------------------------------------------
rm(list = ls())
library(readxl)
library(openxlsx)
library(tidyverse)
library(data.table)
library(httr)
library(jsonlite)

path.in <-"~/Git/ASRS-climate-research/Data/ASRS Geodistance + Airport Data.xlsx"
path.out <-"~/Git/ASRS-climate-research/Output/"

# From GoClimate
api.key <- Sys.getenv("GOCLIMATE_API_KEY")

# Import ------------------------------------------------------------------
YYZ <- read_excel(path.in, sheet = "YYZ 2013") %>% setDT()
SAN <- read_excel(path.in, sheet = "SAN 2014") %>% setDT()
VIE <- read_excel(path.in, sheet = "VIE 2015") %>% setDT()
SFO <- read_excel(path.in, sheet = "SFO 2016") %>% setDT()
BOS <- read_excel(path.in, sheet = "BOS 2017") %>% setDT()
conventions <- read_excel(path.in, sheet = "Convention Centers") %>% setDT()

# Make sure convention are sorted by Meeting Year
conventions <- conventions[order(`Meeting Year`)]

# Build API Queries -------------------------------------------------------

# Set Query Parameters
seat.class <- "economy" # Assume most people will fly economy class

# Initialize list
query.list.YYZ <- vector(mode = "list", length = nrow(conventions))
query.list.SAN <- vector(mode = "list", length = nrow(conventions))
query.list.VIE <- vector(mode = "list", length = nrow(conventions))
query.list.SFO <- vector(mode = "list", length = nrow(conventions))
query.list.BOS <- vector(mode = "list", length = nrow(conventions))
names(query.list.YYZ) <- conventions$IATA
names(query.list.SAN) <- conventions$IATA
names(query.list.VIE) <- conventions$IATA
names(query.list.SFO) <- conventions$IATA
names(query.list.BOS) <- conventions$IATA

query.list <- list(query.list.YYZ, query.list.SAN, query.list.VIE, query.list.SFO, query.list.BOS)
names(query.list) <- conventions$IATA


query_builder <- function(data, destinations, seat.class) {

  # Extract origin IATA
  return_list <- list()
  origins <- data$IATA

  # Build query
  for (destination in destinations) {

    # Build interim query vector
    query.vec <- rep(NA, length(origins))

    # Generate list for each round-trip query
    for (i in 1:length(origins)) {

      query <- paste0("https://api.goclimate.com/v1/flight_footprint?segments[0][origin]=",
        origins[i],          # Loop through each unique origin airport
        "&segments[0][destination]=",
        destination,         # Nearest airport to convention center
        "&segments[1][origin]=",
        destination,         # 2nd-leg of round trip flight
        "&segments[1][destination]=",
        origins[i],          # Return home
        "&cabin_class=",
        seat.class,              # Seat class of passenger
        "&currencies[]=USD")     # Price offsets in USD

      query.vec[i] <- query

    }

    # Put vector in list
    return_list[[destination]] <- query.vec

  }
  return(return_list)
}

# Build queries
for (meeting_name in names(query.list)) {
  query.list[[meeting_name]] <- query_builder(get(meeting_name), conventions$IATA, seat.class)
}


# Query GoClimate API -----------------------------------------------------

# Query function
query_goclimate <- function(queries, destinations, api.key) {


  response.list <- vector(mode = "list", length = length(queries))
  names(response.list) <- names(queries)

  for (destination in names(queries)) {

    # Initialize query responses
    responses.footprint <- rep(NA, length(queries[[destination]]))
    responses.offsets <- rep(NA, length(queries[[destination]]))

    # Loop through all queries in query.list
    for (i in 1:length(queries[[destination]])) {

      r <- GET(queries[[destination]][i],
               authenticate(api.key, ""))

      tryCatch({
        c <- fromJSON(content(r, as = "text"))
      },

      error = function(e){
        print(paste0("QUERY FAIL ON REQUEST: ", i, " STATUS CODE: ", r$status_code))
        Sys.sleep(62)
      })



      # Ignore null return values from API
      tryCatch({
        responses.footprint[i] <- as.numeric(c$footprint)
        responses.offsets[i] <- as.numeric(c$offset_prices[1])
      },

      error = function(e) {
        print(paste0("NULL QUERY ", i))
      })

      # Add status output
      if (i %% 100 == 0) {
        print(paste("On query", i, "of destination", destination))
      }

      Sys.sleep(0.2)

    }

    # Coerce responses into list
    response.list[[destination]] <- data.table(
      "Footprint" = responses.footprint,
      "Offset" = responses.offsets
    )

    # Be explicit in column names
    setnames(
      response.list[[destination]],
      c(paste0("Footprint.", destination),
        paste0("Offset.", destination)
      )
    )
  }

  return(response.list)

}

# Query GoClimate API

goclimate_results <- vector(mode = "list", length = length(query.list))
names(goclimate_results) <- names(query.list)

for (query_name in names(query.list)) {
  goclimate_results[[query_name]] <- query_goclimate(query.list[[query_name]], conventions$IATA, api.key)
}


# Export ------------------------------------------------------------------
saveRDS(goclimate_results, paste0(path.out, "goclimate_results.rds"))

