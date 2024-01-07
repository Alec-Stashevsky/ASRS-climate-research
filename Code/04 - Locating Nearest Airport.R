# Step 04 - Locating Nearest Airport
# Alec Stashevsky
# January 5, 2024


# Setup -------------------------------------------------------------------
library(readxl)
library(openxlsx)
library(tidyverse)
library(data.table)
library(geosphere)
rm(list = ls())


# Import  -----------------------------------------------------------------
path.in <-"~/Git/ASRS-climate-research/Data/ASRS Geodistance Data.xlsx"
path.out <-"~/Git/ASRS-climate-research/Data/"
path.airports <- "~/Git/ASRS-climate-research/Data/OpenFlights/airports.txt"
path.misqueries <- "~/Git/ASRS-climate-research/Data/OpenFlights/"
path.airports.supp <- "~/Git/ASRS-climate-research/Data/OpenFlights/AAO Airport Supplement.xlsx"

YYZ <- read_excel(path.in, sheet = "YYZ 2013") %>% setDT()
SAN <- read_excel(path.in, sheet = "SAN 2014") %>% setDT()
VIE <- read_excel(path.in, sheet = "VIE 2015") %>% setDT()
SFO <- read_excel(path.in, sheet = "SFO 2016") %>% setDT()
BOS <- read_excel(path.in, sheet = "BOS 2017") %>% setDT()
conventions <- read_excel(path.in, sheet = "Convention Centers") %>% setDT()
summary.stats <- read_excel(path.in, sheet = "Summary Statistics") %>% setDT()


# OpenFlights Airports Database
airports.raw <- fread(path.airports)

# Misqueries from previous projects
misqueries <- read_excel(paste0(path.misqueries, "GoClimate API Misqueries.xlsx"), sheet = "Misqueries") %>% setDT()
new.misqueries <- read_excel(paste0(path.misqueries, "new-misqueries.xlsx")) %>% setDT()

# Import Airport Supplement (Manual Research to impute OpenFlights)
airports.supp <- read_excel(paste0(path.airports.supp)) %>% setDT() # might not be needed, but include anyway



# Preprocess Data ---------------------------------------------------------

# Drop columns where lon and lat are NA
YYZ <- YYZ[!is.na(lon) & !is.na(lat)]
SAN <- SAN[!is.na(lon) & !is.na(lat)]
VIE <- VIE[!is.na(lon) & !is.na(lat)]
SFO <- SFO[!is.na(lon) & !is.na(lat)]
BOS <- BOS[!is.na(lon) & !is.na(lat)]


# Drop extraneous columns
airports <- airports.raw[, -c(12:14)]

# Set column names
setnames(airports,
  c("OpenFlights ID",
   "Airport Name",
   "Airport City",
   "Airport Country",
   "IATA",
   "ICAO",
   "Latitude",
   "Longitude",
   "Altitude",
   "Timezone",
   "DST")
)

misquery.iata <- unique(c(misqueries$IATA, new.misqueries$IATA))

airports.clean <- airports[
  # Exclude airports w/o coordinates and IATA codes
  Latitude != 0 & Longitude != 0 & IATA != "\\N"][
    # Filter out any which cause GoClimate Misqueries
    !(IATA %in% misquery.iata)]

# Add in supplement
airports.clean <- rbind(airports.clean, airports.supp, use.names = TRUE, fill = TRUE)



# Enrich Conventions Data -------------------------------------------------

# Join conventions with airports on IATA code
conventions.airports <- merge(
  x = conventions,
  y = airports.clean,
  by.x = "IATA",
  by.y = "IATA",
  all.x = TRUE
)

conventions.airports <- conventions.airports[order(`Meeting Year`)]



# Locate Nearest Airport --------------------------------------------------

## YYZ ##

# Initialize logs
YYZ.airport.log <- rep(NA, nrow(YYZ))
YYZ.airport.dist <- rep(NA, nrow(YYZ))

# Find the closest airport for each YYZ attendee origin
for (i in 1:nrow(YYZ)) {
  YYZ.airport.log[i] <- which.min(
    distGeo(
      c(YYZ$lon[i], YYZ$lat[i]),
      airports.clean[, c('Longitude', 'Latitude')]
      )
    )

  YYZ.airport.dist[i] <- min(
    distGeo(
      c(YYZ$lon[i], YYZ$lat[i]),
      airports.clean[, c('Longitude', 'Latitude')]
      )
    )
}


# Get list of attendee airport codes
closest.airports <- airports.clean[YYZ.airport.log]

# Merge onto YYZ data
YYZ.final <- cbind(
  YYZ,
  closest.airports,
  airport_dist_meters = YYZ.airport.dist
)


## SAN ##

# Initialize logs
SAN.airport.log <- rep(NA, nrow(SAN))
SAN.airport.dist <- rep(NA, nrow(SAN))

# Find the closest airport for each SAN attendee origin
for (i in 1:nrow(SAN)) {
  SAN.airport.log[i] <- which.min(
    distGeo(
      c(SAN$lon[i], SAN$lat[i]),
      airports.clean[, c('Longitude', 'Latitude')]
    )
  )

  SAN.airport.dist[i] <- min(
    distGeo(
      c(SAN$lon[i], SAN$lat[i]),
      airports.clean[, c('Longitude', 'Latitude')]
    )
  )
}


# Get list of attendee airport codes
closest.airports <- airports.clean[SAN.airport.log]

# Merge onto SAN data
SAN.final <- cbind(
  SAN,
  closest.airports,
  airport_dist_meters = SAN.airport.dist
)


## VIE ##

# Initialize logs
VIE.airport.log <- rep(NA, nrow(VIE))
VIE.airport.dist <- rep(NA, nrow(VIE))

# Find the closest airport for each VIE attendee origin
for (i in 1:nrow(VIE)) {
  VIE.airport.log[i] <- which.min(
    distGeo(
      c(VIE$lon[i], VIE$lat[i]),
      airports.clean[, c('Longitude', 'Latitude')]
    )
  )

  VIE.airport.dist[i] <- min(
    distGeo(
      c(VIE$lon[i], VIE$lat[i]),
      airports.clean[, c('Longitude', 'Latitude')]
    )
  )
}


# Get list of attendee airport codes
closest.airports <- airports.clean[VIE.airport.log]

# Merge onto VIE data
VIE.final <- cbind(
  VIE,
  closest.airports,
  airport_dist_meters = VIE.airport.dist
)


## SFO ##

# Initialize logs
SFO.airport.log <- rep(NA, nrow(SFO))
SFO.airport.dist <- rep(NA, nrow(SFO))

# Find the closest airport for each SFO attendee origin
for (i in 1:nrow(SFO)) {
  SFO.airport.log[i] <- which.min(
    distGeo(
      c(SFO$lon[i], SFO$lat[i]),
      airports.clean[, c('Longitude', 'Latitude')]
    )
  )

  SFO.airport.dist[i] <- min(
    distGeo(
      c(SFO$lon[i], SFO$lat[i]),
      airports.clean[, c('Longitude', 'Latitude')]
    )
  )
}


# Get list of attendee airport codes
closest.airports <- airports.clean[SFO.airport.log]

# Merge onto SFO data
SFO.final <- cbind(
  SFO,
  closest.airports,
  airport_dist_meters = SFO.airport.dist
)


## BOS ##

# Initialize logs
BOS.airport.log <- rep(NA, nrow(BOS))
BOS.airport.dist <- rep(NA, nrow(BOS))

# Find the closest airport for each BOS attendee origin
for (i in 1:nrow(BOS)) {
  BOS.airport.log[i] <- which.min(
    distGeo(
      c(BOS$lon[i], BOS$lat[i]),
      airports.clean[, c('Longitude', 'Latitude')]
    )
  )

  BOS.airport.dist[i] <- min(
    distGeo(
      c(BOS$lon[i], BOS$lat[i]),
      airports.clean[, c('Longitude', 'Latitude')]
    )
  )
}


# Get list of attendee airport codes
closest.airports <- airports.clean[BOS.airport.log]

# Merge onto BOS data
BOS.final <- cbind(
  BOS,
  closest.airports,
  airport_dist_meters = BOS.airport.dist
)



# Export ------------------------------------------------------------------
final.sets <- list(
  "YYZ 2013" = YYZ.final,
  "SAN 2014" = SAN.final,
  "VIE 2015" = VIE.final,
  "SFO 2016" = SFO.final,
  "BOS 2017" = BOS.final,
  "Convention Centers" = conventions.airports,
  "Summary Statistics" = summary.stats
)

write.xlsx(
  final.sets,
  paste0(path.out, "ASRS Geodistance + Airport Data.xlsx"),
  overwrite = TRUE
 )
