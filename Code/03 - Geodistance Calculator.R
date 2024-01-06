# Step 03 - Geodistance Calculator
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
path.in <-"~/Git/ASRS-climate-research/Data/"
path.out <-"~/Git/ASRS-climate-research/Data/"
geounique <- readRDS(paste0(path.in, "geounique.rds"))
conventions <- readRDS(paste0(path.in, "geocoded_conventions.rds"))
conventions$IATA <- c("YYZ", "SAN", "VIE", "SFO", "BOS") #


# Preprocess Data ---------------------------------------------------------

# Assign a new variable for every data.table in the geo-unique list
for (i in 1:length(geounique)) {
  assign(conventions$IATA[i], geounique[[i]])
}

dests <- conventions[, .(lon, lat)]


# Calculate Geodistance ---------------------------------------------------
for(i in 1:nrow(conventions)){
  assign(paste0("YYZ.gdist.", conventions$IATA[i]),
         distGeo(YYZ[, c('lon', 'lat')], dests[i]))
}

for(i in 1:nrow(conventions)){
  assign(paste0("SAN.gdist.", conventions$IATA[i]),
         distGeo(SAN[, c('lon', 'lat')], dests[i]))
}

for(i in 1:nrow(conventions)){
  assign(paste0("VIE.gdist.", conventions$IATA[i]),
         distGeo(VIE[, c('lon', 'lat')], dests[i]))
}

for(i in 1:nrow(conventions)){
  assign(paste0("SFO.gdist.", conventions$IATA[i]),
         distGeo(SFO[, c('lon', 'lat')], dests[i]))
}

for(i in 1:nrow(conventions)){
  assign(paste0("BOS.gdist.", conventions$IATA[i]),
         distGeo(BOS[, c('lon', 'lat')], dests[i]))
}


# Merge distances together
YYZ.gdist.total <- as.data.table(cbind(
  gdist.YYZ = YYZ.gdist.YYZ,
  gdist.SAN = YYZ.gdist.SAN,
  gdist.VIE = YYZ.gdist.VIE,
  gdist.SFO = YYZ.gdist.SFO,
  gdist.BOS = YYZ.gdist.BOS
 )
)

SAN.gdist.total <- as.data.table(cbind(
  gdist.YYZ = SAN.gdist.YYZ,
  gdist.SAN = SAN.gdist.SAN,
  gdist.VIE = SAN.gdist.VIE,
  gdist.SFO = SAN.gdist.SFO,
  gdist.BOS = SAN.gdist.BOS
 )
)

VIE.gdist.total <- as.data.table(cbind(
  gdist.YYZ = VIE.gdist.YYZ,
  gdist.SAN = VIE.gdist.SAN,
  gdist.VIE = VIE.gdist.VIE,
  gdist.SFO = VIE.gdist.SFO,
  gdist.BOS = VIE.gdist.BOS
 )
)

SFO.gdist.total <- as.data.table(cbind(
  gdist.YYZ = SFO.gdist.YYZ,
  gdist.SAN = SFO.gdist.SAN,
  gdist.VIE = SFO.gdist.VIE,
  gdist.SFO = SFO.gdist.SFO,
  gdist.BOS = SFO.gdist.BOS
 )
)

BOS.gdist.total <- as.data.table(cbind(
  gdist.YYZ = BOS.gdist.YYZ,
  gdist.SAN = BOS.gdist.SAN,
  gdist.VIE = BOS.gdist.VIE,
  gdist.SFO = BOS.gdist.SFO,
  gdist.BOS = BOS.gdist.BOS
 )
)

# Merge onto Geo-unique data
final.YYZ <- cbind(YYZ, YYZ.gdist.total)
final.SAN <- cbind(SAN, SAN.gdist.total)
final.VIE <- cbind(VIE, VIE.gdist.total)
final.SFO <- cbind(SFO, SFO.gdist.total)
final.BOS <- cbind(BOS, BOS.gdist.total)


# Driver Indicator --------------------------------------------------------
drive.threshold <- 241402 # 150 miles = 241402 meters

# Create indicator variable for driving if less than 400 km away from meeting
final.YYZ[, `:=`(
  drive.YYZ = ifelse(gdist.YYZ <= drive.threshold, 1, 0),
  drive.SAN = ifelse(gdist.SAN <= drive.threshold, 1, 0),
  drive.VIE = ifelse(gdist.VIE <= drive.threshold, 1, 0),
  drive.SFO = ifelse(gdist.SFO <= drive.threshold, 1, 0),
  drive.BOS = ifelse(gdist.BOS <= drive.threshold, 1, 0)
)]

final.SAN[, `:=`(
  drive.YYZ = ifelse(gdist.YYZ <= drive.threshold, 1, 0),
  drive.SAN = ifelse(gdist.SAN <= drive.threshold, 1, 0),
  drive.VIE = ifelse(gdist.VIE <= drive.threshold, 1, 0),
  drive.SFO = ifelse(gdist.SFO <= drive.threshold, 1, 0),
  drive.BOS = ifelse(gdist.BOS <= drive.threshold, 1, 0)
)]

final.VIE[, `:=`(
  drive.YYZ = ifelse(gdist.YYZ <= drive.threshold, 1, 0),
  drive.SAN = ifelse(gdist.SAN <= drive.threshold, 1, 0),
  drive.VIE = ifelse(gdist.VIE <= drive.threshold, 1, 0),
  drive.SFO = ifelse(gdist.SFO <= drive.threshold, 1, 0),
  drive.BOS = ifelse(gdist.BOS <= drive.threshold, 1, 0)
)]

final.SFO[, `:=`(
  drive.YYZ = ifelse(gdist.YYZ <= drive.threshold, 1, 0),
  drive.SAN = ifelse(gdist.SAN <= drive.threshold, 1, 0),
  drive.VIE = ifelse(gdist.VIE <= drive.threshold, 1, 0),
  drive.SFO = ifelse(gdist.SFO <= drive.threshold, 1, 0),
  drive.BOS = ifelse(gdist.BOS <= drive.threshold, 1, 0)
)]

final.BOS[, `:=`(
  drive.YYZ = ifelse(gdist.YYZ <= drive.threshold, 1, 0),
  drive.SAN = ifelse(gdist.SAN <= drive.threshold, 1, 0),
  drive.VIE = ifelse(gdist.VIE <= drive.threshold, 1, 0),
  drive.SFO = ifelse(gdist.SFO <= drive.threshold, 1, 0),
  drive.BOS = ifelse(gdist.BOS <= drive.threshold, 1, 0)
)]



# Descriptive Statistics --------------------------------------------------

# Summary Stats for unique location
summary(final.YYZ$gdist.YYZ)
summary(final.SAN$gdist.SAN)
summary(final.VIE$gdist.VIE)
summary(final.SFO$gdist.SFO)
summary(final.BOS$gdist.BOS)

# Summary Stats for total distances traveled
summary(final.YYZ$Frequency * final.YYZ$gdist.YYZ)
summary(final.SAN$Frequency * final.SAN$gdist.SAN)
summary(final.VIE$Frequency * final.VIE$gdist.VIE)
summary(final.SFO$Frequency * final.SFO$gdist.SFO)
summary(final.BOS$Frequency * final.BOS$gdist.BOS)

# Total distance traveled in kilometers
total.YYZ <- sum(final.YYZ$Frequency * final.YYZ$gdist.YYZ, na.rm = TRUE)/1000
total.SAN <- sum(final.SAN$Frequency * final.SAN$gdist.SAN, na.rm = TRUE)/1000
total.VIE <- sum(final.VIE$Frequency * final.VIE$gdist.VIE, na.rm = TRUE)/1000
total.SFO <- sum(final.SFO$Frequency * final.SFO$gdist.SFO, na.rm = TRUE)/1000
total.BOS <- sum(final.BOS$Frequency * final.BOS$gdist.BOS, na.rm = TRUE)/1000

# Build table
total.distance <- rbind(
  total.YYZ,
  total.SAN,
  total.VIE,
  total.SFO,
  total.BOS
)

# Drivers Vs. Flyers ------------------------------------------------------


# Filter for drivers
drive.YYZ <- final.YYZ[drive.YYZ == 1]
drive.SAN <- final.SAN[drive.SAN == 1]
drive.VIE <- final.VIE[drive.VIE == 1]
drive.SFO <- final.SFO[drive.SFO == 1]
drive.BOS <- final.BOS[drive.BOS == 1]

# Filter to flyers
fly.YYZ <- final.YYZ[drive.YYZ != 1]
fly.SAN <- final.SAN[drive.SAN != 1]
fly.VIE <- final.VIE[drive.VIE != 1]
fly.SFO <- final.SFO[drive.SFO != 1]
fly.BOS <- final.BOS[drive.BOS != 1]

# Calculated percentage of drivers
YYZ.drive.prop <- sum(drive.YYZ$Frequency) / sum(final.YYZ$Frequency)
SAN.drive.prop <- sum(drive.SAN$Frequency) / sum(final.SAN$Frequency)
VIE.drive.prop <- sum(drive.VIE$Frequency) / sum(final.VIE$Frequency)
SFO.drive.prop <- sum(drive.SFO$Frequency) / sum(final.SFO$Frequency)
BOS.drive.prop <- sum(drive.BOS$Frequency) / sum(final.BOS$Frequency)

# Build table
drive.prop <- rbind(
  YYZ.drive.prop,
  SAN.drive.prop,
  VIE.drive.prop,
  SFO.drive.prop,
  BOS.drive.prop
)


# Summary Stats for total distances traveled
summary(drive.YYZ$Frequency * drive.YYZ$gdist.YYZ)
summary(drive.SAN$Frequency * drive.SAN$gdist.SAN)
summary(drive.VIE$Frequency * drive.VIE$gdist.VIE)
summary(drive.SFO$Frequency * drive.SFO$gdist.SFO)
summary(drive.BOS$Frequency * drive.BOS$gdist.BOS)

# Total one-way distance traveled in kilometers
## Drivers
total.drive.YYZ <- sum(drive.YYZ$Frequency * drive.YYZ$gdist.YYZ,
                       na.rm = TRUE)/1000
total.drive.SAN <- sum(drive.SAN$Frequency * drive.SAN$gdist.SAN,
                       na.rm = TRUE)/1000
total.drive.VIE <- sum(drive.VIE$Frequency * drive.VIE$gdist.VIE,
                       na.rm = TRUE)/1000
total.drive.SFO <- sum(drive.SFO$Frequency * drive.SFO$gdist.SFO,
                       na.rm = TRUE)/1000
total.drive.BOS <- sum(drive.BOS$Frequency * drive.BOS$gdist.BOS,
                       na.rm = TRUE)/1000
# Build table
total.drive <- rbind(
  total.drive.YYZ,
  total.drive.SAN,
  total.drive.VIE,
  total.drive.SFO,
  total.drive.BOS
)

## Flyers
total.fly.YYZ <- sum(fly.YYZ$Frequency * fly.YYZ$gdist.YYZ,
                     na.rm = TRUE)/1000
total.fly.SAN <- sum(fly.SAN$Frequency * fly.SAN$gdist.SAN,
                     na.rm = TRUE)/1000
total.fly.VIE <- sum(fly.VIE$Frequency * fly.VIE$gdist.VIE,
                     na.rm = TRUE)/1000
total.fly.SFO <- sum(fly.SFO$Frequency * fly.SFO$gdist.SFO,
                     na.rm = TRUE)/1000
total.fly.BOS <- sum(fly.BOS$Frequency * fly.BOS$gdist.BOS,
                     na.rm = TRUE)/1000
# Build table
total.fly <- rbind(
  total.fly.YYZ,
  total.fly.SAN,
  total.fly.VIE,
  total.fly.SFO,
  total.fly.BOS
)


# International Vs. Domestic ----------------------------------------------

# Do this later, because its not totally cleaned up for United States



# Final Export ------------------------------------------------------------
table.names <- c("YYZ 2013", "SAN 2014", "VIE 2015", "SFO 2016", "BOS 2017")

total.table <- as.data.frame(cbind(table.names,
                                   total.drive,
                                   total.fly,
                                   drive.prop,
                                   # total.domestic,
                                   # total.international,
                                   total.distance
                                   # dom.prop
                                   ))

# Fix headers
colnames(total.table) <- c("Meeting Location",
                           "Distance Driven (km)",
                           "Distance Flown (km)",
                           "Proportion of Drivers (%)",
                           # "Domestic Travel (km)",
                           # "International Travel (km)",
                           "Total (km)"
                           # "Domestic Attendance (%)"
                           )


# Export ------------------------------------------------------------------

final.sets <- list(
  "YYZ 2013" = final.YYZ,
  "SAN 2014" = final.SAN,
  "VIE 2015" = final.VIE,
  "SFO 2016" = final.SFO,
  "BOS 2017" = final.BOS,
  "Convention Centers" = conventions,
  "Summary Statistics" = total.table
)

write.xlsx(
  final.sets,
  file = paste0(path.out, "ASRS Geodistance Data.xlsx")
)
