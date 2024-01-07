# Step 06 - Total Emissions Estimation
# Alec Stashevsky
# January 6, 2024


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(openxlsx)

path.in <- "~/Git/ASRS-climate-research/Output/ASRS EMISSIONS.xlsx"
path.out <- "~/Git/ASRS-climate-research/Output/"


# Import ------------------------------------------------------------------
YYZ <- read_excel(path.in, sheet = "YYZ 2013") %>% setDT()
SAN <- read_excel(path.in, sheet = "SAN 2014") %>% setDT()
VIE <- read_excel(path.in, sheet = "VIE 2015") %>% setDT()
SFO <- read_excel(path.in, sheet = "SFO 2016") %>% setDT()
BOS <- read_excel(path.in, sheet = "BOS 2017") %>% setDT()
conventions <- read_excel(path.in, sheet = "Convention Centers") %>% setDT()
summary.stats <- read_excel(path.in, sheet = "Summary Statistics") %>% setDT()

# Set Conversion Factors --------------------------------------------------
meter.to.mile <- 0.000621371
grams.to.kg <- 0.001
car.emissons.grams.per.mile <- 404
conversion.factor <- meter.to.mile * car.emissons.grams.per.mile * grams.to.kg # converts meters to to kg of CO2e

accomdation.dist <- 30 * 1 / meter.to.mile # Driving distance between SFO and OAK is used

# The meetings are of different lengths
#  5 days for YYZ - August 24 - 28, 2013
#  5 days for SAN - August 09 - 13, 2014
#  4 days for VIE -   July 11 - 14, 2015
#  6 days for SFO - August 09 - 14, 2016
#  5 days for BOS - August 11 - 15, 2017

meeting_days <- list(
  "YYZ" = 5,
  "SAN" = 5,
  "VIE" = 4,
  "SFO" = 6,
  "BOS" = 5
)


# Calculate Total Emissions -----------------------------------------------

# Overwrite the original tables in-place
for (meeting_name in conventions$IATA) {
  get(meeting_name)[, `:=`(
    "Total Emissions YYZ (Kg)" = fcase(
      # Round trip driving emissions for each day (Kg)
      drive.YYZ == 1 & gdist.YYZ < accomdation.dist,
      Frequency * 2 * gdist.YYZ * conversion.factor * meeting_days[[meeting_name]],

      # Round trip driving emissions for 1 trip (Kg)
      drive.YYZ == 1 & gdist.YYZ >=accomdation.dist,
      Frequency * 2 * gdist.YYZ * conversion.factor,

      # Round trip flying emissions (Kg) + Round trip drive to closest airport (kg)
      drive.YYZ == 0,
      Frequency * Footprint.YYZ + (2 * airport_dist_meters * conversion.factor)
    ),

    "Total Emissions SAN (Kg)" = fcase(
      # Round trip driving emissions for 4 trips (Kg)
      drive.SAN == 1 & gdist.SAN < accomdation.dist,
      Frequency * 2 * gdist.SAN * conversion.factor * meeting_days[[meeting_name]],

      # Round trip driving emissions for 1 trip (Kg)
      drive.SAN == 1 & gdist.SAN >=accomdation.dist,
      Frequency * 2 * gdist.SAN * conversion.factor,

      # Round trip flying emissions (Kg)
      drive.SAN == 0,
      Frequency * Footprint.SAN + (2 * airport_dist_meters * conversion.factor)
    ),

    "Total Emissions VIE (Kg)" = fcase(
      # Round trip driving emissions for 4 trips (Kg)
      drive.VIE == 1 & gdist.VIE < accomdation.dist,
      Frequency * 2 * gdist.VIE * conversion.factor * meeting_days[[meeting_name]],

      # Round trip driving emissions for 1 trip (Kg)
      drive.VIE == 1 & gdist.VIE >=accomdation.dist,
      Frequency * 2 * gdist.VIE * conversion.factor,

      # Round trip flying emissions (Kg)
      drive.VIE == 0,
      Frequency * Footprint.VIE + (2 * airport_dist_meters * conversion.factor)
    ),

    "Total Emissions SFO (Kg)" = fcase(
      # Round trip driving emissions for 4 trips (Kg)
      drive.SFO == 1 & gdist.SFO < accomdation.dist,
      Frequency * 2 * gdist.SFO * conversion.factor * meeting_days[[meeting_name]],

      # Round trip driving emissions for 1 trip (Kg)
      drive.SFO == 1 & gdist.SFO >=accomdation.dist,
      Frequency * 2 * gdist.SFO * conversion.factor,

      # Round trip flying emissions (Kg)
      drive.SFO == 0,
      Frequency * Footprint.SFO + (2 * airport_dist_meters * conversion.factor)
    ),

    "Total Emissions BOS (Kg)" = fcase(
      # Round trip driving emissions for 4 trips (Kg)
      drive.BOS == 1 & gdist.BOS < accomdation.dist,
      Frequency * 2 * gdist.BOS * conversion.factor * meeting_days[[meeting_name]],

      # Round trip driving emissions for 1 trip (Kg)
      drive.BOS == 1 & gdist.BOS >=accomdation.dist,
      Frequency * 2 * gdist.BOS * conversion.factor,

      # Round trip flying emissions (Kg)
      drive.BOS == 0,
      Frequency * Footprint.BOS + (2 * airport_dist_meters * conversion.factor)
    ))
  ]
}



# Post Processing ---------------------------------------------------------

# Print all unique country names
country.names <- sort(
  unique(toupper(
    c(
      YYZ$`Country Name`,
      SAN$`Country Name`,
      VIE$`Country Name`,
      SFO$`Country Name`,
      BOS$`Country Name`
      )
    ))
  )

# Normalize Country Name to Uppercase
for (meeting_name in conventions$IATA) {
  get(meeting_name)[, `Country Name` := toupper(`Country Name`)]

  # Convert 'USA' to 'UNITED STATES'
  get(meeting_name)[`Country Name` == "USA", `Country Name` := "UNITED STATES"]

  # Convert "UNITED KINGSOM", "UK", "ENGLAND", to "UNITED KINGDOM"
  get(meeting_name)[`Country Name` %in% c("UNITED KINGSOM", "UK", "ENGLAND"), `Country Name` := "UNITED KINGDOM"]

  # Convert "COLUMBIA" to "COLOMBIA"
  get(meeting_name)[`Country Name` == "COLUMBIA", `Country Name` := "COLOMBIA"]

  # Convert "PHILLIPINES" to "PHILIPPINES"
  get(meeting_name)[`Country Name` == "PHILLIPINES", `Country Name` := "PHILIPPINES"]
}

# Print all unique country names
country.names.normalized <- sort(
  unique(toupper(
    c(
      YYZ$`Country Name`,
      SAN$`Country Name`,
      VIE$`Country Name`,
      SFO$`Country Name`,
      BOS$`Country Name`
      )
    ))
  )



# State Normalization (TODO Later) ----------------------------------------
# Normalize States Names in the US to 2 letter abbreviations
unique.states <- sort(
  unique(toupper(
    c(
      YYZ[`Country Name` == "UNITED STATES"]$State,
      SAN[`Country Name` == "UNITED STATES"]$State,
      VIE[`Country Name` == "UNITED STATES"]$State,
      SFO[`Country Name` == "UNITED STATES"]$State,
      BOS[`Country Name` == "UNITED STATES"]$State
      )
    ))
  )

# for (meeting_name in conventions$IATA) {
#   get(meeting_name)[`Country Name` == "UNITED STATES" & toupper(State) %in% toupper(state.name)]
# }
#
# unique.states2 <- sort(
#   unique(toupper(
#     c(
#       YYZ[`Country Name` == "UNITED STATES"]$State2,
#       SAN[`Country Name` == "UNITED STATES"]$State2,
#       VIE[`Country Name` == "UNITED STATES"]$State2,
#       SFO[`Country Name` == "UNITED STATES"]$State2,
#       BOS[`Country Name` == "UNITED STATES"]$State2
#     )
#   ))
# )



# Export ------------------------------------------------------------------
final.sets <- list(
  "YYZ 2013" = YYZ,
  "SAN 2014" = SAN,
  "VIE 2015" = VIE,
  "SFO 2016" = SFO,
  "BOS 2017" = BOS,
  "Convention Centers" = conventions,
  "Summary Statistics" = summary.stats
)

saveRDS(final.sets, file = paste0(path.out, "ASRS Total Emissions.RDs"))

write.xlsx(
  final.sets,
  paste0(path.out, "ASRS Total Emissions.xlsx"),
  overwrite = TRUE
)
