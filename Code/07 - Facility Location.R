# Step 07 - Facility Location
# Alec Stashevsky
# January 6, 2024


# Setup -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(data.table)
library(geosphere)
library(maps)
library(sp)

set.seed(32) # Needed to keep polygon consistent
path.in <- "~/Git/ASRS-climate-research/Output/ASRS EMISSIONS.xlsx"
path.out <- "~/Git/ASRS-climate-research/Output/"
path.viz <- "~/Git/ASRS-climate-research/Visualizations/"


# Import ------------------------------------------------------------------
YYZ <- read_excel(path.in, sheet = "YYZ 2013") %>% setDT()
SAN <- read_excel(path.in, sheet = "SAN 2014") %>% setDT()
VIE <- read_excel(path.in, sheet = "VIE 2015") %>% setDT()
SFO <- read_excel(path.in, sheet = "SFO 2016") %>% setDT()
BOS <- read_excel(path.in, sheet = "BOS 2017") %>% setDT()

# Facility Location Algorithm ---------------------------------------------

# Sample 50,000 coordinates for whole-earth grid, can be increased to be more precise
coordinate.sample <- as.data.table(
  cbind(
    lon = round(runif(50000, -180, 180), 8),
    lat = round(runif(50000, -90, 90), 8)
  )
)

# Calculate total geodesic distance for each coordinate in sample
distance.log <- rep(NA, nrow(coordinate.sample))

for (i in 1:nrow(coordinate.sample)) {

  distance.log[i] <- sum(

    # Weight distance by frequency
    YYZ$Frequency * distGeo(
      YYZ[, .(lon, lat)],
      c(coordinate.sample$lon[i], coordinate.sample$lat[i])
    )
  )
}

# Sort by smallest aggregate distance
coordinate.rank <- cbind(
  arrange(
    cbind(
      coordinate.sample,
      "Aggregate Distance" = distance.log),
    distance.log
  ),
  "Rank" = 1:nrow(coordinate.sample)
)

# Create grid from 10 coordinates with the smallest Aggregate Distance
grid <- as.matrix(rbind(
  c(min(coordinate.rank$lon[1:30]),
    min(coordinate.rank$lat[1:30])),
  c(max(coordinate.rank$lon[1:30]),
    max(coordinate.rank$lat[1:30]))))


# Plot Region of Optimal Location -----------------------------------------
pdf(
  file = paste0(path.viz, "ASRS Facility Location Results.pdf"),
  onefile = TRUE,
  width = 6,
  height = 8
)

# Plot 15 best sample coordinates to create region of confidence
map("state", col = "grey20", fill = TRUE, bg = "black", lwd = 0.1
    ,
    xlim = c(grid[1,1] - 3, grid[2,1] + 4),
    ylim = c(grid[1,2] - 3, grid[2,2] + 4)
)

# Isolate maximum bounding polygon with best 15 approximations
target <- c(10, 5, 8, 9, 6)

poly.ordering <- as.matrix(
  rbind(
    coordinate.rank[1],
    coordinate.rank[2],
    coordinate.rank[3],
    coordinate.rank[4],
    coordinate.rank[5]
  )
)

# Identify the centroid/geometric median/center of mass
region <- makePoly(poly.ordering[,1:2], sp = TRUE)
center <- centroid(region)

# Draw polygon
polygon(
  poly.ordering,
  col = rgb(red = 1, green = 140/255, blue = 0, alpha = 0.25),
  border = "orange"
)

# Add centroid
# points(x = center[1], y= center[2], col = "gold", pch = 3)

points(
  x = coordinate.rank$lon[1:3],
  y = coordinate.rank$lat[1:3],
  col = "orange red",
  pch = 10, cex = 1
)

text(
  x = coordinate.rank$lon[1:3]+ 0.15,
  y = coordinate.rank$lat[1:3],
  labels = coordinate.rank$Rank[1:3],
  col = "white",
  cex = 0.65,
  pos = 1,
  offset = 0.35,
  adj = c(1, 1)
)


# Plot Chicago
points(
  x = -87.6298,
  y = 41.8781,
  col = "white",
  pch = 19,
  cex = .4
)

text(
  x = -87.6298,
  y = 41.8781,
  labels = "Chicago",
  col = "white",
  cex = 0.65,
  pos = 3,
  offset = .35
)

# Add plot aesthetics
title(main = "ASRS Meeting Optimal Location Region", col.main = "white")


# Export ------------------------------------------------------------------
dev.off()
