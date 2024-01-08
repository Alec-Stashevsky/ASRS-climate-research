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
distance.log.YYZ <- rep(NA, nrow(coordinate.sample))
distance.log.SAN <- rep(NA, nrow(coordinate.sample))
distance.log.VIE <- rep(NA, nrow(coordinate.sample))
distance.log.SFO <- rep(NA, nrow(coordinate.sample))
distance.log.BOS <- rep(NA, nrow(coordinate.sample))

for (i in 1:nrow(coordinate.sample)) {

  distance.log.YYZ[i] <- sum(

    # Weight distance by frequency
    YYZ$Frequency * distGeo(
      YYZ[, .(lon, lat)],
      c(coordinate.sample$lon[i], coordinate.sample$lat[i])
    )
  )

  distance.log.SAN[i] <- sum(

    # Weight distance by frequency
    SAN$Frequency * distGeo(
      SAN[, .(lon, lat)],
      c(coordinate.sample$lon[i], coordinate.sample$lat[i])
    )
  )

  distance.log.VIE[i] <- sum(

    # Weight distance by frequency
    VIE$Frequency * distGeo(
      VIE[, .(lon, lat)],
      c(coordinate.sample$lon[i], coordinate.sample$lat[i])
    )
  )

  distance.log.SFO[i] <- sum(

    # Weight distance by frequency
    SFO$Frequency * distGeo(
      SFO[, .(lon, lat)],
      c(coordinate.sample$lon[i], coordinate.sample$lat[i])
    )
  )

  distance.log.BOS[i] <- sum(

    # Weight distance by frequency
    BOS$Frequency * distGeo(
      BOS[, .(lon, lat)],
      c(coordinate.sample$lon[i], coordinate.sample$lat[i])
    )
  )
}

# Sort by smallest aggregate distance
coordinate.rank.YYZ <- cbind(
  arrange(
    cbind(
      coordinate.sample,
      "Aggregate Distance" = distance.log.YYZ),
    distance.log.YYZ
  ),
  "Rank" = 1:nrow(coordinate.sample)
)

coordinate.rank.SAN <- cbind(
  arrange(
    cbind(
      coordinate.sample,
      "Aggregate Distance" = distance.log.SAN),
    distance.log.SAN
  ),
  "Rank" = 1:nrow(coordinate.sample)
)

coordinate.rank.VIE <- cbind(
  arrange(
    cbind(
      coordinate.sample,
      "Aggregate Distance" = distance.log.VIE),
    distance.log.VIE
  ),
  "Rank" = 1:nrow(coordinate.sample)
)

coordinate.rank.SFO <- cbind(
  arrange(
    cbind(
      coordinate.sample,
      "Aggregate Distance" = distance.log.SFO),
    distance.log.SFO
  ),
  "Rank" = 1:nrow(coordinate.sample)
)

coordinate.rank.BOS <- cbind(
  arrange(
    cbind(
      coordinate.sample,
      "Aggregate Distance" = distance.log.BOS),
    distance.log.BOS
  ),
  "Rank" = 1:nrow(coordinate.sample)
)

coordinate.rank <- list(
  "YYZ" = coordinate.rank.YYZ,
  "SAN" = coordinate.rank.SAN,
  "VIE" = coordinate.rank.VIE,
  "SFO" = coordinate.rank.SFO,
  "BOS" = coordinate.rank.BOS
)


# Create grid from coordinates with the smallest Aggregate Distance
grid.YYZ <- as.matrix(rbind(
  c(min(coordinate.rank.YYZ$lon[1:30]),
    min(coordinate.rank.YYZ$lat[1:30])),
  c(max(coordinate.rank.YYZ$lon[1:30]),
    max(coordinate.rank.YYZ$lat[1:30])))
  )

grid.SAN <- as.matrix(rbind(
  c(min(coordinate.rank.SAN$lon[1:30]),
    min(coordinate.rank.SAN$lat[1:30])),
  c(max(coordinate.rank.SAN$lon[1:30]),
    max(coordinate.rank.SAN$lat[1:30])))
)

grid.VIE <- as.matrix(rbind(
  c(min(coordinate.rank.VIE$lon[1:30]),
    min(coordinate.rank.VIE$lat[1:30])),
  c(max(coordinate.rank.VIE$lon[1:30]),
    max(coordinate.rank.VIE$lat[1:30])))
)

grid.SFO <- as.matrix(rbind(
  c(min(coordinate.rank.SFO$lon[1:30]),
    min(coordinate.rank.SFO$lat[1:30])),
  c(max(coordinate.rank.SFO$lon[1:30]),
    max(coordinate.rank.SFO$lat[1:30])))
)

grid.BOS <- as.matrix(rbind(
  c(min(coordinate.rank.BOS$lon[1:30]),
    min(coordinate.rank.BOS$lat[1:30])),
  c(max(coordinate.rank.BOS$lon[1:30]),
    max(coordinate.rank.BOS$lat[1:30])))
)

grid.list <- list(
  "YYZ" = grid.YYZ,
  "SAN" = grid.SAN,
  "VIE" = grid.VIE,
  "SFO" = grid.SFO,
  "BOS" = grid.BOS
)


# Plot Region of Optimal Location -----------------------------------------
pdf(
  file = paste0(path.viz, "ASRS Facility Location Results.pdf"),
  onefile = TRUE,
  width = 6,
  height = 8
)

title.list <- list(
  "YYZ" = "YYZ 2013",
  "SAN" = "SAN 2014",
  "VIE" = "VIE 2015",
  "SFO" = "SFO 2016",
  "BOS" = "BOS 2017"
)


for (meeting_name in names(coordinate.rank)) {
  # Plot 15 best sample coordinates to create region of confidence

  if (meeting_name == "YYZ") {
    map("state", col = "grey20", fill = TRUE, bg = "black", lwd = 0.1,
        xlim = c(grid.list[[meeting_name]][1,1] - 1, grid.list[[meeting_name]][2,1] + 1),
        ylim = c(grid.list[[meeting_name]][1,2] - 1, grid.list[[meeting_name]][2,2] + 1)
    )
  } else {
    map("state", col = "grey20", fill = TRUE, bg = "black", lwd = 0.1,
        xlim = c(grid.list[[meeting_name]][1,1] - 3, grid.list[[meeting_name]][2,1] + 5),
        ylim = c(grid.list[[meeting_name]][1,2] - 3, grid.list[[meeting_name]][2,2] + 5)
    )
  }

  points(
    x = coordinate.rank[[meeting_name]]$lon[1:3],
    y = coordinate.rank[[meeting_name]]$lat[1:3],
    col = "orange red",
    pch = 10, cex = 1
  )

  pos <- 1
  offset <- 0.35
  adj <- c(1, 1)
  if (meeting_name == "YYZ") {
    pos <- 4
    offset <- 0
  }

  text(
    x = coordinate.rank[[meeting_name]]$lon[1:3]+ 0.15,
    y = coordinate.rank[[meeting_name]]$lat[1:3],
    labels = coordinate.rank[[meeting_name]]$Rank[1:3],
    col = "white",
    cex = 0.65,
    pos = pos,
    offset = offset,
    adj = adj
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

  # Plot Columbus, OH
  points(
    x = -82.99666396486687,
    y = 39.96506279032695,
    col = "white",
    pch = 19,
    cex = .4
  )

  text(
    x = -82.99666396486687,
    y = 39.96506279032695,
    labels = "Columbus",
    col = "white",
    cex = 0.65,
    pos = 1,
    offset = .35
  )


  # Don't plot if meeting is SFO or SAN
  if (!(meeting_name %in% c("SFO", "SAN"))) {
    # Plot Pittsburgh, PA
    points(
      x = -79.99765558139796,
      y = 40.443603413134596,
      col = "white",
      pch = 19,
      cex = .4
    )

    text(
      x = -79.99765558139796,
      y = 40.443603413134596,
      labels = "Pittsburgh",
      col = "white",
      cex = 0.65,
      pos = 1,
      offset = .35
    )
  }

  if (meeting_name != "YYZ") {
    # Plot Philadelphia, PA
    points(
      x = -75.16902803872257,
      y = 39.95324523510575,
      col = "white",
      pch = 19,
      cex = .4
    )

    text(
      x = -75.16902803872257,
      y = 39.95324523510575,
      labels = "Philadelphia",
      col = "white",
      cex = 0.65,
      pos = 1,
      offset = .35
    )

    # Plot New York, NY
    points(
      x = -74.00589107697768,
      y = 40.716662492577136,
      col = "white",
      pch = 19,
      cex = .4
    )

    text(
      x = -74.00589107697768,
      y = 40.716662492577136,
      labels = "New York City",
      col = "white",
      cex = 0.65,
      pos = 1,
      offset = .35
    )
  }

  # Add plot aesthetics
  title(main = paste0("ASRS Meeting Optimal Locations ", title.list[[meeting_name]]), col.main = "white")
}


# Plot Polygon ------------------------------------------------------------


# Isolate maximum bounding polygon with best 15 approximations
# target <- c(10, 5, 8, 9, 6)
#
# poly.ordering <- as.matrix(
#   rbind(
#     coordinate.rank[1],
#     coordinate.rank[2],
#     coordinate.rank[3],
#     coordinate.rank[4],
#     coordinate.rank[5]
#   )
# )

# Identify the centroid/geometric median/center of mass
# region <- makePoly(poly.ordering[,1:2], sp = TRUE)
# center <- centroid(region)

# Draw polygon
# polygon(
#   poly.ordering,
#   col = rgb(red = 1, green = 140/255, blue = 0, alpha = 0.25),
#   border = "orange"
# )

# Add centroid
# points(x = center[1], y= center[2], col = "gold", pch = 3)


# Export ------------------------------------------------------------------
dev.off()
