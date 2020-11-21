# Code for building a map of Primula Q matrix in site locations, using 2 csvs and scatterpie
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
# raster, for getting elevation data as well as slope and aspect
library(raster)
# scatterpie, for plotting pie charts
library(scatterpie)
# ggnewscale, a way to trick ggplot2 into using multiple color scales in a single plot
library(ggnewscale)

# Cluster memberships----
# Read in a csv containing population names, latitudes, and longitudes
setwd("~/kaiser/Code/R/Mapping/")
pops <- read.csv("Coordinates.csv", header=T, sep=",")
keep <- c("PopName", "latitude", "longitude")
pops <- pops[keep]

# Reading in K7_subset file, an updated version of the CLUMPAK K7.output file
# This contains samples in their presented order, and has capillaris and one Troy Peak sample removed from file
k7 <- read.table("K7_plot.csv", header=T)

# For each group in the k7 file (which corresponds to a location on the map), calculate the mean for each cluster in that group
# We assign these means to vectors, with the vector names corresponding to the "name" of the cluster
PAR <- tapply(k7$P1, k7$Group, mean)
JAR <- tapply(k7$P2, k7$Group, mean)
SRP <- tapply(k7$P6, k7$Group, mean)
OWY <- tapply(k7$P4, k7$Group, mean)
DOM <- tapply(k7$P3, k7$Group, mean)
MAG <- tapply(k7$P5, k7$Group, mean)
NEV <- tapply(k7$P7, k7$Group, mean)

# Bind the vectors together to get a reduced Q matrix, with one row for each population
mean.Ks <- cbind(SRP,JAR,OWY,MAG,DOM,NEV,PAR)
# For some reason, the above code will locate domensis populations before maguirei, which is out of sync with the pops location matrix
# The line below reorders rows in this reduced Q matrix to match the order of rows in pops
mean.Ks <- mean.Ks[c((1:6),(10:13),(7:9),(14:17)),]
# For ease of presentation, populations are rounded to their nearest quarter (i.e., only display 0.25, 0.5, 0.75. 1.0)
mean.Ks[1:4,1] <- 1 ; mean.Ks[1:4,2:7] <- 0
mean.Ks[5,2] <- 1 ; mean.Ks[5,c(1,3:7)] <- 0
mean.Ks[6,3] <- 0.87 ; mean.Ks[6,2] <- 0.13 ; mean.Ks[6,c(1,4:7)] <- 0
mean.Ks[7:10,4] <- 1 ; mean.Ks[7:10,c(1:3,5:7)] <- 0
mean.Ks[11:13,5] <- 1 ; mean.Ks[11:13,c(1:4,6:7)] <- 0
mean.Ks[14,5] <- 0.77 ; mean.Ks[14,6] <- 0.09 ; mean.Ks[14,3] <- 0.14 ; mean.Ks[14,c(1:2,4,7)] <- 0
mean.Ks[15,6] <- 0.67 ; mean.Ks[15,2] <- 0.21 ; mean.Ks[15,5] <- 0.12 ; mean.Ks[15,c(1,3:4,7)] <- 0
mean.Ks[16:17,7] <- 1 ; mean.Ks[16:17,1:6] <- 0

pops

# Remove row names, for clarity
rownames(mean.Ks) <- NULL

# Bind the longitudes and latitudes with the Q matrix values. Add a column of radius values for the pie charts to be plotted (all 0.25)
pops <- cbind(pops, mean.Ks)
radius <- rep(0.3, 17)
pops <- cbind(pops, radius)

# Plotting----

# Baseline map of the Great Basin
greatBasin <- st_as_sf(map("state", regions = c("nevada", "utah", "idaho", "oregon"), exact = T, plot = FALSE, fill = TRUE))
# Create columns for centroids of each state (for labels to be added later)
greatBasin <- cbind(greatBasin, st_coordinates(st_centroid(greatBasin)))
greatBasin

# Elevations
# Load elevations using Rdata file
load("map.Rdata")
# Obtaining geographic data from GADM data set
US <- getData("GADM", country="USA", level=1)
# Subset by state name
greatBasinraster <- US[US$NAME_1 %in% c("Oregon", "Idaho", "Nevada", "Utah"),]
# Get elevations
elev <- getData("alt", country = "USA", mask = TRUE)
# Crop elevation dataset by greatBasinraster
elev <- crop(elev[[1]], greatBasinraster)
elev <- mask(elev, greatBasinraster)
# Convert raster to data.frame, for ggplot
elev_df <- raster::as.data.frame(elev, xy=TRUE)
elev_df <- elev_df[!is.na(elev_df$USA1_msk_alt),]
# Generate hillshade based on elev raster
slope <- terrain(elev, opt="slope")
aspect <- terrain(elev, opt="aspect")
hill <- hillShade(slope, aspect, 40, 270)
hill_df <- raster::as.data.frame(hill, xy=TRUE)
hill_df <- hill_df[!is.na(hill_df$layer),]

# First line plots baseline Great Basin map
ggplot(data = greatBasin) +
  theme_void() +
  geom_sf() +
  # Using geom_tile to include elevation data
  geom_tile(data = elev_df, aes(x = x, y = y, fill = USA1_msk_alt), alpha = 0.45, show.legend = FALSE) +
  # Use colors to paint elevation gradient
  scale_fill_gradient(low = "black", high = "white", name = "Elevation") +
  geom_sf(fill = NA) +
  # State labels
  geom_text(data = greatBasin, aes(X, Y, label = c("NV", "UT", "ID", "OR")), size = 3) +
  # Specify plotting window
  coord_sf(xlim = c(-126, -108), ylim = c(34.5, 49.5), expand = FALSE) +
  # Generate a new scale fill (for scatterpie; ggplot2 typically allows only one scale per plot)
  new_scale_fill() +
  # Scatterpie line, using the built pops matrix as data
  geom_scatterpie(data = pops,
                  aes(x=longitude, y=latitude, r = radius),
                  legend_name = "Clusters",
                  cols = c("SRP","JAR","OWY","MAG","DOM","NEV","PAR"),
                  alpha = 0.5) +
  scale_fill_manual(
    breaks = c("SRP","JAR","OWY","MAG","DOM","NEV","PAR"),
    labels = c("cusickiana_Idaho","cusickiana_Jarbidge","cusickiana_owyhee","maguirei","domensis","nevadensis_Troy", "parryi"),
    values = c("SRP" = "#8C510A",
               "JAR" = "#D95F02",
               "OWY" = "#E7298A",
               "MAG" = "#66A61E",
               "DOM" = "#7570B3",
               "NEV" = "#666666",
               "PAR" = "#2171B5"
    )
  )

# Example including hillshade
# First line plots baseline Great Basin map
ggplot(data = greatBasin) +
  theme_void() +
  geom_sf() +
  # Using geom_tile to include hillshade
  #geom_tile(data = hill_df, aes(x = x, y = y, fill = layer), alpha = 0.15, show.legend = FALSE) +
  # Use colors to paint hillshade
  #scale_fill_gradient(low = "white", high = "black", name = "Hillshade") +
  #geom_sf(fill = NA) +
  # Using geom_tile to include elevation data
  geom_tile(data = elev_df, aes(x = x, y = y, fill = USA1_msk_alt), alpha = 0.75, show.legend = FALSE) +
  # Use colors to paint elevation gradient
  scale_fill_gradient2(low = muted("green"), mid = "brown", high = "white", name = "Elevation", midpoint = 1552) +
  geom_sf(fill = NA) +
  # State labels
  geom_text(data = greatBasin, aes(X, Y, label = c("NV", "UT", "ID", "OR")), size = 3) +
  # Specify plotting window
  coord_sf(xlim = c(-126, -108), ylim = c(34.5, 49.5), expand = FALSE) +
  # Generate a new scale fill (for scatterpie; ggplot2 typically allows only one scale per plot)
  new_scale_fill() +
  # Scatterpie line, using the built pops matrix as data
  geom_scatterpie(data = pops,
                  aes(x=longitude, y=latitude, r = radius),
                  legend_name = "Clusters",
                  cols = c("SRP","JAR","OWY","MAG","DOM","NEV","PAR"),
                  alpha = 0.5) +
  scale_fill_manual(
    breaks = c("SRP","JAR","OWY","MAG","DOM","NEV","PAR"),
    labels = c("cusickiana_Idaho","cusickiana_Jarbidge","cusickiana_owyhee","maguirei","domensis","nevadensis_Troy", "parryi"),
    values = c("SRP" = "#8C510A",
               "JAR" = "#D95F02",
               "OWY" = "#E7298A",
               "MAG" = "#66A61E",
               "DOM" = "#7570B3",
               "NEV" = "#666666",
               "PAR" = "#2171B5"
    )
  )

# # First line plots baseline Great Basin map
# ggplot(data = greatBasin) +
#   geom_sf() +
#   # State labels
#   geom_text(data = greatBasin, aes(X, Y, label = c("NV", "UT", "ID", "OR")), size = 3) +
#   # Specify plotting window
#   coord_sf(xlim = c(-126, -108), ylim = c(34.5, 49.5), expand = FALSE) + 
#   # Scatterpie line, using the built pops matrix as data
#   geom_scatterpie(data = pops, 
#                   aes(x=longitude, y=latitude, r = radius),
#                   legend_name = "Clusters",
#                   cols = c("SRP","JAR","OWY","MAG","DOM","NEV","PAR"), 
#                   alpha = 0.5) +
#   scale_fill_manual(
#     breaks = c("SRP","JAR","OWY","MAG","DOM","NEV","PAR"),
#     labels = c("cusickiana_Idaho","cusickiana_Jarbidge","cusickiana_owyhee","maguirei","domensis","nevadensis_Troy", "parryi"),
#     values = c("SRP" = "#8C510A",
#                "JAR" = "#D95F02",
#                "OWY" = "#E7298A",
#                "MAG" = "#66A61E",
#                "DOM" = "#7570B3",
#                "NEV" = "#666666",
#                "PAR" = "#2171B5"
#     ) 
# )

# ggmap----
library(ggmap)
library(gridExtra)

myLocation <- c(-126, 34.5, -108, 49.5)
myMap <- get_map(location=myLocation,
                 source="google", maptype = "satellite", force = TRUE)
# No matter source or maptype, always uses stamen and terrain...
ggmap(myMap) + theme_void() +
  geom_text(data = greatBasin, aes(X, Y, label = c("NV", "UT", "ID", "OR")), size = 3) +
  # Specify plotting window
  coord_sf(xlim = c(-126, -108), ylim = c(34.5, 49.5), expand = FALSE) + 
  # Scatterpie line, using the built pops matrix as data
  geom_scatterpie(data = pops, 
                  aes(x=longitude, y=latitude, r = radius),
                  legend_name = "Clusters",
                  cols = c("SRP","JAR","OWY","MAG","DOM","NEV","PAR"), 
                  alpha = 0.5) +
  scale_fill_manual(
    breaks = c("SRP","JAR","OWY","MAG","DOM","NEV","PAR"),
    labels = c("cusickiana_Idaho","cusickiana_Jarbidge","cusickiana_owyhee","maguirei","domensis","nevadensis_Troy", "parryi"),
    values = c("SRP" = "#8C510A",
               "JAR" = "#D95F02",
               "OWY" = "#E7298A",
               "MAG" = "#66A61E",
               "DOM" = "#7570B3",
               "NEV" = "#666666",
               "PAR" = "#2171B5"
    ) 
  )

# Lab demo----
myLocation <- c(-126, 34.5, -108, 49.5)
myMap <- get_map(location=myLocation,
                 source="stamen", maptype = "terrain")
# No matter source or maptype, always uses stamen and terrain...
ggmap(myMap) + theme_void() + coord_sf(xlim = c(-126, -108), ylim = c(34.5, 49.5), expand = FALSE)
