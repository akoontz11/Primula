# Code for building a map of Primula Q matrix in site locations, using 2 csvs and scatterpie
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(scatterpie)

# Read in a csv containing population names, latitudes, and longitudes
setwd("~/kaiser/Primula/code/Map/")
pops <- read.csv("Coordinates.csv", header=T, sep=",")
keep <- c("PopName", "latitude", "longitude")
pops <- pops[keep]

# Reading in K7_subset file, an updated version of the CLUMPAK K7.output file
# This contains samples in their presented order, and has capillaris and one Troy Peak sample removed from file
setwd("~/kaiser/Primula/code/structure/JuneSubset//")
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
# For some reason, the above code will locate domensis populations before maguirei, which is out oy sync with the pops location matrix
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

# First line plots baseline Great Basin map
ggplot(data = greatBasin) +
  geom_sf() +
  # State labels
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

# DEM
library(dplyr)
library(raster)
library(rgdal)
library(maptools)
library(beepr)

# Load in DEM files and convert into rasters
setwd("~/kaiser/Primula/code/Map/DEM/dem90_hf/dem90_hf/")
files <- list.files(recursive=TRUE)
files <- files[-c(2,4)]
rasters.list <- sapply(files, raster)

# Run the mosaic function on the raster list
names(rasters.list) <- NULL
rasters.list$fun <- mean
mosaic <- do.call(mosaic, rasters.list)

#create a plot
plot(mosaic, col = gray.colors(20, start = 0, end = 1))
