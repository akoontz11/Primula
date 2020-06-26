library(fields)
library(mapproj)
setwd("~/kaiser/Primula/code/Map/")

# Data.frame for populations
pops <- read.csv("Coordinates.csv", header=T, sep=",")
pops
keep <- c("PopName", "latitude", "longitude")
pops <- pops[keep]
pops

sites <- pops[,-1]
sites

# Standard maps command and points----
# Starter map
greatBasin <- c("nevada", "utah", "idaho", "oregon")

# Original code, with smaller map...
# my.map <- map('state', regions=greatBasin, fill = F, interior=T, col = 'white', bg = 'lightgrey', border = 'white',
#               ylim = c(35,49), xlim = c(-130,-100), mar = c(0.05, 0.05, 0.05, 0.05))

my.map <- map('county', regions=fplist, fill = T, interior=T, col = 'white', 
              bg = 'lightgrey', border = 'white', ylim = c(35,49), xlim = c(-130,-100), myborder=0, lforce = "e")

proj <- mapproject(sites$Lat, sites$Long, projection = "rectangular", parameters =  40)

my.map <- map('state', regions=greatBasin, fill = T, interior=T, col = 'white', 
              bg = 'lightgrey', border = 'white', ylim = c(35,49), xlim = c(-130,-100), myborder=0, lforce = "e")
my.map <- map('state', regions=greatBasin, fill = T, interior=T, col = 'white', 
              bg = 'lightgrey', border = 'white', ylim = c(15,74), xlim = c(-155,-75), myborder=0, lforce = "e")

# How to enlarge this map further...?

map("state",proj='bonne', param=45, lforce = "e")
data(state)
text(mapproject(state.center), state.abb)

# Plot populations, with colors derived from STRUCTURE plot. #FF59AC is used for populations that are highly hybridized...
# cusickiana--SRP
points(pops[1:4,3], pops[1:4,2], col = "#A8FFFD", bg = "#A8FFFD", cex = 0.8, pch = 21)
# cusickiana--Jarbidge
points(pops[5,3], pops[5,2], col = "#26CDCD", bg = "#26CDCD", cex = 0.8, pch = 21)
# cusickiana--Owyhee
points(pops[6,3], pops[6,2], col = "#FFFF00", bg = "#FFFF00", cex = 0.8, pch = 21)
# maguirei
points(pops[7:10,3], pops[7:10,2], col = "#FF59AC", bg = "#FF59AC", cex = 0.8, pch = 21)
# domensis
points(pops[11:13,3], pops[11:13,2], col = "#B862D3", bg = "#B862D3", cex = 0.8, pch = 21)
# nevadensis--GRBA
points(pops[14,3], pops[14,2], col = "#FFFF00", bg = "#FFFF00", cex = 0.8, pch = 21)
# nevadensis--Troy
points(pops[15,3], pops[15,2], col = "#FFFF00", bg = "#FFFF00", cex = 0.8, pch = 21)
# parryi
points(pops[17:18,3], pops[17:18,2], col = "#A39D9D", bg = "#A39D9D", cex = 0.8, pch = 21)

# ggplot2 and sf library example----
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
# Library for plotting in the U.S.
library("usmap")
theme_set(theme_bw())
library("maps")
library(scatterpie)

greatBasin <- st_as_sf(map("state", regions = c("nevada", "utah", "idaho", "oregon"), exact = T, plot = FALSE, fill = TRUE))
greatBasin

ggplot(data = greatBasin) +
  geom_sf() +
  geom_point(data = pops, aes(x = longitude, y = latitude), size = 4, shape = 16, colour = "darkred") +
  coord_sf(xlim = c(-126, -108), ylim = c(34, 49), expand = FALSE)

myColors <- c('#A8FFFD','#B862D3', '#A39D9D','#FFFF00', '#FF59AC', '#26CDCD')
names(myColors) <- levels(unique(pops$PopName))
myColors
colScale <- scale_color_manual(name = "PopName", values = myColors)

testColors <-c(rep('#A8FFFD',4),'#26CDCD','#FFFF00',rep('#FF59AC', 4),rep('#B862D3',3),rep('#FFFF00',2),rep('#A39D9D',3))

ggplot(data = greatBasin) +
  geom_sf() +
  geom_point(data = pops, aes(x = longitude, y = latitude, colour = PopName), size = 4, shape = 16) +
  coord_sf(xlim = c(-126, -108), ylim = c(34, 49), expand = FALSE)

#Create a custom color scale
myColors
names(myColors) <- levels(dat$grp)
colScale <- scale_colour_manual(name = "grp",values = myColors)
str(colScale)
