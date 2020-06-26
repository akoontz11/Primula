# ggplot and sf example----
install.packages(c("cowplot","googleway","ggplot2","ggrepel","ggspatial","sf","rnaturalearth","rnaturalearthdata"))

library(cowplot)
library(googleway)
library(ggplot2)
library(ggrepel)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(tools)

# Pull country data, set scale, and specify the sf return class (rather than sp)
world <- ne_countries(scale = "medium", returnclass = "sf")
# Test
class(world)
# Create plot and add geometry stored in an sf object
ggplot(data = world) +
  geom_sf()
# Add titles, axis labels
ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))
# With green polygons
ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen")
# With colors according to population
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")
# Using a euro-centric projection, rather than the default EGS84. The coord_sf call can handle projections and map extent
ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")
# Using coord_sf to specify limits to map extent, allowing us to "zoom" in. expand argument is used to alter positions of axes
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)

# %%% EXAMPLE WITH STATES %%%
# Define sites using geom_point command
sites <- data.frame(longitude = c(-80.144005, -80.109), latitude = c(26.479005, 26.83))
ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)
# Define sites using geom_sf, which allows the map and the sites to be in two different projections
sites <- st_as_sf(sites, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
## Simple feature collection with 2 features and 0 fields
## geometry type:  POINT
## dimension:      XY
## bbox:           xmin: -80.14401 ymin: 26.479 xmax: -80.109 ymax: 26.83
## epsg (SRID):    4326
## proj4string:    +proj=longlat +datum=WGS84 +no_defs
##                     geometry
## 1 POINT (-80.14401 26.47901)
## 2      POINT (-80.109 26.83)
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = sites, size = 4, shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

# Load states object, which contains US state data, from maps package
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)
# Add centroids and coordinates to states object
states <- cbind(states, st_coordinates(st_centroid(states)))
# Capitalize state names
states$ID <- toTitleCase(as.character(states$ID))

# Plot Florida map with state boundaries and labels. geom_label function places name behind rectangular bar, for easing reading
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_label(data = states, aes(X, Y, label = ID), size = 5, fontface = "bold", 
             nudge_y = states$nudge_y) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

# Will's ggplot example----
# Load some packages (maybe I don't need all of them?)
require(choroplethr)
require(choroplethrMaps)
library(ggplot2)
library("ggspatial")
library("rnaturalearth")
library("rnaturalearthdata")
library(sf)
library(rgeos)
# Load some nice built-in maps
data(state.regions)
data(state.map)
# Make a plot of some data (in a data.frame called 'data'(
ggplot(data = state) + geom_sf(fill= "antiquewhite") + 
  geom_point(data=data, aes(x=long, y=lat), col="#ff9999", size=.75)
# Error: You're passing a function as global data. Have you misspelled the `data` argument in `ggplot()`