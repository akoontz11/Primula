# %%% CODE FOR CORRELATING GENETIC AND GEOGRAPHIC DISTANCES %%%
library(adegenet)
library(geosphere) # For calculations of Vincenty distance (see distVincentyEllipsoid)
setwd("/home/akoontz11/kaiser/Code/ipyrad/JuneSubset/Outfiles")

# Read in JuneSubset STRUCTURE file, to create a genind object
# 82 individuals, 1277 loci, with genotype labels. Genotypes NOT coded by a single row
junesub.genind <- import2genind("JuneSubset.str")
str(junesub.genind)

# Read in a csv containing population names, longitudes (x), and latitudes (y)
setwd("~/kaiser/Code/R/Mantel/")
pops <- read.csv("Coordinates_PopNames.csv", header=T, sep=",")
pops

# To calculate genetic and geographic distances using adegenet, we need a genpop object
# So, we add the relevant information to our genind object, and then convert that to genpop
# Add pops to genind object
junesub.genind@pop <- pops$PopName
# Add a matrix of coordinates as part of the genind object, in the 'other' slot
junesub.genind@other$xy <- as.matrix(pops[,-1])
rownames(junesub.genind@other$xy) <- pops$PopName
junesub.genind@other$xy
# Convert gendind to genpop object, using adegenet function
junesub.genpop <- genind2genpop(junesub.genind)

# Now, calculate the genetic and genographic distances of our populations
# Each of these functions returns a distance matrix
# Genetic matrix (Euclidean, Roger's)
Dgen <- dist.genpop(junesub.genpop,method=2)
# Distance matrix
Dgeo <- dist(junesub.genind@other$xy)

length(c(Dgen))
length(c(Dgeo))
length(c(unique(Dgeo)))

plot(c(Dgen) ~ c(unique(Dgeo)))

# Using package geosphere to calculate appropriate geographic distances
distVincentyEllipsoid(p1=junesub.genind@other$xy)

distGeo(p1=junesub.genind@other$xy)
