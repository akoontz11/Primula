# %%% CORRELATING GENETIC AND GEOGRAPHIC DISTANCES %%%
library(adegenet)
library(geosphere) # For distGeo function
setwd("/home/akoontz11/kaiser/Code/ipyrad/JuneSubset/Outfiles")

# Read in JuneSubset STRUCTURE file, to create a genind object
# 82 individuals, 1277 loci, with genotype labels. Genotypes NOT coded by a single row
junesub.genind <- import2genind("JuneSubset.str")
str(junesub.genind)

# Read in a csv containing population names, longitudes (x), and latitudes (y)
setwd("~/kaiser/Code/R/Spatial/")
pops <- read.csv("Coordinates_PopNames.csv", header=T, sep=",")
pops

# To calculate genetic and geographic distances using adegenet, we need a genpop object
# So, we add the relevant information to our genind object, and then convert that to genpop
# Add pops to genind object
junesub.genind@pop <- pops$PopName
# Add a matrix of coordinates as part of the genind object, in the 'other' slot
junesub.genind@other$xy <- as.matrix(pops[,-1])
rownames(junesub.genind@other$xy) <- pops$PopName
# Convert gendind to genpop object, using adegenet function
junesub.genpop <- genind2genpop(junesub.genind)

# Now, calculate the genetic and genographic distances of our populations
# Genetic distance matrix (Euclidean, Roger's)
Dgen <- dist.genpop(junesub.genpop,method=4)

# For geographic distance matrix, first get list of unique longitudes/latitudes
locations <- unique(junesub.genind@other$xy)
# Make a dummy geographic matrix to accept values
Dgeo <- matrix(NA, nrow = nrow(locations), ncol = nrow(locations))
rownames(Dgeo) <- colnames(Dgeo) <- unique(rownames(junesub.genind@other$xy))
# For each row of the matrix,
for(i in 1:nrow(Dgeo)){
  # and for each column in the matrix,
  # (this is repetitive, but also this entire technique is fairly amateur...)
  for(j in 1:ncol(Dgeo)){
    # calculate geographic distance between points based on the locations list
    Dgeo[i,j] <- distGeo(p1 = locations[i,], p2 = locations[j,])
  }
}
# distGeo reports values in meters; convert that to kilometers. 
# Also, only return the lower half of the matrix, since that's what we'll use
Dgeo <- Dgeo/1000

# Plotting
plot(c(Dgen) ~ Dgeo[lower.tri(Dgeo)], 
     xlab = "Geographic distance (km)", ylab = "Genetic distance (Euclidean)",
     main = "Genetic by Geographic distance")

abline(reg=lm(c(Dgen) ~ Dgeo[lower.tri(Dgeo)]), col="red", lty=1, lwd=2)

