library(adegenet)
library(ade4)
library(hierfstat)
library(vcfR)
library(scales)
setwd("/home/akoontz11/kaiser/Primula/ipyrad_UniqueReps/UniqueReps_outfiles/")

# %%% REFORMATTING VCF OBJECT AND REMOVING LOW SNP SAMPLES %%% ----
# Read in vcf object5
vcf.Object <- read.vcfR("UniqueReps.vcf")
# VCF objects include meta-information, fixed fields, and genotype data. We want the latter.
str(vcf.Object@gt)
# We extract the genotype information, and remove the first column 
# (FORMAT describes the format of all the gt information, which in this example is entirely GT:DP:CATG)
# We also transpose the matrix so that samples are rows, and genotype data are columns
genMat.vcf <- t(vcf.Object@gt[,-1])
# Remove DP:CATG data, to only include genotype data (or allele ref.)
genMat.vcf <- gsub(pattern=":[0-9]{1,4}:[0-9]{1,4},[0-9]{1,4},[0-9]{1,4},[0-9]{1,4}", replacement="", genMat.vcf)
# Removing samples with low (generally <1,000) SNPs. These are: Ck95, Nv123, Pa150, Pa152, and Pa156
genMat.vcf <- genMat.vcf[-c(36,77,92,93,95),]

# %%% MERGING REPLICATES %%% ----
# Find replicate rows
grep(pattern="\\_R", x=names(genMat.vcf[,1]))

# Calling out number of mismatches, and replacing mismatched SNPs with "./." in original samples (not replicate)
# Ck54_R
all.equal(genMat.vcf[24,],genMat.vcf[25,]) # 921
genMat.vcf[24,] <- replace(x=genMat.vcf[24,], list=which(genMat.vcf[24,] != genMat.vcf[25,]), values="./.")
# Ck85_R
all.equal(genMat.vcf[31,],genMat.vcf[32,]) # 677
genMat.vcf[31,] <- replace(x=genMat.vcf[31,], list=which(genMat.vcf[31,] != genMat.vcf[32,]), values="./.")
# Dm112_R
all.equal(genMat.vcf[45,],genMat.vcf[46,]) # 787
genMat.vcf[45,] <- replace(x=genMat.vcf[45,], list=which(genMat.vcf[45,] != genMat.vcf[46,]), values="./.")
# Mg40_R
all.equal(genMat.vcf[57,],genMat.vcf[58,]) # 760
genMat.vcf[57,] <- replace(x=genMat.vcf[57,], list=which(genMat.vcf[57,] != genMat.vcf[58,]), values="./.")
# Mg64_R
all.equal(genMat.vcf[65,],genMat.vcf[66,]) # 1059
genMat.vcf[65,] <- replace(x=genMat.vcf[65,], list=which(genMat.vcf[65,] != genMat.vcf[66,]), values="./.")
# Nv134_R
all.equal(genMat.vcf[80,],genMat.vcf[81,]) # 628
genMat.vcf[80,] <- replace(x=genMat.vcf[80,], list=which(genMat.vcf[80,] != genMat.vcf[81,]), values="./.")
# Pa147_R
all.equal(genMat.vcf[88,],genMat.vcf[89,]) # 508
genMat.vcf[88,] <- replace(x=genMat.vcf[88,], list=which(genMat.vcf[88,] != genMat.vcf[89,]), values="./.")
 
# Remove all replicate rows. Now, there are 84 samples (rows)
genMat.vcf <- genMat.vcf[-c(25,32,46,58,66,81,89),]

# %%% CREATE UPDATED VCF FILE %%% ----
# To check if removal/merging of samples leads to loss of informative SNPs, capture unique values of each column
SNP.values <- apply(genMat.vcf, 2, function(x) unique(x))
# Now, captures the uniques of this list
unique(SNP.values)
# Since none of these are just "./.", it seems all 8,035 SNPs are still informative

# Transpose matrix back, such that SNPs are rows and samples are columns
genMat.vcf <- t(genMat.vcf)
# Add FORMAT column back in, specifying new GT:DP format
genMat.vcf <- cbind(rep("GT:DP", times=nrow(genMat.vcf)), genMat.vcf)
colnames(genMat.vcf)[[1]] <- "FORMAT"
# Create a new vcf object with updated genetic matrix data
new.vcf.Object <- vcf.Object
new.vcf.Object@gt <- genMat.vcf
str(new.vcf.Object)
# Write object to new zipped vcf file
write.vcf(new.vcf.Object, file="Subset_UniqueReps.vcf.gz")


# %%% DISCRIMINANT ANALYSIS OF PRINCIPAL COMPONENTS (DAPC) %%% ----
# Convert vcf to gening=d object
genind.vcf.obj <- df2genind(genMat.vcf, sep = "/")
# Warning: In df2genind(vcf.Object, sep = "/"): character '.' detected in names of loci; replacing with '_'
unique(nAll(genind.vcf.obj)) # nAll returns the number of alleles for each locus
nInd(genind.vcf.obj)

# Find number of clusters
grp <- find.clusters(genind.vcf.obj, max.n.clust=60)
# Number of retained PCs (here, there's no cost to retaining a lot of PCs): 120
# Number of clusters: 6 (has the lowest BIC, 623.86)
grp$stat

# Conduct DAPC
dapc1 <- dapc(x=genind.vcf.obj, pop=grp$grp)
# Number of retained PCs: 60 (although it seems like this value could go higher, even past 80)
# Number of discriminant functions to retain: 5

# Make a scatterplot of initial DAPC
scatter(dapc1)

# Attempt at STRUCTURE plot
compoplot(dapc1, posi="bottomright", cleg=0.6, txt.leg=paste("Cluster", 1:6), lab="", ncol=1, xlab="individuals")

# %%% READ IN STRUCTURE OBJECT %%%----
structure.genind.obj <- read.structure("UniqueReps.str") 
# Warning: In df2genind(X = X, pop = pop, ploidy = 2, sep = sep, ncode = ncode) :entirely non-type individual(s) deleted
unique(nAll(structure.genind.obj))
nInd(structure.genind.obj)

structure.genind.obj
# Find number of clusters
grp <- find.clusters(structure.genind.obj, max.n.clust=30)
# Number of retained PCs (here, there's no cost to retaining a lot of PCs): 150
# Number of clusters: 8 (has the lowest BIC, 510.2660)
grp$Kstat
grp$stat
grp$grp
grp$size

# Conduct Discriminant analysis of principal components (DAPC)
dapc1 <- dapc(x=structure.genind.obj, pop=grp$grp)

# %%% PCA--MINSAMPLES35 STRUCTURE FILE %%% ----
setwd("/home/akoontz11/kaiser/Primula/ipyrad_UniqueReps/MinSamples35_outfiles/")

# Read in STRUCTURE file to genind object
# 84 individuals, 1069 loci, with genotype labels. Genotypes NOT coded by a single row
min35.genind <- import2genind("MinSamples35.str")
str(min35.genind)

# Read in population label list from custom built csv of populations
setwd("~/kaiser/Primula/code/DAPC/")
min35.pops <- read.table("PopLabels.csv", header=F)
min35.pops <- min35.pops[-1,]
min35.pops
# Assign pop 'slot' to genind object (to make later plotting of PCA easier)
pop(min35.genind) <- min35.pops

# Scale and center the allele frequency matrix, and replace NAs with mean values
s.min35.genind <- scaleGen(x=min35.genind, NA.method="mean")
str(s.min35.genind)
# Perform the PCA (with scale set to FALSE, as matrix is already scaled and centered). Specify number of axes as 7.
min35.pca <- dudi.pca(df = s.min35.genind, scale = FALSE, scannf = FALSE, nf = 7)
min35.pca
# Of this pca object, the most important parts are $eig (eigenvalues), $c1 (principal axes), and $li (principal components)
# Plot this PCA, with sample labels as points
s.label(min35.pca$li, clabel = 0.75)
add.scatter.eig(min35.pca$eig, 3, 1, 2)

# Generate color palette 
colors <- c("#2171B5","#D95F02","#7570B3","#E7298A","#66A61E","#8C510A","#666666")
show_col(colors)

# Redo PCA, with colors for populations
s.class(min35.pca$l1, fac = pop(min35.genind), col = colors, clabel = 0.60)
s.class(min35.pca$l1, fac = pop(min35.genind), col = colors, label = NULL)

# %%% DAPC--MINSAMPLES35 STRUCTURE FILE %%% ----
# Find number of clusters
grp <- find.clusters(min35.genind, max.n.clust=60)
# Number of retained PCs (here, there is no cost to retaining a lot of PCs): 100
# Number of clusters: 12 (has the lowest BIC, 299.1234)
grp$stat

# Conduct DAPC
dapc1 <- dapc(x=min35.genind, pop=grp$grp)
# Number of retained PCs: 10 (although it seems like this value could go higher, even past 80)
# Number of discriminant functions to retain: 3
str(dapc1)
# List grouping of each individual (helpful for determining correct coloration)
dapc1$grp

dapc1$means


# Make a scatterplot of initial DAPC
scatter(dapc1)
scatter(dapc1, label = NULL)
scatter(dapc1, clab = 0.5, label = letters[1:15])

scatter(dapc1, only.grp = as.character(c(1:2,4:9,11:12)), clab=0.45) # Fails, saying only.grp isn't a graphical parameter...

scatter(dapc1, clab = 0, legend = T, txt.leg = paste("Cluster", 1:6))

scatter(dapc1, label = NULL, col = c("#2171B5","#D95F02","#7570B3","#E7298A","#66A61E","#8C510A","#666666"))

# %%% FST CALCULATIONS %%% ----
library(hierfstat)

# %%% Complex-wide analysis %%%
# Read in relevant STRUCTURE file: 82 samples, 1277 loci, 2 rows per individual, etc.
jSub <- read.structure("JuneSubset.str", n.ind=82, n.loc=1277, onerowperind=FALSE,
                       col.lab=1, col.pop=0, ask=FALSE)

# Declare vector of population labels
popLabels <- vector(length = 82)
# Assign labels
# Snake River Plain (cusickiana)
popLabels[grepl("Ck[1-7]", rownames(jSub@tab))] <- 1
# Jarbidge (cusickiana)
popLabels[grepl("Ck[8]", rownames(jSub@tab))] <- 2
# Owyhee (cusickiana)
popLabels[grepl("Ck[9]", rownames(jSub@tab))] <- 3
# domensis
popLabels[grepl("Dm", rownames(jSub@tab))] <- 4
# maguirei
popLabels[grepl("Mg", rownames(jSub@tab))] <- 5
# GRBA (nevadensis)
popLabels[grepl("Nv1[2]", rownames(jSub@tab))] <- 6
# Troy (nevadensis)
popLabels[grepl("Nv1[3]", rownames(jSub@tab))] <- 7
# parryi
popLabels[grepl("Pa", rownames(jSub@tab))] <- 8

# Push population labels into genind object
pop(jSub) <- factor(popLabels)

# (...for checking that samples and population labels line up)
cbind(rownames(jSub@tab), popLabels)

# Calculate fstats between populations
f.stats.all <- pairwise.fst(x = jSub,pop = jSub@pop, res.type = "dist")
f.stats.all

# %%% maguirei-only analysis %%%
# Navigate to directory containing results of maguirei-only ipyrad run
setwd("/home/akoontz11/kaiser/Primula/ipyrad_MaguireiOnly/MaguireiOnly_outfiles")

# Read in relevant STRUCTURE file: 18 samples, 68492 loci, 2 rows per individual, etc.
magOnly <- read.structure("MaguireiOnly.str", n.ind=18, n.loc=68492, onerowperind=FALSE,
                          col.lab=1, col.pop=0, ask=FALSE)

# Declare vector of population labels
magLabels <- vector(length = 18)
# Assign labels
# Second Practice Wall
magLabels[grepl("Mg[3-4]", rownames(magOnly@tab))] <- 1
# Greenhouse Wall
magLabels[grepl("Mg[5]", rownames(magOnly@tab))] <- 2
# Right Hand Fork
magLabels[grepl("Mg[6]", rownames(magOnly@tab))] <- 3
# Seed Source
magLabels[grepl("Mg[7]", rownames(magOnly@tab))] <- 4

# Push population labels into genind object
pop(magOnly) <- factor(magLabels)

# (...for checking that samples and population labels line up)
cbind(rownames(magOnly@tab), magLabels)

# Calculate fstats between populations
f.stats.maguirei <- pairwise.fst(x=magOnly,pop=magOnly@pop)
f.stats.maguirei
