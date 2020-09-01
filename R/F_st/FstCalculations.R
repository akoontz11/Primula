# %%% FST CALCULATIONS %%%
library(hierfstat)

# %%% Complex-wide analysis %%%----
# Navigate to directory containing results of JuneSubset ipyrad run
setwd("/home/akoontz11/kaiser/Code/ipyrad/JuneSubset/Outfiles")

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

# %%% maguirei-only analysis %%%----
# Navigate to directory containing maguirei-only STRUCTURE file
setwd("/home/akoontz11/kaiser/Code/structure/MaguireiOnly")

# Read in relevant .ustr file (smaller in size): 18 samples, 17988 loci, 2 rows per individual, etc.
magOnly <- read.structure("MaguireiOnly.stru", n.ind=18, n.loc=17988, onerowperind=FALSE,
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

# %%% Snake River Plain-only analysis %%%----
# Navigate to directory containing Snake River Plain-only STRUCTURE file
setwd("/home/akoontz11/kaiser/Code/structure/SnakeRiverPlainOnly")

# Read in relevant .stru (.ustr) file (smaller in size): 24 samples, 4410 loci, 2 rows per individual, etc.
SRPOnly <- read.structure("SnakeRiverPlainOnly.stru", n.ind=24, n.loc=4410, onerowperind=FALSE,
                          col.lab=1, col.pop=0, ask=FALSE)

# Declare vector of population labels
SRPLabels <- vector(length = 24)
# Assign labels
# Boise
SRPLabels[grepl("Ck[0-9]", rownames(SRPOnly@tab))] <- 1
# Bear
SRPLabels[grepl("Ck[1-2][0-9]", rownames(SRPOnly@tab))] <- 2
# Camas Prairie
SRPLabels[grepl("Ck[2-3][0-9]", rownames(SRPOnly@tab))] <- 3
# CRMO
SRPLabels[grepl("Ck[4-5][0-9]", rownames(SRPOnly@tab))] <- 4

# Push population labels into genind object
pop(SRPOnly) <- factor(SRPLabels)

# (...for checking that samples and population labels line up)
cbind(rownames(SRPOnly@tab), SRPLabels)

# Calculate fstats between populations
f.stats.SRP <- pairwise.fst(x=SRPOnly,pop=SRPOnly@pop)
f.stats.SRP
