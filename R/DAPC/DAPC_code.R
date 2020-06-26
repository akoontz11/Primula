# %%% DAPC--JUNESUBSET STRUCTURE FILE %%% ----
setwd("/home/akoontz11/kaiser/Primula/ipyrad/JuneSubset/JuneSubset_outfiles/")

# Read in STRUCTURE file to genind object
# 82 individuals, 1277 loci, with genotype labels. Genotypes NOT coded by a single row
junesub.genind <- import2genind("JuneSubset.str")
str(junesub.genind)

# Generate color palette 
colors <- c("#2171B5","#D95F02","#7570B3","#E7298A","#66A61E","#8C510A","#666666")
show_col(colors)

# Find number of clusters
grp <- find.clusters(junesub.genind, max.n.clust=60)
# Number of retained PCs (here, there is no cost to retaining a lot of PCs): 80
# Number of clusters: 11 (has the lowest BIC, 299.1234)
grp$stat

# Conduct DAPC
dapc1 <- dapc(x=junesub.genind, pop=grp$grp)
# Number of retained PCs: 7
# Number of discriminant functions to retain: 3
str(dapc1)
# List grouping of each individual (helpful for determining correct coloration)
dapc1$grp

# Make a scatterplot of initial DAPC
scatter(dapc1)
scatter(dapc1, label = NULL)
scatter(dapc1, clab = 0.5, label = letters[1:15])
scatter(dapc1, clab = 0.5)

scatter(dapc1, scree.da=F, bg="white", pch=20, cell=0, cstar=0, solid=0.6, clab=0, legend=T,
        posi.leg=locator(n=1), cleg=1.0, cex=1.8, inset.solid=1, 
        col = c("#D95F02","#E7298A","#66A61E","#2171B5","#8C510A","#7570B3","#FF0000"), 
        txt.leg = c("cusickiana_Jarbidge","cusickiana_Owyhee","maguirei","parryi","cusickiana_Idaho","domensis","nevadensis"))
