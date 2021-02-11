library(adegenet)
# %%% DAPC--JUNESUBSET STRUCTURE FILE %%% ----
setwd("/home/austin/Documents/WolfLab/Primula/Analysis/Code/ipyrad/JuneSubset/Outfiles/")

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
# Number of clusters: 7 (even though 11 has the lowest BIC)
grp$stat

# Conduct DAPC
dapc1 <- dapc(x=junesub.genind, pop=grp$grp)
# Number of retained PCs: 7
# Number of discriminant functions to retain: 2
str(dapc1)
# List grouping of each individual (helpful for determining correct coloration)
dapc1$grp
# PC axis variance values
eig.perc <- 100*dapc1$eig/sum(dapc1$eig)

# Make a scatterplot of initial DAPC
scatter(dapc1)
scatter(dapc1, label = NULL)
scatter(dapc1, clab = 0.5, label = letters[1:15])
scatter(dapc1, clab = 0.5, scree.da = F)

# Systematic Botany figure
txt.leg <- c("cusickiana_Idaho","domensis","parryi","cusickiana_Owyhee","cusickiana_Jarbidge","nevadensis","maguirei")
par(mar=c(5,1,1,1)+0.1, mgp = c(3,1,1))
scatter(dapc1, clab = 0, scree.da = F, cstar=0, solid=0.6, cex=1.2,
        col=c("#8C510A","#7570B3","#2171B5","#E7298A","#D95F02","#666666","#66A61E"))
legend(x=-55, y=1.0, legend=txt.leg, cex=1, ncol=2, pch=16, bty="n",
       col=c("#8C510A","#7570B3","#2171B5","#E7298A","#D95F02","#666666","#66A61E"),
       x.intersp=0.2, y.intersp=0.15, text.width=20, xjust=0)
text(x=-63, y=1, cex=0.8, "PC 1: 49.04%")
text(x=-1, y=24, cex=0.8, srt=90, "PC 2: 27.27%")


scatter(dapc1, scree.da=F, bg="white", pch=20, cell=0, cstar=0, solid=0.6, clab=0, legend=T,
        posi.leg=locator(n=1), cleg=1.0, cex=1.8, inset.solid=1, 
        col = c("#66A61E","#E7298A","#7570B3","#2171B5","#666666","#8C510A","#D95F02"), 
        txt.leg = c("maguirei","cusickiana_Owyhee","domensis","parryi","nevadensis","cusickiana_Idaho","cusickiana_Jarbidge"))

# Plot for ESA
scatter(dapc1, scree.da=F, bg="white", pch=20, cell=0, cstar=0, solid=0.6, clab=0, legend=F,
        cleg=1.0, cex=2.5, inset.solid=1, 
        col = c("#E7298A","#D95F02","#8C510A","#66A61E","#666666","#2171B5","#7570B3"))
