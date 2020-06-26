# load in ks from clummpack files
k2 <- read.csv("k2.txt", sep = '', header = F)
k2 <- k2[,-(1:5)]
k3 <- read.csv("k3.txt", sep = '', header = F)
k3 <- k3[,-(1:5)]
k4 <- read.csv("k4.txt", sep = '', header = F)
k4 <- k4[,-(1:5)]
k5 <- read.csv("k5.txt", sep = '', header = F)
k5 <- k5[,-(1:5)]
k6 <- read.csv("k6.txt", sep = '', header = F)
k6 <- k6[,-(1:5)]
k7 <- read.csv("k7.txt", sep = '', header = F)
k7 <- k7[,-(1:5)]

# names file includes individual ids, species names, and geographic locations
names <- read.csv("names.txt", sep = ',', header = T)

x <- as.data.frame(matrix(ncol = 32, nrow = 56))
x[,1:2] <- k2
x[,3:5] <- k3
x[,6:9] <- k4
x[,10:14] <- k5
x[,15:20] <- k6
x[,21:27] <- k7
x[,28:32] <- names

# order by species, then geography
x <- x[order(x$V30, x$V32),]

# list for plotting
klist <- list(x[,1:2], x[,3:5], x[,6:9], x[,10:14], x[15:20], x[,21:27])
k5list <- list(x[10:14])

# geography for k = 6
kglist <- list(g[15:20])

structure_plot(x[,30], ninds = 56, klist)

#######################################################
# functions needed
######################################################

# plotting and labeling function
structure_plot <- function(labels, ninds = 60, klist){
  # define colors
  cols <- c('#A8FFFD', '#A39D9D','#FFFF00', '#ff5a5a', '#69C261', '#26CDCD', '#B862D3','#C1C6FF')
  # unique label names
  sp.names <- as.character(unique(labels))
  #n <- as.data.frame(matrix(ncol = 1, nrow = ninds))
  #n[,1] <- names
  # locations of each column
  b <- as.data.frame(matrix(ncol = 1, nrow = ninds))
  b[,1] <- barplot(t(klist[[1]][1]), beside= F, col= cols, cex.name= 1, cex.axis= 1.2, border = 1, space = 0.05, xaxt = 'n', yaxt = 'n', cex.lab = 1, cex.main = 2)
  # find locations for labels in the barplot
  my.mean <- tapply(X = b[,1], INDEX = labels, mean)
  my.min <- tapply(X = b[,1], INDEX = labels, min)
  my.max <- tapply(X = b[,1], INDEX = labels, max)
  # data frame for plotting
  d <- sp_labels(names = sp.names, min = my.min, mean = my.mean, max = my.max)
  # plot
  plot_q_per_chain(klist)
  text(cex = 1.5, x = (d[,2]-0.3), y = -0.7, labels = d[,1], xpd=NA, srt=50, font=3)
  # lines
  for (i in 1:length(d[,1])){
    lines(x = d[i,3:4] , y = rep(-0.1, 2), lwd = 2.5, col = "black", xpd = NA)
  }
  
}

# create labels
sp_labels <- function(names, min, mean, max, ...){
  d <- as.data.frame(matrix(nrow = length(names), ncol = 4))
  for (j in 1:length(names)){
    d[j,1] <- names[j]
    d[j,3] <- min[[j]][1]
    d[j,2] <- mean[[j]][1]
    d[j,4] <- max[[j]][1]
  }
  return(d)
}

# plot chains with species and geography labels 
plot_q_per_chain <- function(kqlist, ...){
  # thalictroides 2, gaudichaudii, thalictroides 1, pteridoides, cornuta, misc, 
  cols <- c('#A8FFFD','#B862D3', '#A39D9D','#FFFF00', '#69C261', '#FF59AC', '#26CDCD',  '#C1C6FF') 
  #cols <- c('#000075', '#E6194B', '#AAFFC3', '#FFE119', '#F58231', '#3CB44B')
  par(mfrow = c(length(kqlist),1), mar = c(1,3,3,1) + 0.1, oma = c(15,0,0,0), mgp = c(1,1,0))
  chain <- seq(1, length(kqlist), 1) 
  for(i in 1:length(kqlist)){
    barplot(t(kqlist[[i]]), beside= F, col= cols, border = 1, space = 0.05, xaxt = 'n', yaxt = 'n', main = paste("k =", chain[i]+1, sep = ' '), cex.lab = 1.2, cex.main = 1.6)
    # y axis
    axis(2, at = c(0, 0.25, 0.5, 0.75, 1), cex.axis = 1, las = 2, pos = -0.2)
  }
}

structure_plot(labels = names[,4], ninds = 60, klist = k6list)

############################################################################
############################################################################
# for k=5 only
plot_k5 <- function(kqlist, ...){
  cols <- c('#A8FFFD','#B862D3', '#A39D9D','#FFFF00', '#69C261', '#FF59AC', '#26CDCD',  '#C1C6FF')    
  #cols <- c('#A8FFFD', '#B862D3','#A39D9D','#FFFF00', '#ff5a5a', '#69C261', '#26CDCD', '#C1C6FF')    
  par(mfrow = c(length(kqlist),1), mar = c(1,3,3,1) + 0.1, oma = c(15,0,0,0), mgp = c(1,1,0))
  chain <- seq(1, length(kqlist), 1) 
  for(i in 1:length(kqlist)){
    barplot(t(kqlist[[i]]), beside= F, col= cols, border = 1, space = 0.05, xaxt = 'n', yaxt = 'n', main = substitute(paste(italic(K)," = 5")), cex.lab = 1.6, cex.main = 2.0, ylab = "Proportion of genome")
    # y axis
    axis(2, at = c(0, 0.25, 0.5, 0.75, 1), cex.axis = 1, las = 2, pos = -0.2)
  }
}

plot_k5(k5list)

# x axis, rotating labels
text(cex = 1.7, x = (d[,2]-0.4), y = -0.3, labels = d[,1], xpd=NA, srt=50, font=3)

for (i in 1:length(d[,1])){
  lines(x = d[i,3:4] , y = rep(-0.05, 2), lwd = 2.5, col = "black", xpd = NA)
}