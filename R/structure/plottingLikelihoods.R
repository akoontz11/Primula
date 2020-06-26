setwd("~/kaiser/Primula/structure/PopNames/")
# ----%%% READ IN LIKELIHOOD FILES %%%----
K2 <- read.table("K_2_likelihoods.txt")
K2 <- K2$V7

K3 <- read.table("K_3_likelihoods.txt")
K3 <- K3$V7

K4 <- read.table("K_4_likelihoods.txt")
K4 <- K4$V7

K5 <- read.table("K_5_likelihoods.txt")
K5 <- K5$V7

K6 <- read.table("K_6_likelihoods.txt")
K6 <- K6$V7

K7 <- read.table("K_7_likelihoods.txt")
K7 <- K7$V7

K8 <- read.table("K_8_likelihoods.txt")
K8 <- K8$V7

K9 <- read.table("K_9_likelihoods.txt")
K9 <- K9$V7

K10 <- read.table("K_10_likelihoods.txt")
K10 <- K10$V7

# ----%%% PLOT %%%----
likelihoods <- cbind(K2,K3,K4,K5,K6,K7,K8,K9,K10)
likelihoods

sds <- apply(likelihoods, 2, sd)
sds

K_values <- 2:10

plot(means ~ K_values, pch=16, ylab="Mean likelihood values")
# Using arrows function to add error bars onto points
arrows(K_values, means-sds, K_values, means+sds, code=3, length=0.02, angle = 90)
