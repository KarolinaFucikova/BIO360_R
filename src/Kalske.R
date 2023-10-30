library("ggplot2")
library("dplyr")
library("ggpubr")

lupine <- read.csv("data/Kalske_plants.csv", row.names=1)

boxplot(G.height3~inoc, data=lupine)
boxplot(G.height3~inoc*origin, data=lupine, xlab = "Soil inoculum",
        ylab = "Plant height at end of experiment",
        names = c("dead", "live", "dead", "live"),
        col = c("#807F7F", "#807F7F","#BF504D","#BF504D"))
legend(2.8, 32, legend=c("Finnish", "US"),
       col=c("#807F7F","#BF504D"), pch = 15, cex=1)


#BA.LA is the leaf area consumed by snail - lower values mean higher resistance
boxplot(BA.LA~inoc*origin, data=lupine)
summary(aov(BA.LA~inoc*origin, data=lupine))

ggbarplot(
  lupine, x = "inoc", y = "BA.LA", 
  add = c("mean_ci", "jitter"), 
  color = "origin",
  position = position_dodge(0.8)) 

# plotting the herbivore resistance, with standard error instead of conf. int.
ggbarplot(
  lupine, x = "inoc", y = "BA.LA", 
  add = c("mean_se", "jitter"), 
  color = "origin",
  position = position_dodge(0.8),
  xlab = "Soil inoculum", ylab = "Leaf area consumed by snail") 

# further options:
# https://rpkgs.datanovia.com/ggpubr/reference/ggbarplot.html
