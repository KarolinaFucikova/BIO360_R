lupine <- read.csv("data/Kalske_plants.csv", row.names=1)

boxplot(G.height3~inoc, data=lupine)
