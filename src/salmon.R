salmon1 <- read.csv("data/Salmon_lev1.csv")
head(salmon1)
boxplot(Delta.Capacity.Total~Month, data=salmon1)
