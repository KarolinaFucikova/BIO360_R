library("vegan")
library("dplyr")
library("stringr")
# http://lter.konza.ksu.edu/content/ppl01-konza-prairie-long-term-phosphorus-plots-experiment
konza <- read.csv("data/Konza_PPL011.csv")

# paste will put strings together with a space in between, unlike paste0
konza$GenSp <- paste(konza$Genus, konza$Species)
# turn species list into table of presences/absences
konza_table <- table(konza[,c(4,10)])
head(konza_table)
# abundance is approximated with number of plots where species was present
# this is because each plot was sampled multiple years

specnumber(konza_table)
diversity(konza_table)
diversity(konza_table, index="simpson")

konza.matrix <- vegdist(konza_table, method="euclidean")
konza.clust <- hclust(konza.matrix) # makes a hierarchical cluster object
plot(konza.clust)

# parsing by treatment instead of by plot
konza_table <- table(konza[,c(5,10)])
head(konza_table)
# abundance is approximated with number of plots where species was present
# this is because each plot was sampled multiple years

specnumber(konza_table)
diversity(konza_table)
diversity(konza_table, index="simpson")

konza.matrix <- vegdist(konza_table, method="euclidean")
konza.clust <- hclust(konza.matrix) # makes a hierarchical cluster object
plot(konza.clust)



