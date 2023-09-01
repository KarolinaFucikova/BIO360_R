library("vegan")
hornbill <- read.csv("data/hornbill.csv", row.names=1)
plant <- read.csv("data/plant_list.csv")

plants <- table(plant$Species, plant$Site)
colSums(hornbill)
colSums(plants)

# can use base R function dist, or vegan package's vegdist
# data need to be transposed first
horn <- t(hornbill)
plantdata <- t(plants)

vegdist(horn, method="jaccard")
vegdist(plantdata, method="jaccard")
1-vegdist(horn, method="jaccard")

horn.matrix <- vegdist(horn, method="jaccard")
horn.clust <- hclust(horn.matrix) # makes a hierarchical cluster object
plot(horn.clust)

plant.matrix <- vegdist(plantdata, method="jaccard")
plant.clust <- hclust(plant.matrix) # makes a hierarchical cluster object
plot(plant.clust)

moss <- t(read.csv("data/bryo.csv", row.names=1))
head(moss)

specnumber(moss)
diversity(moss)
diversity(moss, index="simpson")

moss.matrix <- vegdist(moss, method="euclidean")
moss.clust <- hclust(moss.matrix) # makes a hierarchical cluster object
plot(moss.clust)


# Vltava data sets from David Zeleny's website: 
# https://anadat-r.davidzeleny.net/doku.php/en:data:vltava
vltava.spe <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava-spe.txt', row.names = 1)
vltava.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava-env.txt')
vltava.ell <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava-ell.txt', row.names = 1)
load (url ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava.RData'))

specnumber(vltava.spe)
diversity(vltava.spe)

vltava.matrix <- vegdist(vltava.spe, method="bray")
vltava.clust <- hclust(vltava.matrix) # makes a hierarchical cluster object
plot(vltava.clust)

png ('pcoa_nmds.png', width = 8, height = 4, units = 'in', res = 300, pointsize = 11)
par (mfrow = c(1,2))
pcoa <- capscale (log1p (vltava.spe) ~ 1, distance = 'bray', sqrt.dist = TRUE, scaling = 1)
plot (pcoa, main = 'PCoA (MDS)', type = 'n')
points (pcoa, display = 'si', col = vltava.env$GROUP, pch = vltava.env$GROUP)
text (pcoa, display = 'sp', col = "#FF000080", cex = 0.6, scaling = 1, select = colSums (vltava.spe>0)>20)
legend ('bottomleft', pch = 1:4, col = 1:4, legend = 1:4, title = 'GROUP', cex = 0.6)

nmds <- metaMDS (log1p (vltava.spe), distance = 'bray', scaling = 1)
plot (nmds, main = 'NMDS', type = 'n', display = 'si')
points (nmds, display = 'si', col = vltava.env$GROUP, pch = vltava.env$GROUP)
text (nmds, display = 'sp', col = "#FF000080", cex = 0.6, select = colSums (vltava.spe>0)>20)
dev.off ()



