# another bare-bones script; student are to make their own
# worst case scenario at least fully annotate this one
# import seed data
seeds <- read.csv("data/seeds.csv")
t.test(seeds$maple2, seeds$elm2, paired=FALSE, var.equal=FALSE)
t.test(seeds$maple2, seeds$elm2)

seeds2 <-seeds[, 3:4]
boxplot(seeds2, col = c("coral", "gray"), names = c("Maple seeds", "Elm seeds"),
        xlab = "Tree species", ylab = "Distance traveled")

#seed_means <- apply(seeds2, MARGIN = 2, FUN = mean, na.rm = TRUE)
#barplot(seed_means)

wilcox.test(seeds$maple,seeds$elm)

sward2 <- read.csv("data/sward2.csv")
boxplot(Height~Site, data=sward2, col =c("coral", "gray", "tan"))

aov(Height~Site, data=sward2)
sward2.aov <- aov(Height~Site, data=sward2)
summary(sward2.aov)
TukeyHSD(sward2.aov)

sward3 <- read.csv("data/sward3.csv")
sward3.aov <- aov(growth~Water*species, data=sward3)
summary(sward3.aov)
TukeyHSD(sward3.aov)

colors <- c("coral", "coral", "coral", "tan", "tan", "tan")
nam <- c("upper", "lower", "middle", "upper", "lower", "middle")
boxplot(growth~Water*species, data=sward3, col = colors, names = nam, 
        xlab = "Site", ylab = "Growth")
legend(5, 40, legend=c("sativa", "vulgaris"),
       col=c("coral", "tan"), pch = 15, cex=1)

Bowden_full <- read.csv("data/Bowden_full.csv")
head(Bowden_full)
bowden.aov <- aov(WL~sex*species, data=Bowden_full)
summary(bowden.aov)

boxplot(WL~sex*species, data=Bowden_full)
colors <- c("coral", "tan", "coral", "tan")
nam <- c("Boleria", "Boleria", "Colias", "Colias")
boxplot(WL~sex*species, data=Bowden_full, col = colors, names = nam, 
        xlab = "Species", ylab = "Wing Length")
legend(1, 25, legend=c("female", "male"),
       col=c("coral", "tan"), pch = 15, cex=1)

##################
birds <- read.csv("data/birds.csv", row.names = 1)
bird <- as.matrix(birds)
# pie(bird[,2], main="Hedgerow")

barplot(bird, legend = TRUE)
barplot(bird, legend=TRUE, beside=TRUE)

my.chi <- chisq.test(bird)
my.chi$expected
my.chi$observed
my.chi$res

barplot(my.chi$res[,1])
abline(h=0)

barplot(t(my.chi$res), legend=TRUE, beside=TRUE, xlab = "Bird species", ylab = "Pearson residual")
abline(h=c(-2,2), lty = "dotted")

#barplot(my.chi$res, legend=TRUE, beside=TRUE, xlab = "Habitat", ylab = "Pearson residual")
#abline(h=c(-2,2), lty = "dotted")

invert <- read.csv("data/invert.csv")
head(invert)
plot(abund~flow, data=invert)

hist(invert$abund)
shapiro.test(invert$abund)

cor.test(~flow+abund, data=invert, method = "pearson")
cor.test(invert$flow, invert$abund, method = "pearson")

lm(invert$abund~invert$flow)
inv.lm <- lm(invert$abund~invert$flow)
coef(inv.lm)

plot(invert$abund~invert$flow, xlab = "Flow velocity", ylab = "Invertebrate abundance")
abline(inv.lm)
text(locator(1), "y = 0.79 + 8.25")
