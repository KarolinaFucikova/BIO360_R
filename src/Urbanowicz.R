#import data
bees <- read.csv("data/Urbanowicz_VisitationData.csv")
head(bees)

# anova for pollinator visitation by type of pollinator and plant species
bees.aov <- aov(totalVisits~FlowerTaxonomicName*beeCat, data=bees)
summary(bees.aov)
head(TukeyHSD(bees.aov))
# that's too much

boxplot(totalVisits~FlowerTaxonomicName*beeCat, data=bees)
# yeah, definitely too much


boxplot(totalVisits~beeCat*introducedOrNative, data=bees)

plot(floralAbundancePer100PerWeek~totalVisits, data=bees)
