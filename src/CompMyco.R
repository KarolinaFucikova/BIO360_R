# nifty trick to clear environment if saved accidentally:
rm(list = ls())

# competition and mycorrhizae experiments
comp <- read.csv("data/BIO360Competition.csv")
myco <- read.csv("data/BIO360Mycorrhizae.csv")

head(comp)
head(myco)

# analyzing with 2-factor ANOVA for all  response variables respectively.
# graphing each response variable with a boxplot
# Tukey tests not conducted for most

# competition data

comp.aov <- aov(Total_length~Competition*Nitrogen, data=comp, na.rm=TRUE)
summary(comp.aov)
boxplot(Total_length~Competition*Nitrogen, data=comp)

comp.aov <- aov(Shoot_length~Competition*Nitrogen, data=comp, na.rm=TRUE)
summary(comp.aov)
boxplot(Shoot_length~Competition*Nitrogen, data=comp)

comp.aov <- aov(Root_length~Competition*Nitrogen, data=comp, na.rm=TRUE)
summary(comp.aov)
TukeyHSD(comp.aov)
boxplot(Root_length~Competition*Nitrogen, data=comp)
# nitrogen makes a difference in the no-competition treatment

comp.aov <- aov(Total_mass~Competition*Nitrogen, data=comp, na.rm=TRUE)
summary(comp.aov)
boxplot(Total_mass~Competition*Nitrogen, data=comp)

comp.aov <- aov(Shoot_mass~Competition*Nitrogen, data=comp, na.rm=TRUE)
summary(comp.aov)
boxplot(Shoot_mass~Competition*Nitrogen, data=comp)

comp.aov <- aov(Root_mass~Competition*Nitrogen, data=comp, na.rm=TRUE)
summary(comp.aov)
boxplot(Root_mass~Competition*Nitrogen, data=comp)

comp.aov <- aov(Leaf_number~Competition*Nitrogen, data=comp, na.rm=TRUE)
summary(comp.aov)
boxplot(Leaf_number~Competition*Nitrogen, data=comp)

comp.aov <- aov(Leaf_width~Competition*Nitrogen, data=comp, na.rm=TRUE)
summary(comp.aov)
boxplot(Leaf_width~Competition*Nitrogen, data=comp)

comp.aov <- aov(germinated~Competition*Nitrogen, data=comp, na.rm=TRUE)
summary(comp.aov)
boxplot(germinated~Competition*Nitrogen, data=comp)


# mycorrhizae data
myco.aov <- aov(Total_length~Species*Soil, data=myco, na.rm=TRUE)
summary(myco.aov)
boxplot(Total_length~Species*Soil, data=myco)

myco.aov <- aov(Shoot_length~Species*Soil, data=myco, na.rm=TRUE)
summary(myco.aov)
boxplot(Shoot_length~Species*Soil, data=myco)

myco.aov <- aov(Root_length~Species*Soil, data=myco, na.rm=TRUE)
summary(myco.aov)
boxplot(Root_length~Species*Soil, data=myco)

myco.aov <- aov(Total_mass~Species*Soil, data=myco, na.rm=TRUE)
summary(myco.aov)
boxplot(Total_mass~Species*Soil, data=myco,
        names=c("No fungi", "No fungi", "Fungi", "Fungi"),
        col=c("forestgreen", "lightgreen", "forestgreen","lightgreen"),
        xlab="Soil treatment", ylab="Total mass")
legend(3.5, 20, legend=c("corn", "broccolini"),
       col=c("forestgreen", "lightgreen"), pch = 15, cex=1)


myco.aov <- aov(Shoot_mass~Species*Soil, data=myco, na.rm=TRUE)
summary(myco.aov)
boxplot(Shoot_mass~Species*Soil, data=myco)

myco.aov <- aov(Root_mass~Species*Soil, data=myco, na.rm=TRUE)
summary(myco.aov)
boxplot(Root_mass~Species*Soil, data=myco)

myco.aov <- aov(Leaf_number~Species*Soil, data=myco, na.rm=TRUE)
summary(myco.aov)
boxplot(Leaf_number~Species*Soil, data=myco)

myco.aov <- aov(Leaf_width~Species*Soil, data=myco, na.rm=TRUE)
summary(myco.aov)
boxplot(Leaf_width~Species*Soil, data=myco)

