# students will not have this script; they are only to follow directions in the handout
# this is just a bare-bones script; there are more tasks in the handout to do 
# e.g. add color to plots

# import data
Boleria <- read.csv("data/Bowden_Boleria.csv")

# plot just wing length for the two sexes, regardless of year
boxplot(WL~sex, data=Boleria)

boxplot(WL~year, data=Boleria)
plot(Boleria$year, Boleria$WL)

plot(Boleria$year, Boleria$snow)
model <- lm(Boleria$snow ~ Boleria$year)
abline(model)

plot(Boleria$mayaug.1, Boleria$WL)
model <- lm(Boleria$WL ~ Boleria$mayaug.1)
abline(model)
