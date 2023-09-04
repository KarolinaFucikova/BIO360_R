butterflies <- read.csv("data/bff.csv")
head(butterflies)

cor.test(~food+count, data=butterflies)
cor.test(~nectar+count, data=butterflies)
cor.test(~food+nectar, data=butterflies)

butter.lm <- lm(count ~ food + nectar, data=butterflies)
summary(butter.lm)

mayfly <- read.csv("data/mayfly_regression.csv")
head(mayfly)
cor(mayfly)

cor(mayfly$Length, mayfly)

mod <- lm(Length~1, data=mayfly)
add1(mod, scope=mayfly, test ="F")
mod <- lm(Length~BOD, data=mayfly) # this of course overwrites the earlier blank model
summary(mod)
add1(mod, scope=mayfly, test ="F")

blue <- read.csv("data/bluebell.csv")
head(blue)
plot(blue)

blue.lm <- lm(Abundance ~ Light + I(Light^2), data = blue)
summary(blue.lm)

fitted(blue.lm)

plot(blue)
lines(spline(blue$Light, fitted(blue.lm)), lwd=2)

### logistic regression
log <- read.csv("data/log_growth.csv")
names(log)
plot(log)
log.lm <- lm(Growth ~ log(Nutrient), data = log)
summary(log.lm)

plot(log)
lines(spline(log$Nutrient, fitted(log.lm)), lwd=2)

plot(Growth ~ log(Nutrient), data = log)
abline (log.lm)

      
#######################
# species-area curve model fitting on Solomon Island bird data
# S = cA^z, or: logS = logc + zlogA
# excel results: birds = 27.63*area^0.15
# or: log(birds) = log(27.63) + 0.15*log(area)
# or: log(birds) = 1.44 + 0.15*log(area)
solomon <- read.csv("data/Solomon_birds.csv")
head(solomon)
sol.lm <- lm(Birds ~ log(Area), data = solomon)
summary(sol.lm)

# birds = 8.1*Ln(Area) + 19.8

plot(solomon)
lines(spline(solomon$Area, fitted(sol.lm)), lwd=2)

plot(Birds ~ log(Area), data = solomon)
abline(sol.lm)

# sol.lm2 <- lm(log10(Birds) ~ log10(Area), data = solomon)
# summary(sol.lm2)
#plot(log10(Birds) ~ log10(Area), data = solomon)
# log10(Birds) = 0.18*log10(Area) + 1.37
# 1.44 vs. 1.37 for log(c) and 0.15 vs. 0.18 for z - NOT BAD R vs. Excel

