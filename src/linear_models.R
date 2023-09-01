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
      
#######################
# species-area curve model fitting on Solomon Island bird data
solomon <- read.csv("data/Solomon_birds.csv")
head(solomon)
# initial parameters
z <- 0.3
c <- 10
# the formula is S = c*A^z
# or logS = logc + zlogA
#solomon.lm <- lm(log(Birds)~log(Area)+log(1), data=solomon)
#fitted(solomon.lm)
#plot(log(solomon))
#lines(spline(solomon$Area, fitted(solomon.lm)), lwd=2)
