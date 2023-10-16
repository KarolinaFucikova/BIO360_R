#################################################################################
# Population growth and sequestration of plant toxins along a gradient of       #
# specialization in four aphid species on the common milkweed Asclepias syriaca #
# T. Zuest and A.A. Agrawal                                                     #
#################################################################################

## load required libraries
library(lattice)
library(nlme)
library(gmodels)

## set working directory
# setwd(choose.dir()) - apparently OS specific, doesn't work on all machines

## read in datafiles
d.aph <-read.table("data/Zust_Aphid data.txt", header=T)
d.pl  <-read.table("data/Zust_Plant data.txt", header=T)
d.hd  <-read.table("data/Zust_Honeydew data.txt", header=T)
d.che <-read.table("data/Zust_Chemistry data.txt", header=T)
d.gen <-read.table("data/Zust_Genotype means.txt", header=T)


################################################################################
########################## Aphid population growth #############################
################################################################################

## define numeric Genotype identifier as factor
d.aph$Genotype<-as.factor(d.aph$Genotype)
head(d.aph)
head(d.pl)
head(d.che)
head(d.gen)

# trying some raw graphing
# genotype means data first
boxplot(Card.tot~Genotype, data=d.gen)
mod <- lm(RGR.ma~Card.tot, data=d.gen)
plot(RGR.ma~Card.tot, data=d.gen)
abline(mod)

mod <- lm(Card.tot~C.N, data=d.gen)
plot(Card.tot~C.N, data=d.gen)
abline(mod)

mod <- lm(RGR.mp~C.N, data=d.gen)
plot(RGR.mp~C.N, data=d.gen)
abline(mod)

# growth rate as a function of toxicity (apolar card.)
mod <- lm(RGR.mp~Card.apo, data=d.gen) # generalist species
plot(RGR.mp~Card.apo, data=d.gen)
abline(mod)

mod <- lm(RGR.an~Card.apo, data=d.gen) # broad specialist species
plot(RGR.an~Card.apo, data=d.gen)
abline(mod)

mod <- lm(RGR.ma~Card.apo, data=d.gen) # monophagous specialist species
plot(RGR.ma~Card.apo, data=d.gen)
abline(mod)

# aphid data
boxplot(Juvenile~Genotype*Species, data=d.aph, col="skyblue")
boxplot(Juvenile~Species, data=d.aph)
boxplot(Sum~Species, data=d.aph)
boxplot(Sum~Genotype, data=d.aph)
# subsetting only for some genotypes
# boxplot(Juvenile~Genotype, data=d.aph, subset=Genotype %in% c(1,2))
#boxplot(Juvenile~Genotype*Species, data=d.aph, frame = FALSE,
#        col = c("#00AFBB", "#E7B800"), ylab="Juvenile aphids")
dev.off()
# the above clears plot panel settings if needed (they are set later in the code)
interaction.plot(x.factor = d.aph$Census, trace.factor = d.aph$Species, 
                 response = d.aph$Sum, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Census Times", ylab="Total aphid count",
                 pch=c(1,4,8,19), col = c("#00AFBB", "#E7B800", "gray", "black"))

interaction.plot(x.factor = d.aph$Census, trace.factor = d.aph$Genotype, 
                 response = d.aph$Sum, fun = mean, 
                 type = "b", legend = FALSE, 
                 xlab = "Census Times", ylab="Total aphid count",
                 pch=c(1,19))


### remove replicates with problematic growth patterns
# M.asc : 17
d.aph<-subset(d.aph, ID!="1_07" & ID!="3_05" & ID!="5_08" & ID!="11_01" & ID!="11_05" & ID!="13_05" & ID!="13_10" & ID!="17_04" & ID!="17_08" & ID!="23_01" & ID!="27_15" & ID!="31_17" & ID!="33_01" & ID!="41_05" & ID!="43_14" & ID!="45_14" & ID!="45_15")
# A.asc : 11
d.aph<-subset(d.aph, ID!="7_15" & ID!="11_08" & ID!="23_04" & ID!="25_02" & ID!="27_04" & ID!="27_16" & ID!="33_10" & ID!="35_13" & ID!="43_09" & ID!="47_08" & ID!="47_09")
# M.per : 1
d.aph<-subset(d.aph, ID!="45_13")


### remove NAs and zeros
d.aph<-subset(d.aph, Sum!="NA" & Sum>0)

### log-transform aphid numbers
d.aph$Log.sum<-log(d.aph$Sum)


### define linear growth function
loglin1 <- function(x, M0, r)
+  (M0+r*x)

### define control statement to lower convergence threshold
myControl<-nlmeControl(maxIter=1000, tolerance=1e1)


################################################################################
##### 1. check deviations from exponential growth

### specify structure of data using groupedData()
attach(d.aph)
model.data.1<-groupedData(Log.sum ~ Growth.days | ID, outer=~Genotype+Chamber+Species, inner=~Census)
detach(d.aph)

### build model using nlsList()
test.fit1 <- nlsList(Log.sum ~ loglin1(Growth.days, M0, r), start=c(M0=log(5), r=0.2), model.data.1)

### model fitting using nlme
lin1 <- nlme(test.fit1,  control=myControl)


### plot residuals against census day
par(mfrow=c(2,2))
plot(resid(lin1)[d.aph$Species=="A.ner"] ~ d.aph$Census[d.aph$Species=="A.ner"]); abline(h=0, lty=2) # --> problem
plot(resid(lin1)[d.aph$Species=="M.per"] ~ d.aph$Census[d.aph$Species=="M.per"]); abline(h=0, lty=2) # --> problem
plot(resid(lin1)[d.aph$Species=="A.asc"] ~ d.aph$Census[d.aph$Species=="A.asc"]); abline(h=0, lty=2) # --> ok
plot(resid(lin1)[d.aph$Species=="M.asc"] ~ d.aph$Census[d.aph$Species=="M.asc"]); abline(h=0, lty=2) # --> ok

### check residuals of individual genotypes for deviating A. ner and M.per
par(mfrow=c(4,5), mar=c(3,3,2,2))
for(i in 1:20){
plot(resid(lin1)[d.aph$Species=="A.ner" & d.aph$Genotype==levels(d.aph$Genotype)[i]] ~ d.aph$Census[d.aph$Species=="A.ner" & d.aph$Genotype==levels(d.aph$Genotype)[i]], main=levels(d.aph$Genotype)[i], xlab="", ylab="", )
abline(h=0, lty=2)}

## --> census 5 deviates for all genotypes except 5, 17, 33, 35, and 47

for(i in 1:20){
plot(resid(lin1)[d.aph$Species=="M.per" & d.aph$Genotype==levels(d.aph$Genotype)[i]] ~ d.aph$Census[d.aph$Species=="M.per" & d.aph$Genotype==levels(d.aph$Genotype)[i]], main=levels(d.aph$Genotype)[i], xlab="", ylab="", )
abline(h=0, lty=2)}

## --> census 5 deviates for all genotypes


### remove deviating datapoints
d.aph2 <-subset(d.aph, Species=="M.asc" | Species=="A.asc" | Species=="A.ner" & Census<5 | Species=="M.per" & Census<5 |
              Species=="A.ner" & Genotype=="5" | Species=="A.ner" & Genotype=="17" | Species=="A.ner" & Genotype=="33" | Species=="A.ner" & Genotype=="35" | Species=="A.ner" & Genotype=="47" )



################################################################################
#####  2. model average growth of reduced dataset of all species

### specify structure of data using groupedData()
attach(d.aph2)
model.data.2<-groupedData(Log.sum ~ Growth.days | ID, outer=~Genotype+Chamber+Species, inner=~Census)
detach(d.aph2)

### build model using nlsList()
test.fit2 <- nlsList(Log.sum ~ loglin1(Growth.days, M0, r), start=c(M0=log(5), r=0.2), model.data.2)

### model fitting using nlme
lin2 <- nlme(test.fit2,  control=myControl)



### specify weights term to estimate unique variances for each census
lin2.1 <- update(lin2, weights=varIdent(form=~1|Census))

anova(lin2, lin2.1) #--> large model improvement

### specify fixed effects of Species
lin2.3 <- update(lin2.1, fixed=list(M0 ~ Species, r ~ Species),  start=c(fixef(lin2.1)[1],0,0,0,fixef(lin2.1)[2],0,0,0))


#### 2.1 extract data for figure 2a
### use estimable() to generate means and SE for each level of Species

## extract slopes (= rgr)
r.avg<-data.frame(mean=numeric(4), se=numeric(4))
x<-c(0,0,0,0,1,0,0,0)

r.avg[1,]<-as.numeric(estimable(lin2.3, cm=x)[1:2])
for(i in 2:4){
x[4+i]<-1
r.avg[i,]<-as.numeric(estimable(lin2.3, cm=x)[1:2])
x[4+i]<-0}

##  extract intercept (= estimated initial population size)
m0.avg<-data.frame(mean=numeric(4), se=numeric(4))
x<-c(1,0,0,0,0,0,0,0)

m0.avg[1,]<-as.numeric(estimable(lin2.3, cm=x)[1:2])
for(i in 2:4){
x[i]<-1
m0.avg[i,]<-as.numeric(estimable(lin2.3, cm=x)[1:2])
x[i]<-0}

### back-transform values of initial population size to normal scale
exp(m0.avg$mean)



#### 2.2 calculate population doubling time

### mean time to double population size
log(2)/r.avg$mean

### upper and lower SEs
(log(2)/r.avg$mean)  - (log(2)/(r.avg$mean+r.avg$se))
(log(2)/r.avg$mean)  - (log(2)/(r.avg$mean-r.avg$se))


################################################################################
##### 3. Calculate variance explained by Genotype

### extract mean + random deviate from model with no fixed effects
RGR.r<-fixef(lin2.1)[2]+ranef(lin2.1)[,2]

### create data frame containing RGR.r and identifiers
Gen<-as.numeric(unlist(strsplit(rownames(ranef(lin2.1)), "_"))[seq(1,575,2)])
Rep<-as.numeric(unlist(strsplit(rownames(ranef(lin2.1)), "_"))[seq(2,576,2)])
dat<-data.frame(RGR.r, Genotype=Gen, Replicate=Rep, ID=rownames(ranef(lin2.1)))
dat<-dat[order(dat$Genotype, dat$Replicate),]
dat$Genotype   <-as.factor(dat$Genotype)
for(i in 1:length(dat$ID))
dat$Species[i]<-as.character(d.aph2$Species[d.aph2$ID==as.character(dat$ID[i])][1])


### perform variance partitioning in lme4
library(lme4)

### subset data to look at Species level
dat.An<-subset(dat, Species=="A.ner")
m.An<-lmer(RGR.r ~ (1|Genotype), dat.An)
data.frame(summary(m.An)$varcor)[1,4] /  sum(data.frame(summary(m.An)$varcor)[c(1,2),4])
## 15.4 %


dat.Aa<-subset(dat, Species=="A.asc")
m.Aa<-lmer(RGR.r ~ (1|Genotype), dat.Aa)
data.frame(summary(m.Aa)$varcor)[1,4] /  sum(data.frame(summary(m.Aa)$varcor)[c(1,2),4])
## 0 %


dat.Ma<-subset(dat, Species=="M.asc")
m.Ma<-lmer(RGR.r ~ (1|Genotype), dat.Ma)
data.frame(summary(m.Ma)$varcor)[1,4] /  sum(data.frame(summary(m.Ma)$varcor)[c(1,2),4])
## 9.8 %


dat.Mp<-subset(dat, Species=="M.per")
m.Mp<-lmer(RGR.r ~ (1|Genotype), dat.Mp)
data.frame(summary(m.Mp)$varcor)[1,4] /  sum(data.frame(summary(m.Mp)$varcor)[c(1,2),4])
## 11.7 %


detach(package:lme4)

################################################################################
##### 4. fit fixed effects of Genotype for three species

#### Aphis nerii
model.data.an<-subset(model.data.2, Species=="A.ner")

###  build model using nlsList()
test.fit.an <- nlsList(Log.sum ~ loglin1(Growth.days, M0, r), start=c(M0=2.5, r=0.2), model.data.an)

### model fitting using nlme
lin.an1 <- nlme(test.fit.an,  control=myControl)

### specify weights term to estimate unique variances for each census
lin.an2 <- update(lin.an1, weights=varIdent(form=~1|Census))
anova(lin.an1, lin.an2)


### compare models with different fixed effects on m0 and r
lin.an3 <- update(lin.an2, fixed=list(M0 ~ Genotype, r ~ Genotype), start=c(fixef(lin.an2)[1],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                                            fixef(lin.an2)[2],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
lin.an4 <- update(lin.an2, fixed=list(M0 ~ Genotype, r ~ 1),        start=c(fixef(lin.an1)[2],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                                            fixef(lin.an1)[2]))
lin.an5 <- update(lin.an2, fixed=list(M0 ~ 1, r ~ Genotype),        start=c(fixef(lin.an2)[1],
                                                                            fixef(lin.an2)[2],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

anova(lin.an3)
anova(lin.an4)
anova(lin.an5)
## r.Genotype always significant

AIC(lin.an3, lin.an4, lin.an5)
## lin.an5 best model with genotype effect


#### Myzocallis asclepiadis
model.data.ma<-subset(model.data.2, Species=="M.asc")

###  build model using nlsList()
test.fit.ma <- nlsList(Log.sum ~ loglin1(Growth.days, M0, r), start=c(M0=2.5, r=0.2), model.data.ma)

### model fitting using nlme
lin.ma1 <- nlme(test.fit.ma,  control=myControl)

### specify weights term to estimate unique variances for each census
lin.ma2 <- update(lin.ma1, weights=varIdent(form=~1|Census))
anova(lin.ma1, lin.ma2)


### compare models with different fixed effects on m0 and r
lin.ma3 <- update(lin.ma2, fixed=list(M0 ~ Genotype, r ~ Genotype), start=c(fixef(lin.ma2)[1],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                                            fixef(lin.ma2)[2],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
lin.ma4 <- update(lin.ma2, fixed=list(M0 ~ Genotype, r ~ 1),        start=c(fixef(lin.ma2)[1],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                                            fixef(lin.ma2)[2]))
lin.ma5 <- update(lin.ma2, fixed=list(M0 ~ 1, r ~ Genotype),        start=c(fixef(lin.ma2)[1],
                                                                            fixef(lin.ma2)[2],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

anova(lin.ma3)
anova(lin.ma4)
anova(lin.ma5)
## r.Genotype significant in ma5

AIC(lin.ma3, lin.ma4, lin.ma5)
## lin.ma5 best model with genotype effect


#### Myzus persicae
model.data.mp<-subset(model.data.2, Species=="M.per")

###  build model using nlsList()
test.fit.mp <- nlsList(Log.sum ~ loglin1(Growth.days, M0, r), start=c(M0=2.5, r=0.2), model.data.mp)

### model fitting using nlme
lin.mp1 <- nlme(test.fit.mp,  control=myControl)

### specify weights term to estimate unique variances for each census
lin.mp2 <- update(lin.mp1, weights=varIdent(form=~1|Census))
anova(lin.mp1, lin.mp2)

### compare models with different fixed effects on m0 and r
lin.mp3 <- update(lin.mp2, fixed=list(M0 ~ Genotype, r ~ Genotype), start=c(fixef(lin.mp2)[1],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                                            fixef(lin.mp2)[2],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
lin.mp4 <- update(lin.mp2, fixed=list(M0 ~ Genotype, r ~ 1),        start=c(fixef(lin.mp2)[1],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                                            fixef(lin.mp2)[2]))
lin.mp5 <- update(lin.mp2, fixed=list(M0 ~ 1, r ~ Genotype),        start=c(fixef(lin.mp2)[1],
                                                                            fixef(lin.mp2)[2],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))


anova(lin.mp3)
anova(lin.mp4)
anova(lin.mp5)
## r.Genotype always significant

AIC(lin.mp3, lin.mp4, lin.mp5)
## lin.mp5 best model with genotype effect



#### double check Aphis asclepiadis
model.data.aa<-subset(model.data.2, Species=="A.asc")

###  build model using nlsList()
test.fit.aa <- nlsList(Log.sum ~ loglin1(Growth.days, M0, r), start=c(M0=2.5, r=0.2), model.data.aa)

### model fitting using nlme
lin.aa1 <- nlme(test.fit.aa,  control=myControl)

### specify weights term to estimate unique variances for each census
lin.aa2 <- update(lin.aa1, weights=varIdent(form=~1|Census))
anova(lin.aa1, lin.aa2)

### compare models with different fixed effects on m0 and r
lin.aa3 <- update(lin.aa2, fixed=list(M0 ~ Genotype, r ~ Genotype), start=c(fixef(lin.aa2)[1],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                                            fixef(lin.aa2)[2],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

lin.aa4 <- update(lin.aa2, fixed=list(M0 ~ Genotype, r ~ 1),        start=c(fixef(lin.aa2)[1],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                                            fixef(lin.aa2)[2]))

lin.aa5 <- update(lin.aa2, fixed=list(M0 ~ 1, r ~ Genotype),        start=c(fixef(lin.aa2)[1],
                                                                            fixef(lin.aa2)[2],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

anova(lin.aa3)
anova(lin.aa4)
anova(lin.aa5)
## r.Genotype never significant


### get estimates with se's using estimable()
x<-c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

rgr.an<-data.frame(gen=numeric(20), se=numeric(20))
rgr.an[1,]<-as.numeric(estimable(lin.an5, cm=x)[1:2])
for(i in 2:20){
x[1+i]<-1
rgr.an[i,]<-as.numeric(estimable(lin.an5, cm=x)[1:2])
x[1+i]<-0}

rgr.ma<-data.frame(gen=numeric(20), se=numeric(20))
rgr.ma[1,]<-as.numeric(estimable(lin.ma5, cm=x)[1:2])
for(i in 2:20){
x[1+i]<-1
rgr.ma[i,]<-as.numeric(estimable(lin.ma5, cm=x)[1:2])
x[1+i]<-0}

rgr.mp<-data.frame(gen=numeric(20), se=numeric(20))
rgr.mp[1,]<-as.numeric(estimable(lin.mp5, cm=x)[1:2])
for(i in 2:20){
x[1+i]<-1
rgr.mp[i,]<-as.numeric(estimable(lin.mp5, cm=x)[1:2])
x[1+i]<-0}



# calculate population doubling time and se

rgr.an$DT<-log(2)/rgr.an$gen         #
rgr.an$DT.se<- (((log(2)/rgr.an$gen)  - (log(2)/(rgr.an$gen+rgr.an$se))) + abs((log(2)/rgr.an$gen)  - (log(2)/(rgr.an$gen-rgr.an$se))))/2

rgr.ma$DT<-log(2)/rgr.ma$gen         #
rgr.ma$DT.se<- (((log(2)/rgr.ma$gen)  - (log(2)/(rgr.ma$gen+rgr.ma$se))) + abs((log(2)/rgr.ma$gen)  - (log(2)/(rgr.ma$gen-rgr.ma$se))))/2

rgr.mp$DT<-log(2)/rgr.mp$gen         #
rgr.mp$DT.se<- (((log(2)/rgr.mp$gen)  - (log(2)/(rgr.mp$gen+rgr.mp$se))) + abs((log(2)/rgr.mp$gen)  - (log(2)/(rgr.mp$gen-rgr.mp$se))))/2



################################################################################
############################## Plant growth ####################################
################################################################################

## define numeric Genotype identifier as factor
d.pl$Genotype<-as.factor(d.pl$Genotype)

## log-transform plant height
d.pl$Log.height<-log(d.pl$Height)


### remove replicates with problematic growth patterns
# M.asc : 17
d.pl<-subset(d.pl, ID!="1_07" & ID!="3_05" & ID!="5_08" & ID!="11_01" & ID!="11_05" & ID!="13_05" & ID!="13_10" & ID!="17_04" & ID!="17_08" & ID!="23_01" & ID!="27_15" & ID!="31_17" & ID!="33_01" & ID!="41_05" & ID!="43_14" & ID!="45_14" & ID!="45_15")
# A.asc : 11
d.pl<-subset(d.pl, ID!="7_15" & ID!="11_08" & ID!="23_04" & ID!="25_02" & ID!="27_04" & ID!="27_16" & ID!="33_10" & ID!="35_13" & ID!="43_09" & ID!="47_08" & ID!="47_09")
# M.per : 1
d.pl<-subset(d.pl, ID!="45_13")


### remove NAs
d.pl<-subset(d.pl, Height!="NA" & Species!="NA")


## restrict data to first five census points
d.pl2<-subset(d.pl, Census<6 )


### specify structure of data using groupedData()
attach(d.pl2)
model.data.3<-groupedData(Log.height ~ Age | ID, outer=~Genotype, inner=~Chamber + Census)
detach(d.pl2)

### build model using nlsList() and built in self-start function SSasymp
test.fit3 <- nlsList(Log.height ~ SSasymp(Age, Asym, m0, lrc), model.data.3)
##--> warning message can be ignored, as nlsList is overfitting the data

### fit model in nlme
asym1<- nlme(test.fit3, control=myControl)

### specify weights term to estimate unique variances for each census
asym2 <- update(asym1, weights=varIdent(form=~1|Census))
anova(asym1, asym2)


### specify fixed effects
asym3 <- update(asym2, fixed=list(Asym ~ Genotype, m0 ~ Genotype, lrc ~ Genotype), start=c(fixef(asym2)[1],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                                                           fixef(asym2)[2],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                                                           fixef(asym2)[3],0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

anova(asym3)
##--> all genotype effects are significant

### extract parameters
K<-    c(fixef(asym3)[1],  fixef(asym3)[1] +fixef(asym3)[2:20])
m0<-   c(fixef(asym3)[21], fixef(asym3)[21]+fixef(asym3)[22:40])
r<-exp(c(fixef(asym3)[41], fixef(asym3)[41]+fixef(asym3)[42:60]))

### calculate average plant age at aphid introduction
age.aph<-tapply(d.pl$Age.aph.int[d.pl$Census==4], list(d.pl$Genotype[d.pl$Census==4]), mean, na.rm=T)


### calculate RGR at time t
RGRt <- r*exp(-r*age.aph)*(K-m0)

### calculate AGR
AGR<-   RGRt * exp(K-(exp(-r*age.aph)*(K-m0)))

################################################################################
############################ Honeydew exudation ################################
################################################################################

### calculate honeydew weight per aphid
d.hd$hd<-d.hd$Honeydew.mass / d.hd$Cumulative.aphids

## exclude NAs
d.hd<-subset(d.hd, hd!="NA")


m.hd1<-lm(log(hd) ~ Species, d.hd)

hd.avg<-data.frame(mean.log=numeric(4), se.log=numeric(4))
x<-c(1,0,0,0)

hd.avg[1,]<-as.numeric(estimable(m.hd1, cm=x)[1:2])
for(i in 2:4){
x[0+i]<-1
hd.avg[i,]<-as.numeric(estimable(m.hd1, cm=x)[1:2])
x[0+i]<-0}

### back-transform values
hd.avg$mean.n<-exp(hd.avg$mean.log)
hd.avg$se.l  <-exp(hd.avg$mean.log-hd.avg$se.log)
hd.avg$se.u  <-exp(hd.avg$mean.log+hd.avg$se.log)



################################################################################
############################# Chemistry means ##################################
################################################################################

## define numeric Genotype identifier as factor
d.che$Genotype<-as.factor(d.che$Genotype)

### sum all cardenolides
d.che$total <- rowSums(d.che[,c(8:39)])

### remove NAs
d.che<- subset(d.che, total!="NA")


##### Plant cardenolides
d.carP<-subset(d.che, Sample.type=="Pl")


### sum polar and apolar cardenolides
d.carP$apol <- rowSums(d.carP[,c(33:39)])
##--> six least polar cardenolides, as c11.8 is absent in plants

d.carP$pola <- rowSums(d.carP[,c(8:16)])
##--> four most polar cardenolides present in plants

### Genotype means for total cardenolides
m.carP1<-gls(total ~ Genotype, weights=varIdent(form=~1|Genotype), d.carP)
Pl.cardT<- c( coef(m.carP1)[1],  coef(m.carP1)[1]+ coef(m.carP1)[2:20])


### full-sib heritability for total cardenolides
m.carP2 <- lme(total ~ 1, random=~1|Genotype , d.carP)
m.carP3 <- lme(total ~ 1, random=~1|Genotype , weights=varIdent(form=~1|Genotype), d.carP)
anova(m.carP2, m.carP3)

(as.numeric(VarCorr(m.carP3)[1,1])*2) /  sum(as.numeric(VarCorr(m.carP3)[c(1:2),1]))


### Genotype means for apolar and polar cardenolides
m.carP4<-gls(apol ~ Genotype, weights=varIdent(form=~1|Genotype), d.carP)
Pl.cardAp<- c( coef(m.carP4)[1],  coef(m.carP4)[1]+ coef(m.carP4)[2:20])

m.carP5<-gls(pola ~ Genotype, weights=varIdent(form=~1|Genotype), d.carP)
Pl.cardPo<- c( coef(m.carP5)[1],  coef(m.carP5)[1]+ coef(m.carP5)[2:20])


##### Aphid cardenolides
d.carA<-subset(d.che, Sample.type=="Aph")

### Genotype means for total cardenolides
m.carA1<-gls(total ~ Genotype*Species, weights=varIdent(form=~1|Genotype), d.carA)

Aaa.cardT<- c( coef(m.carA1)[1],                   coef(m.carA1)[1]+                  coef(m.carA1)[2:20])
Aan.cardT<- c( coef(m.carA1)[1]+coef(m.carA1)[21], coef(m.carA1)[1]+coef(m.carA1)[21]+coef(m.carA1)[2:20]+coef(m.carA1)[24:42])
Ama.cardT<- c( coef(m.carA1)[1]+coef(m.carA1)[22], coef(m.carA1)[1]+coef(m.carA1)[22]+coef(m.carA1)[2:20]+coef(m.carA1)[43:61])
Amp.cardT<- c( coef(m.carA1)[1]+coef(m.carA1)[23], coef(m.carA1)[1]+coef(m.carA1)[23]+coef(m.carA1)[2:20]+coef(m.carA1)[62:80])


##### Honeydew cardenolides
d.carH<-subset(d.che, Sample.type=="Hd")

### Genotype means for total cardenolides
m.carH1<-gls(total ~ Genotype*Species, weights=varIdent(form=~1|Genotype), d.carH)

Haa.cardT<- c( coef(m.carH1)[1],                   coef(m.carH1)[1]+                  coef(m.carH1)[2:20])
Han.cardT<- c( coef(m.carH1)[1]+coef(m.carH1)[21], coef(m.carH1)[1]+coef(m.carH1)[21]+coef(m.carH1)[2:20]+coef(m.carH1)[24:42])
Hma.cardT<- c( coef(m.carH1)[1]+coef(m.carH1)[22], coef(m.carH1)[1]+coef(m.carH1)[22]+coef(m.carH1)[2:20]+coef(m.carH1)[43:61])
Hmp.cardT<- c( coef(m.carH1)[1]+coef(m.carH1)[23], coef(m.carH1)[1]+coef(m.carH1)[23]+coef(m.carH1)[2:20]+coef(m.carH1)[62:80])



##### C:N ratio
d.cn<-subset(d.che, C.N!="NA")


m.cn1<-gls(C.N ~ Genotype, d.cn)
C.N<- c( coef(m.cn1)[1],  coef(m.cn1)[1]+ coef(m.cn1)[2:20])


#### full-sib heritability for C:N ratio
m.cn2<-lme(C.N ~ 1, random=~1|Genotype ,d.cn)
m.cn3<-lme(C.N ~ 1, random=~1|Genotype , weights=varIdent(form=~1|Genotype), d.cn)
AIC(m.cn2, m.cn3)

(as.numeric(VarCorr(m.cn2)[1,1])*2) /  sum(as.numeric(VarCorr(m.cn2)[c(1:2),1]))




################################################################################
############################# Trait correlations ###############################
################################################################################

## define numeric Genotype identifier as factor
d.gen$Genotype<-as.factor(d.gen$Genotype)


##### Aphis nerii
### full model
m.an1<-lm(RGR.an ~  C.N + Plant.AGR + Card.tot + Card.apo + Card.pol, d.gen)
drop1(m.an1,~.,test="F")

### final model
m.an2<-lm(RGR.an ~  Plant.AGR, d.gen)
drop1(m.an2,~.,test="F")



##### Myzus persicae
### full model
m.mp1<-lm(RGR.mp ~  C.N + Plant.AGR + Card.tot + Card.apo + Card.pol, d.gen)
drop1(m.mp1,~.,test="F")

### final model
m.mp2<-lm(RGR.mp ~  Plant.AGR, d.gen)
drop1(m.mp2,~.,test="F")


##### Myzocallis asclepiadis
### full model
m.ma1<-lm(RGR.ma ~  C.N + Plant.AGR + Card.tot + Card.apo + Card.pol, d.gen)
drop1(m.ma1,~.,test="F")

### final model
m.ma2<-lm(RGR.ma ~  Card.apo, d.gen)
drop1(m.ma2,~.,test="F")

