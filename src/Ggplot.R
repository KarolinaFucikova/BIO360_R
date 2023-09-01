# load the following packages;  you may need to install them first:
library("ggplot2")
library("dplyr")
library("ggpubr")
# first, import the invert.csv file and make a simple plot
# of river flow velocity (x axis) and invertebrate abundance (y axis)

# now modify the following line to input your data frame
# and the two variables on the correct axes:
ggplot(data = inv, aes(x = predictor, y = response)) +
  geom_point()

# add some elements to your graph
ggplot(data = inv, aes(x = predictor, y = response)) +
         geom_point(size=5) + geom_smooth(method=lm)
# kind of nice, isn't it?
# the geom_smooth adds a 95% confidence interval "envelope" around the trendline
# which visually tells us where else that line could reasonably be, based on the data

# you can try your hand at slightly more complex data - download it from Brightspace first
GCcontent <- read.csv("data/GC_content.csv")
GCclean <- na.omit(GCcontent[,c(2,3,6)]) # what does this line do?

# there are data for three kinds of algae - some from water, some from snow and some from dry land
# each will be plotted with a different symbol and color
# for each species we are graphing the temperature of its site of origin and the GC content in its ribosomal DNA genes
ggplot(GCclean, aes(x=max_temp, y=SSU_GC, color=habitat)) + 
  geom_point(size=5, aes(shape=habitat)) 
# quite informative, right? 
# ugly colors though. I mean... oof. 

# some previous data - boxplots first
sward2 <- read.csv("data/sward2.csv")
head(sward2)
ggplot(data = sward2, aes(x = Site, y = Height)) +
  geom_boxplot()
# ad elements one by one
ggplot(data = sward2, aes(x = Site, y = Height, fill = Site)) +
  geom_boxplot() +
  scale_fill_grey() + # add if you want to be colorblind-friendly; other palettes exist
  geom_jitter(alpha = 0.5, color = "tomato") # add data points to show distribution and sample size

# bar plot of the same data
# error bars are actually not trivial, but a new package will help us

ggbarplot(
  sward2, x = "Site", y = "Height", 
  add = c("mean_se", "jitter"), # adds standard error bars as well as data points
  fill = "#BF504D"
)

ggbarplot(
  sward2, x = "Site", y = "Height", 
  add = c("mean_se", "jitter"), 
  color = "Site", palette = c("#807F7F", "#BF504D", "black"),
  position = position_dodge(0.8)
)

# Change bars fill color by groups
# You need to change also point shapes by groups
ggbarplot(
  sward2, x = "Site", y = "Height", 
  add = c("mean_se", "jitter"), 
  add.params = list(shape = "Site"),
  fill= "Site", palette = c("#807F7F", "#BF504D", "black"),
  position = position_dodge(0.8)
)

# a more complicated data set - sward 3
sward3 <- read.csv("data/sward3.csv")
head(sward3)
boxplot(growth~Water*species, data=sward3)

ggplot(data = sward3, aes(x = Water, y = growth)) +
  geom_boxplot()
# problem is - how do we indicate the two factors in ggplot?
# two ways - one with color, one by defining interaction of factors
ggplot(aes(x = Water, y = growth, fill=species), data=sward3) +
  geom_boxplot() 
# or
sward3$f1f2 <- interaction(sward3$Water, sward3$species)
ggplot(data=sward3, aes(x=f1f2, y=growth)) + 
  geom_boxplot() 

# adding more elements (can work for either method):
ggplot(data=sward3, aes(x=f1f2, y=growth, fill=species)) + 
  geom_boxplot() +
  geom_jitter(alpha = 0.5, color = "black")

# bar plot
ggbarplot(
  sward3, x = "Water", y = "growth", 
  add = c("mean_se", "jitter"), 
  add.params = list(shape = "species"),
  fill= "species", palette = c("#807F7F", "#BF504D"),
  position = position_dodge(0.8)) 


# try to recreate the Kalske plots for number of leaves (G.leaves3)
Kalske <- read.csv("data/Kalske_plants.csv")
head(Kalske)

ggbarplot(
  Kalske, x = "inoc", y = "G.leaves3", 
  add = c("mean_ci", "jitter"), 
  color = "origin",
  position = position_dodge(0.8)) 

ggbarplot(
  Kalske, x = "inoc", y = "G.leaves3", 
  add = "mean_ci", 
  add = "jitter",
  color = "origin",
  position = position_dodge(0.8)) 


############################
# some exploration of Martinez-Curci data
#  using palettes
library(RColorBrewer)
display.brewer.all(colorblindFriendly = T)

birds <- read.csv("data/Shorebirds_PreyIntake.csv", sep=';')
head(birds)
birds$Period<-as.factor(birds$Period)

boxplot(PreyPerMinute~Treatment*Period, data=birds)
ggplot(aes(y=PreyPerMinute, x=Treatment, fill=Period), data=birds) +
  geom_violin() 
ggplot(aes(y=PreyPerMinute, x=Treatment, fill=Period), data=birds) +
  geom_boxplot() 


birds$f1f2 <- interaction(birds$Treatment, birds$Period)
ggplot(data=birds, aes(x=f1f2, y=MeanLenghtPreyMinute)) + geom_boxplot()
  
ggplot(data = birds, aes(x = f1f2, y = PreyPerMinute)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.5, color = "tomato")

ggline(inverts, x = "Period", y = "Total_Biomass", color = "Treatment",
       add = c("mean_se", "jitter"))

# https://www.datanovia.com/en/blog/how-to-easily-create-barplots-with-error-bars-in-r/
# https://rpkgs.datanovia.com/ggpubr/reference/ggbarplot.html

