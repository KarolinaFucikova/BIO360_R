# let's jump right in and import some data into R
# if you organized your project exactly, we won't have to look for them
# they are in the folder called 'data', right here in the same folder where the R project is
BIO360_seeds <- read.csv("data/seeds.csv")
BIO360_seeds
head(BIO360_seeds)

# you can click on the seeds object in the environment quadrant on the right
# to see what it looks like in spreadsheet form
# R is smart and will assume column names are in the first row - see that?

# let's try some different data visualization/exploration graphs)
boxplot(BIO360_seeds$maple)
boxplot(BIO360_seeds)
# what do you think the $ does in BIO360_seeds$maple ?
# what happened when you ran the line?
# we will learn more graphing tricks later; now try:

hist(BIO360_seeds$elm2)
# now make a histogram for each of the remaining columns
# do the data look normally distributed?

# calculate means, variances and standard deviations for the four columns:
var(BIO360_seeds$maple,na.rm=TRUE)
# the na.rm=TRUE ensures that R will ignore missing values
# calculate variances for the remaining columns
# standard deviations can be calculated as sd() and means as mean()
# calculate sd and mean for each column

# note that there are many data files in the data folder
# you will build a set of scripts for various analyses in the coming weeks


############################################################################
# let's try to look at the first tab of the Talal supplementary Table 1
# it is saved in your data folder as Talal_plant_comm_comp.csv
# import it into R by completing the next line:
BIO360_Talal <- read.csv("data/Talal_plant_comm_comp.csv")

# One thing that R does not handle well is extra columns and rows that do not contain data
# which is why we will learn to subset the data first
# only rows 2-17 contain data
BIO360_Talal <- BIO360_Talal[2:17,] # you can even comment on the same line!
# let's talk about what the line above means - take notes as comments!

# the column names are not right because of how the original table was set up
# let's fix that now; we want the first row to become the column names
colnames(BIO360_Talal) <- BIO360_Talal[1,]
# ugh but now there's still a problem - you know how to fix it though. Give it a try!

# and how about making the first column into row names?


# and subsequently removing the extra first column?
# trick to make it easier if you don't know how many columns there are:
BIO360_Talal <- BIO360_Talal[,-1]

# big lesson here - this work could have been avoided if the csv file had been 
# formatted correctly in the first place!

# try to calculate the sum of the first column
# you will get an error, which has to do with the data type
# R does not recognize the numbers as numbers - that sometimes happens
# this time it is because the columns and rows were not set up right
# R probably assumed that all cells contained letter characters rather than numbers

# this time, the fix is not as easy, and it will require us to install a package
# we'll install several packages this semester; might as well start now
install.packages("dplyr")

# you will not need to run the above line again; once you have it, you have it
# you will, however, have to load the package every time you start R:
library("dplyr")
BIO360_Talal <- mutate_all(BIO360_Talal, function(x) as.numeric(as.character(x)))
# no need to remember the above line - hopefully you will format your data better!

# now try to calculate the sum for the first column

# in fact we can do sums for all columns and all sums:
colSums(BIO360_Talal)
rowSums(BIO360_Talal)
# what did these sums tell us about the species and parks in the data set?

# let's install the package called "vegan" - we will need it later in the semester
# once you've installed it, load it

# vegan knows all sorts of ecological tricks, such as calculating species richness
# and diversity indices
specnumber(BIO360_Talal)
diversity(BIO360_Talal, index="shannon")
# now try the Simpson index!

# note that this wouldn't work if species were in rows and parks in columns
# but we could easily fix it with a nifty transpose function, which is just 
# t(BIO360_Talal)
# further notes on what vegan can do: https://rdrr.io/cran/vegan/

# it would be nice if we knew which park had which kind of use
# those data are in Talal_envdata.csv
# go ahead and import the data as Talal_env
Talal_env <- 

# make the first column into row names


# there are many ways to analyze these data; we'll try a simple way now
# we'll start by making a new table (data frame) with our use type and diversity measures for each park
# call this table just "parks"
# below, describe what each line does (try them out first, of course!)

parks <- Talal_env
richness <- specnumber(BIO360_Talal)
shannon <- diversity(BIO360_Talal, index="shannon")
simpson <- diversity(BIO360_Talal, index="simpson")

parks$richness <- richness
parks$shannon <- shannon
parks$simpson <- simpson

# messed something up? NO BIGGIE
# go back a few lines, re-import data, correct and rerun the lines

# now for some analyses, this line performs an analysis of variance:
Talal_aov <- aov(richness ~ Park.Type., data = parks)
summary(Talal_aov)

# plot the results by MODIFYING the following line:
boxplot(response_variable ~ independent_variable, data=data_object)
# make it colorful
boxplot(response_variable ~ independent_variable, data=data_object, col=c("orange", "navy", "turquoise"))
# change colors any way you like and see what happens!
# save one of your plots into your 'plots' subfolder

# try the plotting for shannon and simpson index too!
# that will be enough for one day - review your notes, organize them, 
# and save all your files in the proper places