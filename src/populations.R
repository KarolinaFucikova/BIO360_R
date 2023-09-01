# starting with population demography: survivorship curves
# read in sheep_survivorship.csv as 'sheep'

#plot is as is, number of dying population members per time period
plot(Age_interval~Number_dying, data=sheep)
# or
plot(sheep$Age_interval~sheep$Number_dying)
# see any difference between the plots?
# we actually want the axes reversed - go ahead and plot it that way

# create a new column called Number_surviving
# in it we will want 1000-the number of dying sheep
# we'll be one row off but we'll deal with that later
sheep$Number_surviving <- 1000-sheep$Number_dying
# what did that do?

# we could solve this in R, but right now, it will be easier to do in Excel
# go ahead and calculate the column in the csv file in Excel, then re-import the table into R

# now plot survival against Age_interval

# neat trick to plot on log axes: add log="x" or log="y" into the parentheses

# save your plot in the plots sub-folder


####################################
# MODELING POPULATION DYNAMICS

# lucky us, R can simulate data

N0 = 100
lambda = 1.5
t = 0:10
N = N0 * lambda^t
round(N, 0)

# what do you think these lines did?
# at this point you know how to make a scatter plot with linear and log axes
# this way is a little different; try it:
plot(t, N)
# turns out comma will do the same thing as ~ sometimes!

# let's try altering the graph a little:
plot(t, N, type = "o", pch = 19, las = 1)
# what changed?
# spend some time playing with these new arguments to see what they do

# log-transform the y axis

# changing the growth rate
N0 = 100
lambda = seq(0.6, 1.4, 0.2)
t = 0:10
N = sapply(lambda, function(lambda) N0 * lambda^t)
N

# what happened? What did the change in code do?

# next, try this:
matplot(t, N, las = 1)

# what did that do?

# next, make a set of five colors for yourself. CHANGE the example below
# so that you end up with your own unique color sheme in the figure
colors <- c("blue", "gray", "orange", "turquoise", "green")
# you can ask R for names of colors like so:
colors()
# more about colors in R: https://www.datamentor.io/r-programming/color

# then, try plotting again; change the code below to plot on a log y axis:
matplot(t, N, type = "o", las = 1, pch = 1:5, col = colors, lty = 1:5, 
        yaxt = "n")

#############################
# logistic growth
#Create some simulated population data, of let's say newly introduced fox population
t = (seq(1900,1920,1)) 
N = (sort(round((runif(21,200,2489)),0), decreasing = F) )
pop<- as.data.frame(cbind(t,N)) 
head(pop)
# annotate your code to explain what each line does
# describe the curve


####################################
# next, let's try some clean, formula-following data
# logistic growth is a little more complicated, so the script will be too
# no need to remember how to do this; it is definitely advanced

#Set number of generations to plot
PlotGen <- 200

###Set parameters for simulation 1###
# set initial population size
N0 <- 10
#Set R
R <- 1.7
#Set K
K <- 300

###Run simulation###
# initialize vector to hold results 
PopSize <- N0 
# create variable to hold the current population size
PopNow <- N0 

# calculate population sizes and append to popsize
for(i in 1:PlotGen) { 
  PopNow <- PopNow + PopNow*R*(1-PopNow/K)    #discrete logistic
  if (PopNow < 0) {PopNow <- 0}
  PopSize <- c(PopSize,PopNow)                #add result to vector
}

###Plot results###
tvals <- 1:length(PopSize) #create a vector of time values equal to length of results vector

plot(tvals[1:PlotGen], PopSize[1:PlotGen],type="o",col="red", 
     xlab="Generation",ylab="Population size",pch=16,cex=.75)

