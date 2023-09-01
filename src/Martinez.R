birds <- read.csv("data/Shorebirds_PreyIntake.csv", sep=';')
inverts <-read.csv("data/Benthic_AbundanceAndBiomass.csv", sep=';')
head(birds)
head(inverts)

boxplot(PreyPerMinute~Treatment*Period, data=birds)
boxplot(MeanLenghtPreyMinute~Treatment*Period, data=birds)

boxplot(Total_Abundance~Treatment*Period, data=inverts)
boxplot(Total_Biomass~Treatment*Period, data=inverts)
boxplot(Polychaeta_Biomass~Treatment*Period, data=inverts)
