####### Witman & Lamb 2017 #######

## -- This is a script for analyzing and visualizing data from Witman & Lamb 2017. Download of this script along with the associated data files from Dryad will reproduce all analyses carried out in R for the manuscript. All figures and analyses appear in the order they appear in the article, with corresponding figure and table numbers -- ##

### Persistent differences between coastal and offshore kelp forest communities in a warming Gulf of Maine ###

# Set working directory

setwd("/Users/robertlamb/Desktop/GOM_R/")

# Load all necessary libraries #

library(ncdf4)
library(raster)
library(xts)
library(caTools)
library(ggseas)
library(ggplot2)
library(dplyr)
library(readr)
library(readxl)
library(vegan)
library(forcats)
library(tidyr)
library(lubridate)

# Bring in all data files

# struc is the table of percent cover values for benthic community organisms including all sessile invertebrate and macroalgae species #

struc=read.csv("data/GOM_benthic_full.csv", header=T, sep=",") %>%
  # Combine duplicate categories and remove excess
  mutate(`Botrylloides.violaceous` = (Unknown.orange.ascidian + Bottryloides..orange.), Dendrobeania.murrayana = (Unknown.Bryozoan + Dendrobeania), Diplosoma.listeriatum = (Diplosoma+ Bottryloides..gray.), Halisarca.sp = (Unknown.Yellow.Sponge+ Halisarca)) %>%
  select(-Unknown.orange.ascidian, -Bottryloides..orange., -Unknown.Bryozoan, -Dendrobeania, -Diplosoma, -Bottryloides..gray., -Unknown.Yellow.Sponge, -Halisarca) %>%
  rename(Didemnum.vexillum = brown.Didemnum, Eudendrium.sp. = branched.hydroid, Lithothamnion.sp. = pink.coralline..bumpy, Boltenia.echinata = cactus.sea.squirt, Corallinaceae.sp. = pink.coralline..smooth, Balanus.sp. = Balanus.spp., Isodictya.deichmannae = Isodictya..sponge., Heterosiphonia.japonica = Heterosiphonia, Modiolus.sp. = Modiolus, Halichondria.sp. = Halichondria, Bugula.sp. = Bugula, Hildenbrandia.sp. = Hildenbrandia, Schizomavella.sp. = Schizomavella, Polysiphonia.sp. = Polysiphonia, Chondrus.crust = chondrus.mat, Parasmittina.sp = Parasmittina, Gonactinia.sp. = Gonactinia, Gracilaria.sp. = Gracilaria, Anomia.sp. = Anomia, Crepidula.sp. = Crepidula, Tonicella.sp. = Tonicella..chiton., Leucoselenia.sp. = Leucoselenia..sponge., Molgula.sp. = Molgula..tunicate..sea.grape.) %>%
  mutate(total_organic = sediment.diatom + Heterosiphonia.japonica + Euthora.cristata + Modiolus.sp. + Corallinaceae.sp. + Phycodrys.fimbriata + Ptilota.serrata + Palmaria.palmata + Chondrus.crispus + Balanus.sp. + Halichondria.sp. + Lithothamnion.sp. + Mytilus.edulis + Eudendrium.sp. + Aplidium.pallidum + Bugula.sp. + Hildenbrandia.sp. + Schizomavella.sp. + Polysiphonia.sp. + Lomentaria.orcadensis + Corallina.officinalis + Ulva.lactuca + Didemnum.albidum + Terebratulina + Dendrodoa.carnea + Chondrus.crust + Parasmittina.sp + Boltenia.echinata + Aplidium.constellatum + Gonactinia.sp. + Isodictya.deichmannae + Gracilaria.sp. + Anomia.sp. + Crepidula.sp. + Tonicella.sp. + Leucoselenia.sp. + Molgula.sp. + Didemnum.vexillum + Botrylloides.violaceous + Dendrobeania.murrayana + Diplosoma.listeriatum + Halisarca.sp)

#Get values for all organic benthic substrates into proportional cover out of total

struc[,-c(1:7)]=struc[,-c(1:7)]/struc$total_organic

# remove totals column

struc = struc[,-50]

# Call to the sst anomaly data. These were pulled from the Met Office Hadley Centre for Climate Science and Services

load("data/sst_anomalies.rda")

# Call to the 2012 temperature and wave height data

load("data/coastal_vs_cashes.rda")


#  Call to kelp density data

density <- read_csv(file="data/kelp_density.csv")

# Call to kelp biomass data

biomass <- read_csv(file="data/kelp_biomass.csv")

# Call to fish underwater visual census data

fish_transects <- read_csv(file="data/all_fish_transects.csv")

# Call to fish stationary video data

fish_videos <- read_excel("data/fishvid2.xlsx")

## Figure 1 was created in ArcGIS ##

### Table 1 ###

# Kelp density # of 1.0 m2 quadrats #

density_Table1 <- density %>%
  mutate(Region = fct_recode(Site, "Cashes Ledge" = "Ammen Rock 1",
                             "Cashes Ledge" = "Ammen Rock 2",
                             "Cashes Ledge" = "Ammen Rock 3",
                             "Coastal"      = "Lunging Island",
                             "Coastal"      = "Mingo Rock",
                             "Coastal"      = "Spout Shoal",
                             "Coastal"      = "Star Island",
                             "Coastal"      = "Duck Island")) %>%
  group_by(Region, Site, Year) %>%
  summarize(Density_No.1m2_quads = length(unique(Quadrat)))



# Kelp biomass # of 1.0 m2 quadrats #

biomass_Table1 <- biomass %>%
  mutate(Region = fct_recode(Site, "Cashes Ledge" = "Ammen Rock 1",
                             "Cashes Ledge" = "Ammen Rock 2",
                             "Cashes Ledge" = "Ammen Rock 3",
                             "Coastal"      = "Lunging Island",
                             "Coastal"      = "Mingo Rock",
                             "Coastal"      = "Spout Shoal",
                             "Coastal"      = "Star Island")) %>%
  group_by(Region, Site, Year) %>%
  filter(Species == "A. clathratum") %>%
  summarize(Biomass_No.1m2_quads = length(Biomass))

# Fish biomass # of 10x5 m transects #
fish_transect_Table1 = fish_transects %>%
  mutate(Region = fct_recode(Region, "Cashes Ledge" = "Offshore"))%>%
  mutate(Site = fct_recode(Site, "Lunging Island" = "Lunging Shoal"))  %>%
  group_by(Region, Site, Year, Transect, Block) %>%
  summarize(segments = length(unique(`Transect segment`))) %>%
  group_by(Region, Site, Year) %>%
  summarize(Fish_transects = sum(segments))

# Fish abundance # of 10 min video segments #

fish_video_Table1 = fish_videos %>%
  mutate(Region = fct_recode(Region, "Cashes Ledge" = "Offshore", "Coastal" = "Onshore")) %>%
  mutate(Site = fct_recode(Site, "Lunging Island" = "Lunging Shoal", "Mingo Rock" = "Mingo"))  %>%
  group_by(Region, Site, Year, Camera) %>%
  summarize(segments = length(unique(Segment))) %>%
  group_by(Region, Site, Year) %>%
  summarize(Fish_video_segments = sum(segments))
  

kelp_tables = merge(density_Table1, biomass_Table1, all = T)
                    
fish_tables = merge(fish_transect_Table1, fish_video_Table1, all = T) 

Table_1 = merge(kelp_tables, fish_tables, all = T)

Table_1[is.na(Table_1)] <- 0

Table_1

### Figure 2 ###

# subset gom

sst_anomalies_gom <- sst_anomalies %>%
  dplyr::filter(region == "gom")

# create figure

figure_2 <- ggplot(sst_anomalies_gom, aes(monthYear, anomaly)) +
  geom_hline(yintercept = 0, colour = "grey50", alpha = 0.7) +
  geom_point(size = 1, colour = "grey70") +
  # NOTE: the stat_rollapplyr code below is not functional on some computers, the geom_smooth function that is commented out below yields a similar rolling smoother line to show long-term temperature trends
  stat_rollapplyr(colour = "blue", 
                  width  = 24, 
                  FUN    = median) +
  #geom_smooth(method = "loess", color = "blue", span = .005) +
  theme_bw() +
  scale_y_continuous(breaks = seq(from = -2.5, to = 2.5, by = 0.5)) +
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y") +
  # ggtitle("SST Anomalies - Gulf of Maine") +
  ylab("SST anomaly (°C)") +
  xlab("") +
  theme(plot.title      = element_text(hjust = 0.5),
        axis.text.y     = element_text(size = 13),
        axis.text.x     = element_text(size = 13),
        axis.title.y    = element_text(size = 16),
        legend.position = "none") + 
  annotate("rect", xmin   = as.Date("1987-01-01"),
             xmax   = as.Date("1987-12-31"),
             ymin   = -Inf,
             ymax   = Inf,
             fill   = "#ec7014",
             alpha  = 0.2) +
  annotate("rect", xmin   = as.Date("2012-01-01"),
           xmax   = as.Date("2012-12-31"),
           ymin   = -Inf,
           ymax   = Inf,
           fill   = "#ec7014",
           alpha  = 0.2) +
  annotate("rect", xmin   = as.Date("2014-01-01"),   ## -- these coalesce into one block        ##
           xmax   = as.Date("2014-12-31"),   ##    leaving here to modify, if needed -- ##
           ymin   = -Inf,
           ymax   = Inf,
           fill   = "#ec7014",
           alpha  = 0.2) +
  annotate("rect", xmin   = as.Date("2015-01-01"),
           xmax   = as.Date("2015-12-31"),
           ymin   = -Inf,
           ymax   = Inf,
           fill   = "#ec7014",
           alpha  = 0.2) +
  annotate("rect", xmin   = as.Date("2016-01-01"),
           xmax   = as.Date("2016-12-31"),
           ymin   = -Inf,
           ymax   = Inf,
           fill   = "#ec7014",
           alpha  = 0.2)

figure_2                                                                                                                
### Supplemental Figure 1 ###

# create temperature plot
S1_figure <-
  coastal_vs_cashes %>%
  mutate(site = factor(site),
         site = fct_recode(site, `Cashes Ledge`        = "cashes",
                           `Western Maine Shelf` = "coastal")) %>%
  mutate(Year = year(timePeriod)) %>%
  dplyr::filter(Year > 2001,
                Year < 2017) %>%
  # dplyr::filter(groupCumsum != 899 & Year != 2006) %>%  ## -- need to fix this in
  dplyr::filter(significant_wave_height > 0) %>%
  ggplot(aes(timePeriod, temperature, group = groupCumsum)) +
  geom_line(aes(colour = site), # alpha = 0.7, 
            na.rm = FALSE) +        
  theme_bw() +
  facet_wrap(~ site, nrow = 2) +
  scale_colour_manual(values = c("#e6550d","#756bb1")) +
  scale_x_datetime(date_breaks = "2 years",
                   date_labels = "%Y") +
  ylab("Temperature (°C)") +
  xlab("Time") +
  theme(plot.title       = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text       = element_text(size = 12),
        axis.text        = element_text(size = 11),
        axis.title       = element_text(size = 14),
        legend.position  = "none")

S1_figure

### Supplemental Figure 2 ###

S2_figure <-
  coastal_vs_cashes %>%
  mutate(site = factor(site),
         site = fct_recode(site, `Cashes Ledge`        = "cashes",
                           `Western Maine Shelf` = "coastal")) %>%
  mutate(Year = year(timePeriod)) %>%
  dplyr::filter(Year == 2012) %>%
  # dplyr::filter(groupCumsum != 899 & Year != 2006) %>%  ## -- need to fix this in
  dplyr::filter(significant_wave_height > 0) %>%
  ggplot(aes(timePeriod, temperature, group = groupCumsum)) +
  geom_line(aes(colour = site),  #alpha = 0.7, 
            na.rm = FALSE) +        
  theme_bw() +
  # scale_colour_manual(values = c("grey20", "grey70")) +
  scale_colour_manual(values = c("#e6550d","#756bb1")) +           
  scale_x_datetime(date_breaks = "2 months",
                   date_labels = "%b-%y") +
  ylab("Temperature (°C)") +
  xlab("Time") +
  theme(plot.title       = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text       = element_text(size = 12),
        axis.text        = element_text(size = 11),
        axis.title       = element_text(size = 14),
        legend.position  = c(0.22, 0.875)
  )

S2_figure

### Supplemental Table 1 ###


# create list of years to summarise
sampling_years <- c(1987, 2012, 2014, 2015, 2016)

# shape data for summarising
S1_table <-
  coastal_vs_cashes %>%
  mutate(site = factor(site),
         site = fct_recode(site, `Cashes Ledge`        = "cashes",
                           `Western Maine Shelf` = "coastal")) %>%
  mutate(Year = year(timePeriod)) %>%
  dplyr::filter(Year %in% sampling_years) %>%
  group_by(site, Year) %>%
  summarise(nintyFive = quantile(temperature, probs = 0.95) %>% round(3),
            nintyNine = quantile(temperature, probs = 0.99) %>% round(3))

S1_table

### Supplemental Figure 3 ###

# create list of years to plot
sampling_years <- c(1987, 2012, 2014, 2015, 2016)


S3_figure <-
  coastal_vs_cashes %>%
  mutate(site = factor(site),
         site = fct_recode(site, `Cashes Ledge`        = "cashes",
                           `Western Maine Shelf` = "coastal")) %>%
  mutate(Year = year(timePeriod)) %>%
  dplyr::filter(Year %in% sampling_years) %>%
  ggplot(aes(significant_wave_height)) +
  stat_ecdf(aes(colour = site), alpha = 0.7, na.rm = FALSE) +        
  theme_bw() +
  facet_wrap(~ Year, ncol = length(sampling_years)) +
  scale_colour_manual(values = c("#e6550d","#756bb1")) +
  ylab("Cumulative distribution") +
  xlab("Significant Wave Height (m)") +
  scale_y_continuous(breaks = c(0.25, 0.50, 0.65, 0.85, 1.0)) +
  coord_flip() +
  theme(plot.title       = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text       = element_text(size = 10),
        axis.text.y      = element_text(size = 9),
        axis.text.x      = element_text(size = 8),
        axis.title       = element_text(size = 14)
        # legend.position  = "none"
        )

S3_figure

### Supplemental Table 2 ###

# create list of years to summarise
sampling_years <- c(1987, 2012, 2014, 2015, 2016)

# shape data for summarising
S2_Table <-
  coastal_vs_cashes %>%
  mutate(site = factor(site),
         site = fct_recode(site, `Cashes Ledge`        = "cashes",
                           `Western Maine Shelf` = "coastal")) %>%
  mutate(Year = year(timePeriod)) %>%
  dplyr::filter(Year %in% sampling_years) %>%
  group_by(site, Year) %>%
  summarise(nintyFive = quantile(significant_wave_height, probs = 0.95) %>% round(3),
            nintyNine = quantile(significant_wave_height, probs = 0.99) %>% round(3))

S2_Table

### Figure 3 ###

# add log-transformed values to kelp density table
density = density %>% 
  mutate(Species     = gsub("S.digitata", "S. digitata", Species),
         log_density = log(Density + 1.1))

biomass = biomass %>%
  mutate(log_biomass = log(Biomass + 1.1))


# Generate boxplot versions
# create species table for shading matrix
speciesTable <- data.frame(Year = rep(c(2012, 2014, 2015, 2016), 7),
                           Site = rep(c("Ammen Rock 1", 
                                        "Ammen Rock 2", 
                                        "Ammen Rock 3", 
                                        "Lunging Island", 
                                        "Mingo Rock", 
                                        "Spout Shoal", 
                                        "Star Island"), 4),
                           Species = rep(c("A. clathratum", 
                                           "S. digitata", 
                                           "S. longicruris"), 28)) %>%
  arrange(Year, Site, Species) %>%
  mutate(Year = as.factor(Year),
         Site = as.factor(Site))          

# create data.frame for shading na values
shading <- 
  with(biomass, table(Year, Site)) %>%
  data.frame() %>%
  mutate(log_biomass = ifelse(Freq > 0, 1, 0),
         Biomass     = ifelse(Freq > 0, 1, 0)) %>%
  left_join(speciesTable) %>%
  mutate(Region = fct_recode(Site, "Cashes Ledge" = "Ammen Rock 1",
                             "Cashes Ledge" = "Ammen Rock 2",
                             "Cashes Ledge" = "Ammen Rock 3",
                             "Coastal"      = "Lunging Island",
                             "Coastal"      = "Mingo Rock",
                             "Coastal"      = "Spout Shoal",
                             "Coastal"      = "Star Island"))

# modify data for boxplots
biomass_to_plot <- biomass %>%
  mutate(Year = factor(Year, levels = unique(biomass$Year))) %>%
  mutate(Region = fct_recode(Site, "Cashes Ledge" = "Ammen Rock 1",
                             "Cashes Ledge" = "Ammen Rock 2",
                             "Cashes Ledge" = "Ammen Rock 3",
                             "Coastal"      = "Lunging Island",
                             "Coastal"      = "Mingo Rock",
                             "Coastal"      = "Spout Shoal",
                             "Coastal"      = "Star Island"),
         Species = factor(Species, levels = c("S. longicruris", 
                                              "S. digitata", 
                                              "A. clathratum"))) %>%
  full_join(shading %>%
              dplyr::select(Year, Site, Region, Species))

# create boxplots
figure_3 <- ggplot(biomass_to_plot %>%
                          dplyr::filter(Year != "2012",
                                        !is.na(Species)) %>%
                          mutate(Species = factor(Species, levels = c("S. longicruris", 
                                                                      "S. digitata", 
                                                                      "A. clathratum"))), 
                        aes(x      = Site, 
                            y      = Biomass, 
                            colour = Region)) +
  geom_boxplot(aes(colour = Region, fill = Region)) +                              
  geom_rect(aes(xmin   = as.numeric(Site) - 0.3,
                xmax   = as.numeric(Site) + 0.3,
                ymin   = -Inf,
                ymax   = Inf),
            fill   = "grey90",
            colour = "grey90",
            data   = shading %>%
              dplyr::filter(Biomass == 0,
                            Year != "2012")) +
  facet_grid(Species ~ Year, scales = "free_y") +
  theme_bw() +
  scale_colour_manual(values = c("#e6550d","#756bb1")) +
  scale_fill_manual(values   = c("#fdae6b","#bcbddc")) +
  labs(x = "", 
       y = expression(paste('Kelp biomass (g*m',""^-2,')')))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

figure_3


### Code for extracting summary values mentioned in-text ###

# Get summary stats on each species site and year
biomasses <- biomass %>%
  group_by(Site, Year, Species) %>%
  summarise(mean    = mean(Biomass),
            sem     = sd(Biomass)/sqrt(length(Biomass)),
            logmean = mean(log_biomass),
            logsem  = sd(log_biomass)/sqrt(length(log_biomass))) %>%
  mutate(lower     = mean - 2 *sem, 
         upper     = mean + 2 *sem, 
         log_lower = logmean - 2 * logsem,
         log_upper = logmean + 2 * logsem) %>%
  ungroup() %>%        
  mutate(Year = factor(Year, levels = unique(biomass$Year))) %>%
  mutate(Region = fct_recode(Site, "Ammen Rock" = "Ammen Rock 1",
                             "Ammen Rock" = "Ammen Rock 2",
                             "Ammen Rock" = "Ammen Rock 3",
                             "Coastal"    = "Lunging Island",
                             "Coastal"    = "Mingo Rock",
                             "Coastal"    = "Spout Shoal",
                             "Coastal"    = "Star Island"),
         Species = factor(Species, levels = c("S. longicruris", 
                                              "S. digitata", 
                                              "A. clathratum")))  

biomasses

### Supplemental Table 3 ###

# Perform 1-way ANOVA and post-hoc Tukey's honest significant difference tests on the factor "Site" for each species of kelp and year of sampling.

byears=unique(biomass$Year)
byears=byears[-1]
species=unique(biomass$Species)
for (i in 1:length(byears)){
  yeardata=biomass[which(biomass$Year==byears[i]),]
  for(j in 1:length(species)){
    dat=yeardata[which(yeardata$Species==species[j]),]
    mod=lm(log_biomass ~ Site, data=dat)
    print(byears[i])
    print(species[j])
    print(anova(mod))
    print(TukeyHSD(aov(log_biomass ~ Site, data=dat)))
  }
}

### Figure 4 ###

# Generate figures for kelp densities
# create species table for shading matrix
speciesTable <- data.frame(Year = rep(c(1987, 2012, 2014, 2015, 2016), 8),
                           Site = rep(c("Ammen Rock 1", 
                                        "Ammen Rock 2", 
                                        "Ammen Rock 3", 
                                        "Lunging Island", 
                                        "Mingo Rock", 
                                        "Spout Shoal", 
                                        "Star Island", 
                                        "Duck Island"), 5),
                           Species = rep(c("S. longicruris", 
                                           "S.digitata", 
                                           "A. clathratum"), 40)) %>%
  arrange(Year, Site, Species) %>%
  mutate(Year = as.factor(Year),
         Site = as.factor(Site))   

# create data.frame for shading na values
shading <- with(density, table(Year, Site)) %>%
  data.frame() %>%
  mutate(log_density = ifelse(Freq > 0, 1, 0),
         Density     = ifelse(Freq > 0, 1, 0)) %>%
  left_join(speciesTable) %>%
  mutate(Region = fct_recode(Site, "Cashes Ledge" = "Ammen Rock 1",
                             "Cashes Ledge" = "Ammen Rock 2",
                             "Cashes Ledge" = "Ammen Rock 3",
                             "Coastal"      = "Lunging Island",
                             "Coastal"      = "Mingo Rock",
                             "Coastal"      = "Spout Shoal",
                             "Coastal"      = "Star Island",
                             "Coastal"      = "Duck Island"))

# modify data
density_to_plot <- density %>%
  # group_by(Site, Year, Species) %>%
  # summarise(mean    = mean(Density),
  # sem     = sd(Density)/sqrt(length(Density)),
  # logmean = mean(log_density),
  # logsem  = sd(log_density)/sqrt(length(log_density))) %>%
  # mutate(lower     = mean - 2 *sem, 
  # upper     = mean + 2 *sem, 
  # log_lower = logmean - 2 * logsem,
  # log_upper = logmean + 2 * logsem) %>%
  # ungroup() %>%        
  mutate(Year = factor(Year, levels = unique(density$Year))) %>%
  mutate(Region = fct_recode(Site, "Cashes Ledge" = "Ammen Rock 1",
                             "Cashes Ledge" = "Ammen Rock 2",
                             "Cashes Ledge" = "Ammen Rock 3",
                             "Coastal"      = "Lunging Island",
                             "Coastal"      = "Mingo Rock",
                             "Coastal"      = "Spout Shoal",
                             "Coastal"      = "Star Island",
                             "Coastal"      = "Duck Island"),
         Species = factor(Species, levels = c("S. longicruris", 
                                              "S.digitata", 
                                              "A. clathratum"))) %>%
  full_join(shading %>%
              dplyr::select(Year, Site, Region, Species))

# create boxplots
figure_4 <- ggplot(density_to_plot %>%
                          mutate(Species=factor(Species, levels = c(
                            "S. longicruris", 
                            "S.digitata", 
                            "A. clathratum"))) %>%
                          dplyr::filter(!Year %in% c("1987", "2012"),
                                        !is.na(Species)), 
                        aes(x      = Site, 
                            y      = Density, 
                            colour = Region)) +
  geom_boxplot(aes(colour = Region, fill = Region)) +                              
  geom_rect(aes(xmin   = as.numeric(Site) - 0.3,
                xmax   = as.numeric(Site) + 0.3,
                ymin   = -Inf,
                ymax   = Inf),
            fill   = "grey90",
            colour = "grey90",
            data   = shading %>%
              dplyr::filter(Density == 0,
                            !Year %in% c("1987", "2012"),
                            !is.na(Species))) +
  facet_grid(Species ~ Year) +
  theme_bw() +
  scale_colour_manual(values = c("#e6550d","#756bb1")) +
  scale_fill_manual(values   = c("#fdae6b","#bcbddc")) +
  labs(x = "", 
       y = expression(paste('Kelp density (N*m',""^-2,')')))+
  # theme(axis.text.x = element_text(angle = 60, hjust = 1),  ## -- test clearing facet backgrounds -- ##
  # strip.background   = element_blank())
  theme(axis.text.x = element_text(angle = 60, hjust = 1))                 

figure_4


### Code for extracting summary values mentioned in-text ###

densities <- density %>%
  group_by(Site, Year, Species) %>%
  summarise(mean    = mean(Density),
            sem     = sd(Density)/sqrt(length(Density)),
            logmean = mean(log_density),
            logsem  = sd(log_density)/sqrt(length(log_density))) %>%
  mutate(lower     = mean - 2 *sem, 
         upper     = mean + 2 *sem, 
         log_lower = logmean - 2 * logsem,
         log_upper = logmean + 2 * logsem) %>%
  ungroup() %>%        
  mutate(Year = factor(Year, levels = unique(density$Year))) %>%
  mutate(Region = fct_recode(Site, "Ammen Rock" = "Ammen Rock 1",
                             "Ammen Rock" = "Ammen Rock 2",
                             "Ammen Rock" = "Ammen Rock 3",
                             "Coastal"    = "Lunging Island",
                             "Coastal"    = "Mingo Rock",
                             "Coastal"    = "Spout Shoal",
                             "Coastal"    = "Star Island"),
         Species = factor(Species, levels = c("S. longicruris", 
                                              "S. digitata", 
                                              "A. clathratum")))  

densities

### Supplemental Table 4 ###

byears=unique(density$Year)
byears=byears[-c(4,5)]
species=unique(density$Species)
for (i in 1:length(byears)){
  yeardata=density[which(density$Year==byears[i]),]
  for(j in 1:length(species)){
    dat=yeardata[which(yeardata$Species==species[j]),]
    mod=lm(log_density ~ Site, data=dat)
    print(byears[i])
    print(species[j])
    print(anova(mod))
    print(TukeyHSD(aov(log_density ~ Site, data=dat)))
  }
}

### Figure 5 ###

density <- density %>%
  mutate(Year = factor(Year, levels = c(1987, 2012, 2014, 2015, 2016))) %>%
  mutate(Region = fct_recode(Site, "Ammen Rock" = "Ammen Rock 1",
                             "Ammen Rock" = "Ammen Rock 2",
                             "Ammen Rock" = "Ammen Rock 3",
                             "Coastal"    = "Lunging Island",
                             "Coastal"    = "Mingo Rock",
                             "Coastal"    = "Spout Shoal",
                             "Coastal"    = "Star Island",
                             "Coastal"    = "Duck Island"),
         Species = factor(Species, levels = c("S. longicruris", 
                                              "S. digitata", 
                                              "A. clathratum")))

# subset the longicruris
longicruris <- 
  density %>%
  dplyr::filter(Species == "S. longicruris",
                Site    == "Ammen Rock 1") %>%
  dplyr::select(Year, Quadrat, Density) %>%
  arrange(Year)

# calculate the mean density by year
longicruris_means <-
  longicruris %>%
  group_by(Year) %>%
  summarise(meanDensity = mean(Density, na.rm = TRUE))


##
##  2. Create visual
##
# geom_point version
# quartz("density", 5, 5)
figure_5 <- 
  longicruris %>%
  ggplot(aes(Year, Density)) +
  geom_boxplot(fill = "grey70") +
  geom_point(aes(y = meanDensity), 
             size = 4,
             pch  = 18,
             data = longicruris_means) +
  # geom_violin(fill = "grey70", alpha = 0.7) +
  theme_bw() +
  # scale_colour_manual(values = c(rep("#e6550d"), 4)) +
  # scale_fill_manual(values   = c(rep("#fdae6b"), 4)) +
  labs(x = "", 
       y = expression(paste('Density (N*m',""^-2,')'))) +
  # ggtitle("S. longicruris density - Cashes Ledge") +
  scale_y_continuous(breaks = seq(from = 10, to = 100, by = 10)) +
  theme(plot.title         = element_text(size  = 16, 
                                          vjust = 0, 
                                          hjust = 0.5),
        legend.position    = "none",
        panel.grid.major.x = element_blank(),
        axis.text          = element_text(size = 14),
        axis.title.y       = element_text(size = 16))


figure_5

## In-text mention of tests for significant differences between years for AR1 S. latissima densities

temporal.mod = lm(log_density ~ Year, data = density[which(density$Site=="Ammen Rock 1" & density$Species=="S. longicruris"),])

anova(temporal.mod)
TukeyHSD(aov(temporal.mod))

## Supplemental Table S5 was created in JMP ##

## Figure 6 was created in Photoshop ##

## Table 2 was created in JMP ##

## The relationship between Saccharina latissima and S. digitata was derived in JMP ##

### Table 3 ###

table_3=struc %>%
  group_by(Site) %>%
  summarize(mean.het=mean(Heterosiphonia.japonica*100), se.het=sd(Heterosiphonia.japonica*100/sqrt(length(Heterosiphonia.japonica))), N = length(Heterosiphonia.japonica))

table_3

## Supplemental Table 6 ##

log.japonica = struc[which(struc$Site!="Ammen Rock 1 1987"),c(1:4, 9)] %>%
  mutate(log_japonica = log(Heterosiphonia.japonica*100+1.1))


mod.japonica=lm(log(Heterosiphonia.japonica*100+1.1) ~ Site, data=struc[which(struc$Site!="Ammen Rock 1 1987"),])
summary(mod.japonica)
test.japonica = anova(mod.japonica)

# Use TukeyHSD to test for differences in Dasysiphonia japonica densities

tukey.japonica=TukeyHSD(aov(mod.japonica))

S6_table = list(test.japonica, tukey.japonica)

S6_table

## S7 Table (One-way ANOVA and post-hoc Tukey's honest significant difference tests for differences in sea urchin abundance between sites) was created in JMP ##

### Supplemental Table 8 ###

# Perform multivariate analysis of variance (PERMANOVA) on factor Site

# Perform arcsine square root transformation of the data

struc1 = struc
struc1[,-c(1:7)]=asin(sqrt(struc1[,-c(1:7)]))

S8_table = adonis(struc1[,-c(1:7)] ~ struc$Site, perm=1000)

S8_table 

### Figure 7 ###

# Create the dissimilarity matrix using the built-in vegan package MetaMDS (this step may need to be repeated a few times until the model reaches convergence at an acceptable stress level)

mod=metaMDS(struc1[,-c(1:7)], distance="bray", autotransform = F)

# Add environmental data for plotting

mod_points=cbind(struc1[,1:4], mod$points)

#Plot MDS with color set to region

reg2 <- as.vector(mod_points$Region)
reg2[reg2=="Onshore"] <- "#bcbddc"
reg2[reg2=="Offshore"] <- "#fdae6b"
reg2=as.factor(reg2)

#Set pch (symbol type) to site

reg <- as.vector(mod_points$Site)

reg[reg=="Lunging Island"]<- 8
reg[reg=="Mingo Rock"] <- 4
reg[reg=="Spout Shoal"]<- 3
reg[reg=="Star Island"]<- 5
reg[reg=="Ammen Rock 1"]<- 0
reg[reg=="Ammen Rock 2"]<- 1
reg[reg=="Ammen Rock 1 1987"]<- 2

reg=as.numeric(reg)


#plot using ordispider

par(pty ="m")

ordiplot(mod, type="n", xlab="nMDS 1", ylab="nMDS 2", ylim = c(-1.2, .75), xlim = c(-1.2, 1.8))

ordispider(mod_points[c(5:6)], mod_points$Site, lty=1, col="#525252", lwd=.3)

points(mod_points[c(5:6)], col=as.character(reg2), pch=reg, cex=1)

legend("bottomright", 
       legend = unique(as.factor(mod_points$Site)),
       pch= c(8, 4, 3, 5, 0, 1, 2),
       col = c("#bcbddc", "#bcbddc", "#bcbddc", "#bcbddc", "#fdae6b", "#fdae6b", "#fdae6b"),
       cex=.5)

## In-text mention of replication of photo quadrats ##

reps = struc %>%
  group_by(Site) %>%
  summarize(rep = length(unique(Quadrat)))

reps

### Supplemental Table 9 ###

# Use simper analysis to extract species of importance in differences among sites

site.simper=simper(struc[,-c(1:7)]*100, struc$Site, perm=200)

S9_table=summary(site.simper, ordered = T)

S9_table

### Supplemental Table 10 ###

# Use simper analysis to extract species of importance in differences between offshore and coastal regions

reg.simper=simper(struc[,-c(1:7)]*100, struc$Region, perm=200)

S10_table=summary(reg.simper, ordered = T)

S10_table

### Supplemental Table 11 ###

# Select only Ammen Rock sites and use simper analysis to extract species of importance in differences between offshore sites on Ammen Rock only

arock = struc %>%
  dplyr::filter(Site %in% c("Ammen Rock 1", "Ammen Rock 2", "Ammen Rock 1 1987"))

# Use Adonis PERMANOVA to test for differences among sites

arock_permanova = adonis(asin(sqrt(arock[,-c(1:7)])) ~ arock$Site, perm=1000)

# Use simper analysis to extract species of importance

arock.simper = simper(arock[,-c(1:7)]*100, arock$Site, perm=200)

arock_simper_table=summary(arock.simper, ordered = T)

S11_table = list(arock_permanova, arock_simper_table)

S11_table

### Figure 8 ###

fish_transect <- fish_transects %>%
  mutate(`Transect segment`=gsub("20-Oct", "10-20", `Transect segment`), Site = gsub("Lunging Shoal", "Lunging Island", Site))


#Bring in species length-weight equations

equations <- read_csv(file="data/length_weights.csv")


# Melt on size classes to get all abundances in a single column, with another column expressing the sizes, remove X from sizes column, set NA values to 0, merge with length-weight equations, calculate biomass using length-weight equation parameters, standardize to 50m^2 area, sum all biomass for each species within each block/transect, sum all species except cunner cod and pollack into "other" column, fill in implicit zeroes

size.fish <- fish_transect %>% 
  gather(Size, Abundance, -Site, -Region, -Year, -Date,
         -Transect, -`Transect Length`,
         -`Transect segment`,
         -`Block Length`, -Block, -`Depth (m)`,
         -`Temperature (C)`, -Genus_species,
         -Total) %>%
  mutate(Size      = gsub("X", "", Size),
         Size      = as.numeric(Size),
         Abundance = ifelse(is.na(Abundance), 0, Abundance)) %>%
  left_join(equations) %>%
  mutate(biomass=Abundance*(a*Size^b)*10/`Block Length`) %>%
  group_by(Genus_species, Site, Region, Year, Transect, `Transect segment`, Block) %>%
  summarize(biomass=sum(biomass)) %>%
  spread(Genus_species, biomass, fill = 0) %>%
  mutate(Other = sum(`Cyclopterus lumpus`, `Hemitripterus americanus`, `Myoxocephalus spp`, `Pholis gunnellus`, `Ulvaria subbifurcata`, `Zoarces americanus`)) %>%
  gather(Species, Biomass, -Site, -Region, -Year, -Transect, -`Transect segment`, -Block) %>%
  dplyr::filter(Species %in% c("Gadus morhua",  "Tautogolabrus adspersus", "Pollachius virens", "Other"))


## Generate Boxplots

# Create species table for shading unobserved site/years
# create species table for shading matrix
species_table <- 
  data.frame(Year    = c(rep(2014, 28), c(rep(2015, 28))),
             Site    = rep(c("Ammen Rock 1", "Ammen Rock 2", "Duck Island",
                             "Lunging Island", "Mingo Rock", "Spout Shoal", 
                             "Star Island"), 8),
             Species = rep(c("Gadus morhua", "Pollachius virens", "Tautogolabrus adspersus", "Other"), 14)) %>%
  arrange(Year, Site) %>%
  mutate(Year = as.factor(Year),
         Site = as.factor(Site))          

# create data.frame for shading na values
shading <- with(size.fish, table(Year, Site)) %>%
  data.frame() %>%
  mutate(Biomass     = ifelse(Freq > 0, 1, 0)) %>%
  right_join(species_table) %>%
  mutate(Region = fct_recode(Site, "Cashes Ledge" = "Ammen Rock 1",
                             "Cashes Ledge" = "Ammen Rock 2",
                             "Coastal"      = "Duck Island",
                             "Coastal"      = "Lunging Island",
                             "Coastal"      = "Mingo Rock",
                             "Coastal"      = "Spout Shoal",
                             "Coastal"      = "Star Island")) %>%
  arrange(Year, Site, Biomass)

# modify data for boxplots
biomass_to_plot <- size.fish %>%
  ungroup() %>%
  mutate(Year = factor(Year, levels = unique(size.fish$Year))) %>%
  mutate(Region = fct_recode(Site, "Cashes Ledge" = "Ammen Rock 1",
                             "Cashes Ledge" = "Ammen Rock 2",
                             "Coastal"      = "Duck Island",
                             "Coastal"      = "Lunging Island",
                             "Coastal"      = "Mingo Rock",
                             "Coastal"      = "Spout Shoal",
                             "Coastal"      = "Star Island"),
         Species = factor(Species, levels = c(
           
           "Gadus morhua",
           
           "Pollachius virens",
           "Tautogolabrus adspersus",
           #                                 "Myoxocephalus spp",
           #                                "Hemitripterus americanus",
           #                               "Pholis gunnellus",
           #                              "Ulvaria subbifurcata",
           #                             "Cyclopterus lumpus",
           #                            "Zoarces americanus"))) 
           "Other")))%>%
  full_join(shading %>%
              dplyr::select(Year, Site, Region, Species))

figure_8 <- ggplot(biomass_to_plot %>% 
                            mutate(Species=factor(Species, levels = c(
                              "Gadus morhua",
                              "Pollachius virens",
                              "Tautogolabrus adspersus",
                              #           "Myoxocephalus spp",
                              #          "Hemitripterus americanus",
                              #         "Pholis gunnellus",
                              #        "Ulvaria subbifurcata",
                              #       "Cyclopterus lumpus",
                              #      "Zoarces americanus"
                              "Other"                             
                            ))) %>%
                            dplyr::filter(Species != "Other"),
                          aes(x      = Site,
                              y      = Biomass,
                              colour = Region)) +
  geom_boxplot(aes(colour = Region, fill = Region)) +
  geom_rect(aes(xmin = as.numeric(Site) - 0.3,
                xmax = as.numeric(Site) + 0.3,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey90",
            colour = "grey90",
            data = shading %>%
              dplyr::filter(Species != "Other") %>%
              dplyr::filter(Biomass == 0)) +
  facet_grid(Species ~ Year, scales = "free_y") +
  theme_bw() +
  scale_colour_manual(values = c("#e6550d","#756bb1")) +
  scale_fill_manual(values   = c("#fdae6b","#bcbddc")) +
  labs(x = "", 
       y = expression(paste('Biomass (g*50m',""^-2,')')))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

figure_8

### Supplemental Table 12 ###

# Total biomass (sum across species) for each transect segment

total.biomass <- size.fish %>%
  group_by(Site, Transect, `Transect segment`, Block, Year) %>%
  summarise(total = sum(Biomass))

# Create linear model testing differences between sites and years

total.biomass.mod <- lm(log(total+1) ~ Site*Year, data=total.biomass) %>%
  anova()

# Post-hoc tests for significant differences between sites

total.biomass.tukey <- TukeyHSD(aov(log(total+1) ~ Site, data=total.biomass))

# Post-hoc tests for significant differences between sites and years for which there is data for both years

year.total.biomass <- dplyr::filter(total.biomass, Site %in% c("Ammen Rock 1", "Mingo Rock", "Spout Shoal", "Star Island")) 
year.total.biomass.tukey  <- TukeyHSD(aov(log(total+1)~Site*as.factor(Year), data=year.total.biomass))

S12_table = list(total.biomass.mod, total.biomass.tukey, year.total.biomass.tukey)

S12_table

## In-text citations of average biomass values per site and species ##

# Mean biomass for each species, site, and year

mass.counts <- size.fish %>% 
  group_by(Site, Year, Species) %>%
  summarise(mean = mean(Biomass),
            sd = sd(Biomass)) %>%
  arrange(Species, mean) %>%
  as.data.frame()

mass.counts

## In-text citations of average abundance values per site and species ##


abund.counts <- fish_transect %>%
  mutate(Total=Total*10/`Block Length`) %>%
  complete(Genus_species, nesting(Site, Transect, `Transect segment`, Block, Year), fill=list(Total=0)) %>%
  group_by(Site, Year, Genus_species) %>%
  summarise(mean = mean(Total),
            sd = sd(Total)) %>%
  arrange(Genus_species, mean) %>%
  as.data.frame()

abund.counts

### Figure 9 ###

fish_video <- fish_videos %>%
  mutate(logMaxN = log(MaxN+1), 
         Species=fct_recode(Fish.Species, "Gadus morhua" = "Cod",
                            "Pollachius virens" = "Pollack",
                            "Tautogolabrus adspersus" = "Cunner"),
         Site = fct_recode(Site, "Lunging Island" = "Lunging Shoal",
                           "Mingo Rock"     = "Mingo"),
         Region = fct_recode(Region, "Cashes Ledge" = "Offshore",
                             "Coastal"      = "Onshore")) %>%
  complete(Species, nesting(Site, Video.Number, Segment, Year), fill=list(MaxN=0)) 
# Generate Boxplots

# Create species table for shading unobserved site/years
# create species table for shading matrix
species_table <- 
  data.frame(Year    = c(rep(2014, 21), c(rep(2015, 21), c(rep(2016, 21)))),
             Site    = rep(c("Ammen Rock 1", "Ammen Rock 2", "Ammen Rock 3",
                             "Lunging Island", "Mingo Rock", "Spout Shoal", 
                             "Star Island"), 9),
             Species = rep(c("Gadus morhua", "Pollachius virens", "Tautogolabrus adspersus"), 21)) %>%
  arrange(Year, Site) %>%
  mutate(Year = as.factor(Year),
         Site = as.factor(Site))          

# create data.frame for shading na values
shading <- with(fish_video, table(Year, Site)) %>%
  data.frame() %>%
  mutate(MaxN     = ifelse(Freq > 0, 1, 0)) %>%
  right_join(species_table) %>%
  mutate(Region = fct_recode(Site, "Cashes Ledge" = "Ammen Rock 1",
                             "Cashes Ledge" = "Ammen Rock 2",
                             "Cashes Ledge"      = "Ammen Rock 3",
                             "Coastal"      = "Lunging Island",
                             "Coastal"      = "Mingo Rock",
                             "Coastal"      = "Spout Shoal",
                             "Coastal"      = "Star Island")) %>%
  arrange(Year, Site, MaxN)

# modify data for boxplots
maxn_to_plot <- fish_video %>%
  ungroup() %>%
  mutate(Year = factor(Year, levels = unique(fish_video$Year))) %>%
  mutate(Region = fct_recode(Site, "Cashes Ledge" = "Ammen Rock 1",
                             "Cashes Ledge" = "Ammen Rock 2",
                             "Cashes Ledge"      = "Ammen Rock 3",
                             "Coastal"      = "Lunging Island",
                             "Coastal"      = "Mingo Rock",
                             "Coastal"      = "Spout Shoal",
                             "Coastal"      = "Star Island"),
         Species = factor(Species, levels = c(
           
           "Gadus morhua",
           
           "Pollachius virens",
           "Tautogolabrus adspersus")))%>%
  full_join(shading %>%
              dplyr::select(Year, Site, Region, Species))

# Make a boxplot for each species site and year

figure_9 <- ggplot(maxn_to_plot %>% 
                         mutate(Species=factor(Species, 
                                               levels = c(
                                                 "Gadus morhua",
                                                 "Pollachius virens",
                                                 "Tautogolabrus adspersus"))),
                       aes(x = Site,
                           y = MaxN,
                           colour = Region)) +
  geom_boxplot(aes(colour = Region, fill = Region)) +
  facet_wrap(~Species, scales = "free_y") +
  theme_bw() +
  scale_colour_manual(values = c("#e6550d","#756bb1")) +
  scale_fill_manual(values   = c("#fdae6b","#bcbddc")) +
  labs(x = "", 
       y = expression(paste('MaxN (10min',""^-1,')')))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

figure_9

### Supplemental Video 1 was created in Adobe Premiere ###

### Supplemental Table 13 ###

## 2. Statistical analyses

counts <- fish_video %>% 
  group_by(Site, Year, Species) %>%
  summarise(mean = mean(MaxN),
            sd = sd(MaxN)) %>%
  arrange(Species, mean) %>%
  as.data.frame()

# Create linear model testing differences between sites for cod, cunner, and pollack

cod.mod <- lm(log(MaxN+1) ~ Site, data=fish_video[which(fish_video$Fish.Species=="Cod"),]) %>%
  anova()

cun.mod <- lm(log(MaxN+1) ~ Site, data=fish_video[which(fish_video$Fish.Species=="Cunner"),]) %>%
  anova()

pol.mod <- lm(log(MaxN+1) ~ Site, data=fish_video[which(fish_video$Fish.Species=="Pollack"),]) %>%
  anova()

# Post-hoc tests for significant differences between sites

cod.tukey <- TukeyHSD(aov(log(MaxN+1) ~ Site, data=fish_video[which(fish_video$Fish.Species=="Cod"),]))

cun.tukey <- TukeyHSD(aov(log(MaxN+1) ~ Site, data=fish_video[which(fish_video$Fish.Species=="Cunner"),]))

pol.tukey <- TukeyHSD(aov(log(MaxN+1) ~ Site, data=fish_video[which(fish_video$Fish.Species=="Pollack"),]))


S13_table = list(cod.mod, cod.tukey, cun.mod, cun.tukey, pol.mod, pol.tukey)

S13_table

### Figure 10 ###

# Time series boxplots for Ammen Rock


figure_10 <- ggplot(maxn_to_plot %>%
                           dplyr::filter(Site %in% c("Ammen Rock 1", "Ammen Rock 2", "Ammen Rock 3"))%>%
                           mutate(Species=factor(Species, 
                                                 levels = c(
                                                   "Gadus morhua",
                                                   "Pollachius virens",
                                                   "Tautogolabrus adspersus"))),
                         aes(x = Site,
                             y = MaxN,
                             colour = Region)) +
  geom_boxplot(aes(colour = Region, fill = Region)) +
  geom_rect(aes(xmin = as.numeric(Site) - 0.3,
                xmax = as.numeric(Site) + 0.3,
                ymin = -Inf,
                ymax = Inf),
            fill = "grey90",
            colour = "grey90",
            data = shading %>%
              dplyr::filter(Site %in% c("Ammen Rock 1", "Ammen Rock 2", "Ammen Rock 3"))%>%
              dplyr::filter(MaxN==0)) +
  facet_grid(Species~Year, scales = "free_y") +
  theme_bw() +
  scale_colour_manual(values = c("#e6550d","#756bb1")) +
  scale_fill_manual(values   = c("#fdae6b","#bcbddc")) +
  labs(x = "", 
       y = expression(paste('MaxN (10min',""^-1,')')))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

figure_10

### Supplemental Table 14 ###

cashes <- dplyr::filter(fish_video, Site %in% c("Ammen Rock 1", "Ammen Rock 2", "Ammen Rock 3"))%>%
  filter(Year!=1987)


cashes.cod.mod=lm(log(MaxN+1) ~ as.factor(Year), data=cashes[which(cashes$Fish.Species=="Cod"),]) %>%
  anova()

cashes.cun.mod=lm(log(MaxN+1) ~ as.factor(Year), data=cashes[which(cashes$Fish.Species=="Cunner"),]) %>%
  anova()

cashes.pol.mod=lm(log(MaxN+1) ~ as.factor(Year), data=cashes[which(cashes$Fish.Species=="Pollack"),]) %>%
  anova()

cashes.cod.tukey  <- TukeyHSD(aov(log(MaxN+1)~as.factor(Year), data=as.data.frame(cashes[which(cashes$Fish.Species=="Cod"),])))

cashes.cun.tukey  <- TukeyHSD(aov(log(MaxN+1)~as.factor(Year), data=as.data.frame(cashes[which(cashes$Fish.Species=="Cunner"),])))

cashes.pol.tukey  <- TukeyHSD(aov(log(MaxN+1)~as.factor(Year), data=as.data.frame(cashes[which(cashes$Fish.Species=="Pollack"),])))

S14_table = list(cashes.cod.mod, cashes.cod.tukey, cashes.cun.mod, cashes.cun.tukey, cashes.pol.mod, cashes.pol.tukey)

S14_table

### Supplemental Table 15 was created in Excel ###

### Supplemental Table 16 ###

S16_table <- fish_transect %>%
  mutate(Total=Total*10/`Block Length`) %>%
  complete(Genus_species, nesting(Region, Site, Transect, `Transect segment`, Block, Year), fill=list(Total=0)) %>%
  group_by(Region, Site, Transect, `Transect segment`, Block, Year) %>%
  summarise(total = sum(Total)) %>%
  #Use the following commented out line and add a pound sign # to comment out the group_by(Site) line to get regional averages
  #group_by(Region) %>%
  group_by(Site) %>%
  summarize(mean_abundance = mean(total*10), se_abundance = sd(total*10)/sqrt(length(total)))

S16_table
