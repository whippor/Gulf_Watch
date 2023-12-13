#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Gulfwatch intertidal community exploration                                     ##
# Script created 2023-12-01                                                      ##
# Data source: Alaska Gulf Watch                                                 ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2023-12-05                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:

# Script exploring rocky intertidal community data collected by Gulfwatch


# Required Files (check that script is loading latest version):
# KBAY2012-2023_Rocky_Intertidal_Motile_Invert_Count.csv
# KBAY2012-2023_Rocky_Intertidal_Percent_Cover.csv
# KATMKEFJPWS_2006-2023_Rocky_Intertidal_Cover.csv


# Associated Scripts:
# NONE

# TO DO 

# -join KBAY and other site datasets by harmonizing categories
# -import intertidal temperature anomalies (air and water) and summarise
# per site
# -PCA/PERMANOVA/nMDS of communities as they relate to air and water temp anoms

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# RECENT CHANGES TO SCRIPT                                                        +
# LOAD PACKAGES                                                                   +
# READ IN AND PREPARE DATA                                                        +
# MANIPULATE DATA                                                                 +
#                                                                                 +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)
library(lubridate)
library(GGally)
library(worrms)
library(scales)

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# fuction for "%notin%
`%notin%` <- Negate(`%in%`)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

allCover_tax <- read.csv("~/git/Gulf_Watch/ProcessedData/ProcessedIntertidal/allCoverQAQC.csv")

# Seastar cover data

# all but KBAY
star_KAT <- read_csv("RawData/IntertidalCover/KATMKEFJWPWS_2006-2023_Sea_Star_Count.csv")
# summarise to 100m2 for joining
star_KAT1 <- star_KAT %>%
  mutate(dens100 = Density_individual200SqM/2) %>%
  mutate(Block_Name = case_when(SiteName == "Observation Island" ~ "EPWS",
                                SiteName == "Simpson Bay" ~ "EPWS",
                                SiteName == "Olsen  Bay" ~ "EPWS",
                                SiteName == "Port Fidalgo" ~ "EPWS",
                                SiteName == "Galena Bay" ~ "EPWS",
                                SiteName == "Northwest Bay" ~ "WPWS",
                                SiteName == "Disk Island" ~ "WPWS",
                                SiteName == "Herring Bay (Bear Cove)" ~ "WPWS",
                                SiteName == "Herring Bay" ~ "WPWS",
                                SiteName == "Johnson Bay" ~ "WPWS",
                                SiteName == "Whale Bay" ~ "WPWS",
                                SiteName == "Iktua Bay" ~ "WPWS",
                                SiteName == "Hogan Bay" ~ "WPWS",
                                SiteName == "Unakwik Inlet" ~ "NPWS",
                                SiteName == "Perry Island" ~ "NPWS",
                                SiteName == "Bettles Bay" ~ "NPWS",
                                SiteName == "Esther Passage" ~ "NPWS",
                                SiteName == "Cedar Bay" ~ "NPWS",
                                SiteName == "Harris Bay" ~ "KEFJ",
                                SiteName == "Nuka Passage" ~ "KEFJ",
                                SiteName == "Nuka Bay" ~ "KEFJ",
                                SiteName == "McCarty Fjord" ~ "KEFJ",
                                SiteName == "Aialik Bay" ~ "KEFJ",
                                SiteName == "Ninagiak Island" ~ "KATM",
                                SiteName == "Takli Island" ~ "KATM",
                                SiteName == "Amalik Bay" ~ "KATM",
                                SiteName == "Kinak Bay" ~ "KATM",
                                SiteName == "Kaflia Bay" ~ "KATM",
                                SiteName == "Kukak Bay" ~ "KATM"),
         .before = SiteName) %>%
  mutate(SiteName = case_when(SiteName == "Olsen  Bay" ~ "Olsen Bay",
                              TRUE ~ SiteName),
         .keep = "unused", .before = SampleDate) %>%
  select(Block_Name:SiteName, Year, Species, dens100)

# KBAY
star_KBA <- read_csv("RawData/IntertidalCover/KBAY2012-2023_Sea_Star_Anemone_Count.csv")
# filter for seastar species and prepare to join (chose to only keep "low" stratum to 
# match up with other regions' sampling set at 0 MLLW.)
star_KBA1 <- star_KBA %>%
  filter(Species %in% c("Evasterias troschelii", "Solaster spp.", "Henricia leviuscula",
                        "Orthasterias koehleri", "Pycnopodia helianthoides", 
                        "Pisaster ochraceus", "Lethasterias nanimensis", "Asterias sp.",
                        "Dermasterias imbricata", "Solaster stimpsoni")) %>%
  mutate(SiteName = Site, .keep = "unused") %>%
  mutate(SampleDate = mdy(Date), .keep = "unused") %>%
  mutate(dens100 = `Abundance (# ind/100 m2)`, .keep = "unused") %>%
  select(SiteName, SampleDate, Year, Species, dens100, Stratum)
star_KBA2 <- star_KBA1 %>%
  tibble()
star_KBA3 <- star_KBA2 %>%
  complete(SiteName, Year, Stratum, Species) %>%
  mutate(zerodens = replace_na(dens100, 0), .keep = "unused") %>%
  mutate(dens100 = zerodens, .keep = "unused")
star_KBA4 <- star_KBA3 %>%
  group_by(SiteName, Year, Species) %>%
  summarise(sumstar = sum(dens100)) %>%
  mutate(dens100 = sumstar, .keep = "unused") %>%
  mutate(Block_Name = "KBAY", .before = "SiteName")
# Join star data
allStar <- star_KAT1 %>%
  bind_rows(star_KBA4)

rm(star_KAT1, star_KBA1, star_KBA2, star_KBA3, star_KBA4)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                              ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# VISUALIZATIONS                                                               ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# how has fucus cover changed through time across all sites?
allCover_tax %>%
  filter(Year %in% c(2013:2022)) %>%
  filter(Block_Name %notin% c("NPWS", "EPWS")) %>%
  filter(Species == "Fucus distichus") %>%
  mutate(yr = year(SampleDate)) %>%
  group_by(yr, SiteName, Block_Name) %>%
  summarise(PC = mean(Percent_Cover, na.rm = TRUE)) %>%
  ggplot(aes(x = yr, y = PC, color = Block_Name)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_viridis(discrete = TRUE, option = "F", begin = 0.2, end = 0.8) +
  theme_bw() +
  labs(title = "Gulfwatch Fucus Cover", y = "Percent Cover", x = "Date") +  
  scale_x_continuous(breaks= pretty_breaks())

# how has Alaria cover changed through time across all sites?
allCover_tax %>%
  filter(Species == "Alaria marginata") %>%
  mutate(yr = year(SampleDate)) %>%
  group_by(yr, SiteName, Block_Name) %>%
  summarise(PC = mean(Percent_Cover, na.rm = TRUE)) %>%
  ggplot(aes(x = yr, y = PC, color = Block_Name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(y = "Percent Cover", x = "Date")

# how are species abundances changing across all sites
allCover_tax %>%
  mutate(yr = year(SampleDate)) %>%
  filter(Species != "bare space") %>%
  group_by(yr, Species, Block_Name) %>%
  summarise(PC = mean(Percent_Cover, na.rm = TRUE)) %>%
  ggplot(aes(x = yr, y = PC, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(y = "Percent Cover", x = "Date") +
  theme(legend.position="none")
  
# how are order abundances changing across all sites
allCover_tax %>%
  mutate(yr = year(SampleDate)) %>%
  filter(Species != "bare space") %>%
  group_by(yr, order, Block_Name) %>%
  summarise(PC = mean(Percent_Cover, na.rm = TRUE)) %>%
  ggplot(aes(x = yr, y = PC, color = order)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(y = "Percent Cover", x = "Date") 

# how are class abundances changing across all sites
allCover_tax %>%
  mutate(yr = year(SampleDate)) %>%
  filter(Species != "bare space") %>%
  group_by(yr, class, Block_Name) %>%
  summarise(PC = mean(Percent_Cover, na.rm = TRUE)) %>%
  ggplot(aes(x = yr, y = PC, color = class)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  labs(y = "Percent Cover", x = "Date") +
  facet_wrap(.~Block_Name)

# how are phylum abundances changing across all sites

allCover_tax %>%
  filter(Year %in% c(2013:2022)) %>%
  filter(Block_Name %notin% c("NPWS", "EPWS")) %>%
  mutate(yr = year(SampleDate)) %>%
  filter(!is.na(phylum)) %>%
  group_by(yr, phylum, Block_Name) %>%
  summarise(PC = mean(Percent_Cover, na.rm = TRUE)) %>%
  ggplot(aes(x = yr, y = PC, color = phylum)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis(discrete = TRUE, option = "H", begin = 0.2, end = 0.8) +
  theme_bw() +
  labs(title = "Gulfwatch Cover by Phylum", y = "Percent Cover", x = "Date") +
  facet_wrap(.~Block_Name) +
  scale_x_continuous(breaks= pretty_breaks())

# SEASTAR TEMPS

# Ranked list of most sub sublethal exposures
# ranks
# KBAY 2020
# KATM 2017
# KATM 2020
# WPWS 2020
# KEFJ 2020
# KATM 2022 (21)
# KBAY 2021
# KEFJ 2017
# WPWS 2021
# KATM 2022
# KBAY 2022 (21, 22)
# KATM 2021

# What was change from 2019-2021 in seastar abundance?
#test <- 
allStar %>%
  filter(Year %in% c(2019, 2021)) %>%
  filter(Block_Name %notin% c("NPWS", "EPWS")) %>%
  group_by(Block_Name, SiteName, Year) %>%
  summarise(total = mean(dens100)) %>%
  ggplot() +
  geom_point(aes(x = Year, y = total, color = Block_Name)) +
  geom_line(aes(x = Year, y = total, group = SiteName, color = Block_Name))

# What was change from 2013-2022 in seastar abundance?
#test <- 
allStar %>%
  filter(Year %in% c(2013:2022)) %>%
  filter(Block_Name %notin% c("NPWS", "EPWS")) %>%
  group_by(Block_Name, SiteName, Year) %>%
  summarise(total = mean(dens100)) %>%
  ggplot() +
  geom_point(aes(x = Year, y = total, color = Block_Name)) +
  geom_line(aes(x = Year, y = total, group = SiteName, color = Block_Name))

allStar %>%
  filter(Year %in% c(2013:2022)) %>%
  filter(Block_Name %notin% c("NPWS", "EPWS")) %>%
  group_by(Block_Name, SiteName, Year) %>%
  summarise(total = mean(dens100)) %>%
  ggplot(aes(x = Year, y = total)) +
  geom_point(aes(color = Block_Name)) +
  geom_smooth(method = "loess", aes(color = Block_Name), se = FALSE) +
  scale_color_viridis(discrete = TRUE, option = "F", begin = 0.2, end = 0.8) +
  theme_bw() +
  labs(title = "Gulfwatch Seastar Density", y = "(mean) density/100m^2") +
  scale_x_continuous(breaks= pretty_breaks())







############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

df <- tibble(
  group = c(1:2, 1, 2),
  item_id = c(1:2, 2, 3),
  item_name = c("a", "a", "b", "b"),
  value1 = c(1, NA, 3, 4),
  value2 = 4:7
)
df


df %>% complete(group, item_id, item_name)
