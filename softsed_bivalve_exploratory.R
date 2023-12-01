#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Soft sediment bivalve exploratory                                              ##
# Script created 2023-08-02                                                      ##
# Data source: Alaska Gulf Watch                                                 ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2023-08-02                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:

# Script exploring soft sediment bivalve data collected by Gulf Watch Alaska between
# 2007-2021


# Required Files (check that script is loading latest version):
# KBAYKATMKEFJWPWS_2007-2021_Soft_Sediment_Bivalve_Count.csv

# Associated Scripts:
# NONE

# TO DO 

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

counts_wide <-  read_csv("Data/KBAYKATMKEFJWPWS_2007-2021_Soft_Sediment_Bivalve_Count.csv", 
                         col_types = cols(YearSample = col_character()))

str(counts_wide)

counts_long <- counts_wide %>%
  pivot_longer(Mytilus_trossulus:Unid_Mussel, names_to = 'species', values_to = 'count') %>%
  filter(count != '.') %>%
  mutate(count = as.numeric(count))

str(counts_long)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                              ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# plot mean abundance per quadrat for each site by year for littlenecks and butter clams
count_summary <- counts_long %>%
#  filter(SiteName == 'Jakalof Bay') %>%
  filter(species %in% c('Leukoma_staminea', 'Saxidomus_gigantea')) %>%
  summarySE(measurevar="count", groupvars=c("species","YearSample", "SiteName"))

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

# Use 95% confidence interval instead of SEM
count_summary %>%
  ggplot(aes(x=YearSample, y=count, colour=species)) + 
  scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.6) +
  geom_errorbar(aes(ymin=count-ci, ymax=count+ci), width=.2, position=pd, size = 1) +
  geom_line(position=pd,aes(group=species), linewidth = 1) +
  geom_point(position=pd) +
  theme_bw() +
  facet_wrap('SiteName', scales = 'free')

# KBAY ONLY
counts_long %>%
  filter(SiteName %in% c("Bear Cove", 
                         "China Poot Bay",
                         "Jakalof Bay",
                         "Port Graham")) %>%
  ggplot(aes(x = YearSample, y = count, color = SiteName)) +
  stat_summary(fun = "mean")
  

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####
otterprey <- read_csv("C:/Users/rossw/Desktop/KBAYKATMKEFJPWS_2012-2021_Sea_Otter_Foraging_Observations.csv")
otterprey <- as_tibble(otterprey)

ottersums <- otterprey %>%
  group_by(preytype_cd) %>%
  summarise(quantity = sum(prey_qty, na.rm = TRUE))
ottersums <- rename(ottersums, abbv = preytype_cd)

library(xml2)

# parse out species abbreviations
otter_abbvs <- xmlParse('C:/Users/rossw/Desktop/KBAYKATMKEFJPWS_2012-2021_Sea_Otter_Foraging_Observations_Metadata.xml')
otter_abbvs_tib <- as_tibble(xpathSApply(otter_abbvs, '//common', xmlValue))
otter_split <- otter_abbvs_tib %>%
  filter(grepl("; ",value))
otter_split_list <- otter_split %>%
  separate(value, c("species", "abbv"), ";")  
otter_split_list <- otter_split_list %>%
  add_row(species = 'any clam', abbv = 'cla') %>%
  add_row(species = 'bivalve', abbv = 'biv') %>%
  add_row(species = 'unidentified', abbv = 'uni') %>%
  add_row(species = 'scallop', abbv = 'sca') %>%
  add_row(species = 'unid prey on algae', abbv = 'alg') %>%
  add_row(species = 'mussel', abbv = 'msc') %>%
  add_row(species = 'katy chiton', abbv = 'kat') %>%
  add_row(species = 'other', abbv = 'oth') %>%
  add_row(species = 'horsemussel', abbv = 'mom') %>%
  add_row(species = 'Pacific oyster', abbv = 'crg') %>%
  add_row(species = 'skate egg case', abbv = 'egc') %>%
  add_row(species = 'whelk', abbv = 'nus') %>%
  add_row(species = 'Pandalus shrimp', abbv = 'pas') %>%
  add_row(species = 'whelk', abbv = 'nes') %>%
  add_row(species = 'Pacific lyre crab', abbv = 'hyl') %>%
  add_row(species = 'Leptasterias', abbv = 'les') %>%
  add_row(species = 'red king crab', abbv = 'pac') %>%
  add_row(species = 'moonsnail', abbv = 'eul') %>%
  add_row(species = 'sea peach', abbv = 'haa') %>%
  add_row(species = 'hermit crab', abbv = 'pag') 
otter_split_list <- otter_split_list %>%
  filter(species %notin% c('skates', 'clams')) %>%
  filter(abbv %notin% ' former name Crassostrea gigas') %>%
  filter(duplicated(abbv) == FALSE) 
otter_split_list$abbv <-  trimws(otter_split_list$abbv, which = "both")

# join abbvs to ottersums
otter_final <- ottersums %>%
  left_join(otter_split_list, by = 'abbv')

# site names
list(unique(counts_long$SiteName))
