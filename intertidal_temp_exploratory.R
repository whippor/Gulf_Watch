#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Gulfwatch intertidal community exploration                                     ##
# Script created 2023-12-08                                                      ##
# Data source: Alaska Gulf Watch                                                 ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2023-12-08                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:

# Script exploring rocky intertidal temperature data collected by Gulfwatch


# Required Files (check that script is loading latest version):
# allTempQAQC.csv


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
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)
library(lubridate)
library(ggpubr)


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

allTemps <- read_csv("~/git/Gulf_Watch/ProcessedData/ProcessedTempFull/allTempQAQC.csv", 
                     col_types = cols(date = col_date(format = "%m/%d/%Y"), 
                                      time = col_time(format = "%H:%M")))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# VISUALIZATIONS                                                               ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# how are air temps distributed through time and space?
allTemps %>%
  filter(block %notin% c("NPWS", "EPWS")) %>%
  filter(exposure == "air") %>%
  mutate(yrday = yday(date)) %>%
  mutate(yy = year(date)) %>%
  mutate(subzero = case_when(temperature <= 0 ~ "freeze",
                             temperature > 0 ~ "nofreeze"
                             )) %>%
  ggplot(aes(x = yrday, y = temperature, color = subzero)) +
  geom_point() +
  scale_color_viridis(discrete = TRUE, option = "D", begin = 0.3, end = 0.9) +
  facet_grid(yy~block)


# timing, frequency, and duration and sublethal/SCP for seastars and whelks
# by block

# duration years averaged seastar SCP
ss <- allTemps %>%
  filter(block %notin% c("NPWS", "EPWS")) %>%
  filter(temperature <= 0) %>%
  mutate(yy = year(date)) %>%
  filter(yy %in% c(2013:2022)) %>%
  mutate(mm = month(date, label = TRUE)) %>%
  mutate(morder = factor(mm, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                                        "Jan", "Feb", "Mar", "Apr", "May", "Jun"))) %>%
  mutate(seastar = case_when(temperature > -2.50 ~ "sublethal",
                             (temperature <= -2.50) & (temperature >= -7.99) ~ "SCP",
                             temperature <= -8.0 ~ "lethal")) %>%
  mutate(timeduration = 1) %>%
  group_by(block, site, yy, morder, seastar) %>%
  summarise(lengthtime = sum(timeduration)) %>%
  ungroup() %>%
  group_by(block, morder, seastar) %>%
  summarise(meantime = mean(lengthtime)) %>%
  mutate(increment = ((meantime*30)-29)/60) %>%
  ggplot() +
  geom_col(aes(x = morder, y = increment, fill = seastar)) +
  scale_fill_viridis(discrete = TRUE, option = "turbo", begin = 0, end = 0.35) +
  theme_bw() +
  labs(x = "Month", y = "Mean Hours") +
  #scale_x_date(date_labels = "%b") +
  facet_wrap(.~block, nrow = 1)



# duration years averaged whelk SCP
wh <- allTemps %>%
  filter(block %notin% c("NPWS", "EPWS")) %>%
  filter(temperature <= 0) %>%
  mutate(yy = year(date)) %>%
  filter(yy %in% c(2013:2022)) %>%
  mutate(mm = month(date, label = TRUE)) %>%
  mutate(morder = factor(mm, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                                        "Jan", "Feb", "Mar", "Apr", "May", "Jun"))) %>%
  mutate(whelk = case_when(temperature > -3.99 ~ "sublethal",
                           (temperature <= -3.99) & (temperature >= -7.99) ~ "SCP",
                           temperature <= -8.0 ~ "lethal")) %>%
  mutate(timeduration = 1) %>%
  group_by(block, site, yy, morder, whelk) %>%
  summarise(lengthtime = sum(timeduration)) %>%
  ungroup() %>%
  group_by(block, morder, whelk) %>%
  summarise(meantime = mean(lengthtime)) %>%
  mutate(increment = ((meantime*30)-29)/60) %>%
  ggplot() +
  geom_col(aes(x = morder, y = increment, fill = whelk)) +
  scale_fill_viridis(discrete = TRUE, option = "turbo", begin = 0, end = 0.35) +
  theme_bw() +
  labs(x = "Month", y = "Mean Hours") +
  #scale_x_date(date_labels = "%b") +
  facet_wrap(.~block, nrow = 1)


ggarrange(ss, wh, ncol = 1, nrow = 2, labels = c("A", "B"))

# How many SCP and lethal events occurred for each region and year?
SCPLethalCount <- allTemps %>%
  filter(block %notin% c("NPWS", "EPWS")) %>%
  filter(temperature <= -2.50) %>%
  mutate(yy = year(date)) %>%
  mutate(ss = semester(date)) %>%
  filter(yy %in% c(2013:2022)) %>% 
  mutate(number = 1) %>%
  group_by(block, yy, ss) %>%
  summarise(total = sum(number))

# filter out values less than 720 (avg of 2hr per day for Jan-Jun)
TwoHrCount <- SCPLethalCount %>%
  filter(total >= 720)
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
  

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####




# by day of year
allTemps %>%
  filter(block %notin% c("NPWS", "EPWS")) %>%
  filter(temperature <= 0) %>%
  mutate(yy = year(date)) %>%
  filter(yy %in% c(2013:2022)) %>%
  mutate(yearDay = yday(date)) %>%
  mutate(seastar = case_when(temperature > -2.50 ~ "sublethal",
                             (temperature <= -2.50) & (temperature >= -7.99) ~ "SCP",
                             temperature <= -8.0 ~ "lethal")) %>%
  mutate(timeduration = 1) %>%
  group_by(block, site, yy, yearDay, seastar) %>%
  summarise(lengthtime = sum(timeduration)) %>%
  ungroup() %>%
  group_by(block, yearDay, yy, seastar) %>%
  summarise(meantime = mean(lengthtime)) %>%
  mutate(increment = ((meantime*30)-29)/60) %>%
  ggplot() +
  geom_col(aes(x = yearDay, y = increment, fill = seastar)) +
  scale_fill_viridis(discrete = TRUE, option = "turbo", begin = 0, end = 0.35) +
  theme_bw() +
  labs(x = "Day of Year", y = "Mean Hours") +
  #scale_x_date(date_labels = "%b") +
  facet_grid(yy~block)











































