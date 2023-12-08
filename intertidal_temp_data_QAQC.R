#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Gulfwatch intertidal tempertaure data QAQC                                     ##
# Script created 2023-12-07                                                      ##
# Data source: Alaska Gulf Watch                                                 ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2023-12-07                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:

# Script to join all intertidal hobo temperature data across all sites


# Required Files (check that script is loading latest version):
# Intertidal_Temperature_EPWS_2012-2013.csv
# Intertidal_Temperature_EPWS_2013-2014.csv
# Intertidal_Temperature_EPWS_2014-2015.csv
# Intertidal_Temperature_EPWS_2015-2016.csv
# Intertidal_Temperature_EPWS_2016-2017.csv
# Intertidal_Temperature_KATM_2006-2007.csv
# Intertidal_Temperature_KATM_2007-2008.csv
# Intertidal_Temperature_KATM_2008-2009.csv
# Intertidal_Temperature_KATM_2009-2010.csv
# Intertidal_Temperature_KATM_2010-2011.csv
# Intertidal_Temperature_KATM_2011-2012.csv
# Intertidal_Temperature_KATM_2012-2013.csv
# Intertidal_Temperature_KATM_2013-2014.csv
# Intertidal_Temperature_KATM_2014-2015.csv
# Intertidal_Temperature_KATM_2015-2016.csv
# Intertidal_Temperature_KATM_2016-2017.csv
# Intertidal_Temperature_KATM_2017-2018.csv
# Intertidal_Temperature_KATM_2018-2019.csv
# Intertidal_Temperature_KATM_2019-2020.csv
# Intertidal_Temperature_KATM_2020-2021.csv
# Intertidal_Temperature_KATM_2021_2022.csv
# Intertidal_Temperature_KATM_2022_2023.csv
# Intertidal_Temperature_KBAY_2012-2013.csv
# Intertidal_Temperature_KBAY_2013-2014.csv
# Intertidal_Temperature_KBAY_2014-2015.csv
# Intertidal_Temperature_KBAY_2015-2016.csv
# Intertidal_Temperature_KBAY_2016-2017.csv
# Intertidal_Temperature_KBAY_2017-2018.csv
# Intertidal_Temperature_KBAY_2018-2019.csv
# Intertidal_Temperature_KBAY_2019-2020.csv
# Intertidal_Temperature_KBAY_2020-2021.csv
# Intertidal_Temperature_KBAY_2021_2022.csv
# Intertidal_Temperature_KBAY_2022_2023.csv
# Intertidal_Temperature_KEFJ_2007-2008.csv
# Intertidal_Temperature_KEFJ_2008-2009.csv
# Intertidal_Temperature_KEFJ_2009-2010.csv
# Intertidal_Temperature_KEFJ_2010-2011.csv
# Intertidal_Temperature_KEFJ_2011-2012.csv
# Intertidal_Temperature_KEFJ_2012-2013.csv
# Intertidal_Temperature_KEFJ_2013-2014.csv
# Intertidal_Temperature_KEFJ_2014-2015.csv
# Intertidal_Temperature_KEFJ_2015-2016.csv
# Intertidal_Temperature_KEFJ_2016-2017.csv
# Intertidal_Temperature_KEFJ_2017-2018.csv
# Intertidal_Temperature_KEFJ_2018-2019.csv
# Intertidal_Temperature_KEFJ_2019-2020.csv
# Intertidal_Temperature_KEFJ_2020-2021.csv
# Intertidal_Temperature_KEFJ_2021_2022.csv
# Intertidal_Temperature_KEFJ_2022_2023.csv
# Intertidal_Temperature_NPWS_2012-2013.csv
# Intertidal_Temperature_NPWS_2013-2014.csv
# Intertidal_Temperature_NPWS_2014-2015.csv
# Intertidal_Temperature_NPWS_2015-2016.csv
# Intertidal_Temperature_NPWS_2016-2017.csv
# Intertidal_Temperature_WPWS_2010-2011.csv
# Intertidal_Temperature_WPWS_2011-2012.csv
# Intertidal_Temperature_WPWS_2012-2013.csv
# Intertidal_Temperature_WPWS_2013-2014.csv
# Intertidal_Temperature_WPWS_2014-2015.csv
# Intertidal_Temperature_WPWS_2015-2016.csv
# Intertidal_Temperature_WPWS_2016-2017.csv
# Intertidal_Temperature_WPWS_2017-2018.csv
# Intertidal_Temperature_WPWS_2018-2019.csv
# Intertidal_Temperature_WPWS_2019-2020.csv
# Intertidal_Temperature_WPWS_2020-2021.csv
# Intertidal_Temperature_WPWS_2021_2022.csv
# Intertidal_Temperature_WPWS_2022_2023.csv


# Associated Scripts:
# NONE


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
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

# import and append all csv's at once!
library(data.table)

setwd("RawData/Temperature")
allTemps <- 
  list.files(pattern = "\\.csv$") %>% 
  map_df(~fread(.))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                              ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# export CSV
write_csv(allTemps, "C:/Users/Ross.Whippo/Documents/git/Gulf_Watch/ProcessedData/allTempQAQC.csv")



# SCRATCH PAD ####

