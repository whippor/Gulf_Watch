#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Gulfwatch intertidal community cover data QAQC                                 ##
# Script created 2023-12-01                                                      ##
# Data source: Alaska Gulf Watch                                                 ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2023-12-07                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:

# Script to join KBAY and other sites into single intertidal percent cover dataset
# with all current taxonomy


# Required Files (check that script is loading latest version):
# KATMKEFJWPWS_2006-2023_Rocky_Intertidal_Percent_Cover.csv
# KBAY2012-2023_Rocky_Intertidal_Percent_Cover.csv


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
library(lubridate)
library(GGally)
library(worrms)

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

# calculated percent cover of sessile organisms
KATMKEFJWPWS_cover <- read_csv("RawData/IntertidalCover/KATMKEFJWPWS_2006-2023_Rocky_Intertidal_Percent_Cover.csv")

# raw data, percent cover of sessile organisms
KBAY_cover <- read_csv("RawData/IntertidalCover/KBAY2012-2023_Rocky_Intertidal_Percent_Cover.csv", 
                       col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                        Replicate = col_character(), `Quadrat(m2)` = col_character()))


# join datasets together

# simplify KBAY and standardize names
K1 <- KBAY_cover %>% 
  select(SiteName = Site, 
         SampleDate = Date, 
         Year, 
         Quadrat_Num = Replicate, 
         Elevation_Position_raw = Stratum, 
         Species = ScientificName_accepted, 
         overstory = `%overstory`, 
         understory = `%understory`) %>%
  mutate(Percent_Cover_raw = understory + overstory, .keep = "unused") %>%
  mutate(Percent_Cover = case_when(Percent_Cover_raw > 100 ~ 100,
                                   TRUE ~ Percent_Cover_raw),
         .keep = "unused") %>%
  mutate(Block_Name = "KBAY", .before = SiteName) %>%
  mutate(Elevation_Position = case_when(Elevation_Position_raw == "High" ~ "Upper",
                                        Elevation_Position_raw == "Mid" ~ "Mid",
                                        Elevation_Position_raw == "Low" ~ "Low",
                                        Elevation_Position_raw == "-1 m" ~ "Sub",
                                        Elevation_Position_raw == "-1" ~ "Sub"),
         .keep = "unused", .before = Species) %>%
  mutate(Quadrat_Num = as.numeric(Quadrat_Num))

# add blocks to KATM dataset
A1 <- KATMKEFJWPWS_cover %>%
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
  select(-SiteID) %>%
  mutate(Elevation_Position = case_when(Elevation_Position == "Mid (0.5 m MLLW)" ~ "Mid",
                                        Elevation_Position == "Upper (1.5 m MLLW)" ~ "Upper"))


# join datasets together
allCover <- A1 %>%
  bind_rows(K1)

rm(A1, K1)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                              ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# how many species/categories identified in both surveys
KKW_cats <- unique(KATMKEFJWPWS_cover$Species)
KBY_cats <- unique(KBAY_cover$`Original field ID`)

# how many overlap?
overlap <- intersect(KKW_cats, KBY_cats)

# dataframe of all species/categories
a <- KKW_cats
length(a) <- 115
a <- tibble(a)
colnames(a) <- "KKW_cats"
b <- tibble(KBY_cats)

dat1_2 <- a %>%
  mutate(ID1 = KKW_cats) %>%
  group_by(KKW_cats) %>%
  mutate(ID2 = row_number()) %>%
  ungroup()

dat2_2 <- b %>% 
  mutate(ID1 = KBY_cats) %>%
  group_by(KBY_cats) %>%
  mutate(ID2 = row_number()) %>%
  ungroup()

all_species <- full_join(dat1_2, dat2_2, by = c("ID1", "ID2")) %>%
  select(-starts_with("ID")) %>%
  arrange(KBY_cats)

rm(a,b,dat1_2,dat2_2)


# check all names in WORMS database and join taxonomy

sp_list <- c(KKW_cats, KBY_cats)
sp_list <- unique(sp_list)

TaxWorms <- wm_records_names(name = c(sp_list))
TaxWormsTib <- data.table::rbindlist(TaxWorms)

# find unaccepted names in the dataset and replace
unaccepted <- TaxWormsTib %>%
  filter(status == "unaccepted")

allCover_update <- allCover %>%
  mutate(newSp = case_when(Species == "Corallina frondescens" ~ "Bossiella frondescens",
                           Species == "Pachyarthron cretaceum" ~ "Corallina officinalis",
                           Species == "Colpomenia bullosa" ~ "Dactylosiphon bullosus",
                           Species == "Saccharina sessilis" ~ "Hedophyllum sessile",
                           Species == "Eurystomella bilabiata" ~ "Integripelta bilabiata",
                           Species == "Pododesmus macroschisma" ~ "Pododesmus macrochisma",
                           Species == "Neoptilota asplenioides" ~ "Ptilota asplenioides",
                           Species == "Saccharina subsimplex" ~ "Saccharina latissima",
                           Species == "Polyostea bipinnata" ~ "Savoiea bipinnata",
                           Species == "Pterosiphonia bipinnata" ~ "Savoiea bipinnata",
                           Species == "Scagelia occidentale" ~ "Scagelia americana",
                           Species == "Stomachetosella cruenta" ~ "Stomacrustula cruenta",
                           Species == "Polysiphonia\xa0sp." ~ "Polysiphonia sp.",
                           TRUE ~ Species), .keep = "unused") %>%
  mutate(Species = newSp, .before = Percent_Cover, .keep = "unused")

# final table with correct taxonomy
TaxWorms_final <- wm_records_names(name = c(unique(allCover_update$Species)))
TaxWormsTib_final <- data.table::rbindlist(TaxWorms_final)

tax_join <- TaxWormsTib_final %>%
  select(valid_name, valid_AphiaID, kingdom:genus) %>%
  mutate(Species = valid_name, .before = valid_AphiaID) %>%
  distinct(.keep_all = TRUE) %>%
  filter(valid_AphiaID != 325747)
# add taxonomy to common names
common_frame <- as_tibble(c("Species", "AphiaID", "kingdom", "phylum", "class", "order", "family", "genus"))
common_wide <- common_frame %>%
  pivot_wider(names_from = value)
common_wide <- common_wide %>%
  rbind(c("barnacle", "NA", "Animalia", "Arthropoda", "Thecostraca", "NA", "NA", "NA")) %>%
  rbind(c("non-coralline algal crust", "NA", "Plantae", "NA", "NA", "NA", "NA", "NA")) %>%
  rbind(c("unidentified sponge", "NA", "Porifera", "NA", "NA", "NA", "NA")) %>%
  rbind(c("Boreophyllum / Pyropia / Wildemania spp.", "NA", "Plantae", "Rhodophyta", "Bangiophyceae", "Bangiales", "Bangiaceae", "NA")) %>%
  rbind(c("encrusting coralline algae", "NA", "Plantae", "Rhodophyta", "Florideophyceae", "Corallinales", "NA", "NA")) %>%
  rbind(c("Palmaria hecatensis/Devaleraea mollis", "NA", "Plantae", "Rhodophyta", "Florideophyceae", "Palmariales", "Palmariaceae", "NA")) %>%
  rbind(c("encrusting bryozoan", "NA", "Bryozoa", "NA", "NA", "NA", "NA", "NA")) %>%
  rbind(c("Spirorbidae", "NA", "Animalia", "Annelida", "Polychaeta", "Sabellida", "Spirorbinae", "NA")) %>%
  rbind(c("Ptilota asplenoides", "NA", "Plantae", "Rhodophyta", "Florideophyceae", "Ceramiales", "Wrangeliaceae", "Ptilota")) %>%
  rbind(c("unidentified filamentous red algae", "NA", "Plantae", "Rhodophyta", "NA", "NA", "NA", "NA")) %>%
  rbind(c("unidentified anemone", "NA", "Animalia", "Cnidaria", "Hexacorallia", "Actinaria", "NA", "NA")) %>%
  rbind(c("barnacle spat", "NA", "Animalia", "Arthropoda", "Thecostraca", "NA", "NA", "NA")) %>%
  rbind(c("Blidingia minima var. minima", "NA", "Plantae", "Chlorophyta", "Ulvophyceae", "Ulvales", "Kornmanniaceae", "Blidingia")) %>%
  rbind(c("foliose coralline algae", "NA", "Plantae", "Rhodophyta", "Florideophyceae", "Corallinales", "NA", "NA")) %>%
  rbind(c("unidentified brown algae", "NA", "Chromista", "Heterokontophyta", "NA", "NA", "NA", "NA")) %>%
  rbind(c("unidentified green algae", "NA", "Plantae", "Chlorophyta", "NA", "NA", "NA", "NA")) %>%
  rbind(c("unidentified tunicate", "NA", "Animalia", "Chordata", "Ascidiacea", "NA", "NA", "NA")) %>%
  rbind(c("unidentified hydroid", "NA", "Animalia", "Cnidaria", "Hydrozoa", "NA", "NA", "NA")) %>%
  rbind(c("unidentified worm", "NA", "Animalia", "NA", "NA", "NA", "NA", "NA")) %>%
  rbind(c("foliose bryozoan", "NA", "Bryozoa", "NA", "NA", "NA", "NA", "NA")) %>%
  rbind(c("unidentified red blade algae", "NA", "Plantae", "Rhodophyta", "NA", "NA", "NA", "NA")) %>%
  rbind(c("Polysiphonia sp.", "NA", "Plantae", "Rhodophyta", "Florideophyceae", "Ceriamiales", "Rhodomelaceae", "Polysiphonia")) %>%
  rbind(c("Hydroidea", "NA", "Animalia", "Cnidaria", "Hydrozoa", "NA", "NA", "NA")) %>%
  rbind(c("Pyropia/Boreophyllum complex", "NA", "Plantae", "Rhodophyta", "Bangiophyceae", "Bangiales", "Bangiaceae", "NA")) %>%
  rbind(c("Bangia sp.", "NA", "Plantae", "Rhodophyta", "Bangiophyceae", "Bangiales", "Bangiaceae", "NA")) %>%
  filter(!row_number() == 1) %>%
  mutate(valid_AphiaID = as.numeric(AphiaID), .after = Species, .keep = "unused")
tax_join_final <- tax_join %>%
  bind_rows(common_wide)


# proper taxonomy now included for all valid taxonomic identifications
allCover_tax <- allCover_update %>%
  left_join(tax_join_final, by = "Species") %>%
  select(-valid_name) %>%
  mutate(PercentCover = Percent_Cover, .after = "genus", .keep = "unused") %>%
  mutate(Percent_Cover = PercentCover, .after = "genus", .keep = "unused") 


# WRITE CSV OUPUT
# write_csv(allCover_tax, "~/git/Gulf_Watch/ProcessedData/ProcessedIntertidal/allCoverQAQC.csv")

rm(allCover_update, tax_join, unaccepted, TaxWormsTib, TaxWorms, TaxWorms_final, common_frame, common_wide, tax_join_final, TaxWormsTib_final)