library(tidyverse)
library(viridis)

KBAYKATMKEFJWPWS_2007_2021_Soft_Sediment_Bivalve_Count <- read_csv("RawData/Bivalves/KBAYKATMKEFJWPWS_2007-2021_Soft_Sediment_Bivalve_Count.csv")


test <- KBAYKATMKEFJWPWS_2007_2021_Soft_Sediment_Bivalve_Count %>%
  pivot_longer(cols = c(Astarte_montagui:Unid_Mussel)) %>%
  group_by(YearSample, SiteName, name) %>%
  mutate(value = as.numeric(value)) %>%
  summarise(value =  sum(value))
test %>%
  filter(name %in% c( "Macoma_spp",
                     "Saxidomus_gigantea")) %>%
  ggplot(aes(x = YearSample, y = log10(value+1), color = name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis(discrete = TRUE,
                      option = "viridis",
                      begin = 0.3, end = 0.8) +
  theme_bw()



library(tidyverse)

url <- "http://varianceexplained.org/files/Brauer2008_DataSet1.tds"

# Clean and tidy the data
cleaned_data <- read_delim(url, delim = "\t") %>%
  separate(NAME, c("name", "BP", "MF", "systematic_name", "number"), sep = "\\|\\|") %>%
  mutate_at(vars(name:systematic_name), funs(trimws)) %>%
  select(-number, -GID, -YORF, -GWEIGHT) %>%
  gather(sample, expression, G0.05:U0.3) %>%
  separate(sample, c("nutrient", "rate"), sep = 1, convert = TRUE) %>%
  filter(!is.na(expression), systematic_name != "")

# Visualize a set of four genes
cleaned_data %>%
  filter(BP == "leucine biosynthesis") %>%
  ggplot(aes(rate, expression, color = nutrient)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~name + systematic_name)
