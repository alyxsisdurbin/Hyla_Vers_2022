# Haley Durbin
# 2023-06-07
# Miami REU Models

#load in the data from the excel sheet
library(readxl)

library(tidyverse)
library(rstatix) # for levene_test
library(knitr)
library(GGally) # ggpairs()
library(ggiraph)
library(ggiraphExtra)

####data frame of data 

hyla_water = read_excel("Durbin_Hyla_Data.xlsx", sheet="Water Quality")
head(hyla_water)

hyla_frogs = read_excel("Durbin_Hyla_Data.xlsx", sheet="Metamorph Collection")
names(hyla_frogs) = c("collected", "weight_date", "pond", "mass_g", "day0", "age")
hyla_frogs$mass_g = as.numeric(hyla_frogs$mass_g)
head(hyla_frogs)


#organizing data


sort_ponds <- hyla_frogs %>%
  select(pond, age, mass_g) %>% 
  mutate(mass_g = as.numeric(mass_g)) %>%
  group_by(pond) %>%
  summarise(mmass = mean(mass_g, na.rm = TRUE), mmday = mean(age, na.rm = TRUE))
head(sort_ponds)
  #add_column(nutrients_g=NA, density=NA) %>%
  
sort_ponds$pop_density = ifelse(sort_ponds$pond %in% c(120,180,128,116,185,171,179,152,129,114,110,167,134,156,190),"low density","high density")
sort_ponds$leaf_litter = ifelse(sort_ponds$pond %in% c(150,128,105,185,106,129,138,114,134,192),0.25,
                                ifelse(sort_ponds$pond %in% c(180,197,116,121,107,179,110,193,126,190),0.5,
                                       ifelse(sort_ponds$pond %in% c(120,173,171,119,136,152,167,133,156,135),1,0)))
head(sort_ponds)

lwide = sort_ponds %>%
  pivot_wider(names_from=pond, values_from=)
# head(long)



test = ggplot(data=sort_ponds) +
  geom_point(aes(x=age, y=mass_g))




