#Checking working directory
getwd()

# Loading packages
library(tidyverse)

# Setting ggplot theme
deftheme <- theme_classic(base_size = 14) + 
  theme(axis.text = element_text(color = "black"),
        legend.position = "right") 
theme_set(deftheme)

# Loading datasets
GBR_Sites <- read.csv("./Data/Raw/data.gov.au_fish_benthos_GBR_sites_raw.csv")
GBR_Zones <- read.csv("./Data/Raw/data.gov.au_fish_benthos_GBR_zones_raw.csv")