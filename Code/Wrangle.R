#Checking working directory
getwd()

# Loading packages
library(tidyverse)

# Loading datasets
GBR_Sites <- read.csv("./Data/Raw/data.gov.au_fish_benthos_GBR_sites_raw.csv")
GBR_Zones <- read.csv("./Data/Raw/data.gov.au_fish_benthos_GBR_zones_raw.csv")

#Looking at column names to help with column selection
colnames (GBR_Sites)

#Selecting needed columns, filtering out % cover values of over 100%, 
#and omitting NAs
GBR_Sites_Processed_2004 <- GBR_Sites %>%
  select(Year:Site, Total.Fish.Densit_mean, Fish.Species.richness_mean, 
         Grazers_mean, Corallivores_mean, SCI_mean, LCC_mean, LHC_mean, MAC_mean) %>%
  filter(LCC_mean <= 100 & LHC_mean <= 100) %>%
  na.omit()

GBR_Sites_Processed_1999 <- GBR_Sites %>%
  select(Year:Site, Total.Fish.Densit_mean, Fish.Species.richness_mean, 
         Grazers_mean, Corallivores_mean, SCI_mean, LCC_mean, LHC_mean, MAC_mean) %>%
  filter(LCC_mean <= 100 & LHC_mean <= 100)

#Saving processed datasets
write.csv(GBR_Sites_Processed_2004, row.names = FALSE, 
          file = "./Data/Processed/data.gov.au_fish_benthos_GBR_sites_2004_processed.csv")

write.csv(GBR_Sites_Processed_1999, row.names = FALSE, 
          file = "./Data/Processed/data.gov.au_fish_benthos_GBR_sites_1999_processed.csv")
