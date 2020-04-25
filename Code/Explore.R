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

colnames(GBR_Sites)
colnames(GBR_Zones)

str(GBR_Sites)
str(GBR_Zones)

dim(GBR_Sites)
dim(GBR_Zones)

summary(GBR_Sites$LHC_mean)
summary(GBR_Sites$LCC_mean)
summary(GBR_Sites$Total.Fish.Densit_mean)
summary(GBR_Sites$Fish.Species.richness_mean)

#Note: Only the plots used in the final report were finalized. 
#Polished exploratory plots
#Number of zones
zones_explore <- ggplot(GBR_Sites) + aes(x = Zone) +
  geom_bar() +
  labs(y = "Frequency")

print(zones_explore)

#Frequency graph of coral and macroalgae
LHC_MAC_explore <- ggplot(GBR_Sites_Processed_1999) +
  geom_freqpoly(aes(x = LHC_mean, color = "Living hard coral")) + 
  geom_freqpoly(aes(x = MAC_mean, color = "Fleshy macroalgae")) +
  scale_color_viridis_d() +
  labs(x = "Mean % cover", y = "Frequency", color = "")

print(LHC_MAC_explore)

#Coral vs. macroalgae
coral_mac_scatter <- ggplot(GBR_Sites_Processed_1999) + 
  aes(x = MAC_mean, y = LHC_mean) +
  geom_point() + 
  labs(y = "Mean live hard coral % cover", x = "Mean fleshy macroalgae % cover")
print(coral_mac_scatter)

#Fish density and coral coverage over time
LHC_year_explore <- ggplot(GBR_Sites_Processed_2004) + aes(x = Year, y = LHC_mean) +
  geom_point() +
  labs(y = "Mean live hard coral % cover") +
  theme(axis.title.y = element_text(size = 9))

Fish_year_explore <- ggplot(GBR_Sites_Processed_2004) + 
  aes(x = Year, y = Total.Fish.Densit_mean) +
  geom_point() +
  labs(y = expression('Mean total fish density (per 1000 m'^"2"*')')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.y = element_text(size = 9))

plot_grid(Fish_year_explore, LHC_year_explore, align = "v", ncol = 1)

#Fish vs. coral
coral_fish_scatter <- ggplot(GBR_Sites_Processed_2004) + 
  aes(x = LHC_mean, y = Total.Fish.Densit_mean) +
  geom_point() + 
  labs(x = "Mean live hard coral % cover", 
       y = expression('Mean total fish density (per 1000 m'^"2"*')'))
print(coral_fish_scatter)

#Unpolished exploratory plots
#Year scatterplots
ggplot(GBR_Sites) + aes(x = Year, y = LHC_mean) +
 geom_point()

ggplot(GBR_Sites) + aes(x = Year, y = LCC_mean) +
  geom_point()

ggplot(GBR_Sites) + aes(x = Year, y = Total.Fish.Densit_mean) +
  geom_point()

ggplot(GBR_Sites) + aes(x = Year, y = Fish.Species.richness_mean) +
  geom_point()

#Scatterplot showing mean fish density in relation to mean % hard coral cover
ggplot(GBR_Sites) + aes(x = LHC_mean, y = Total.Fish.Densit_mean) +
  geom_point() + 
  xlim(0, 100)

#Bar graph showing the number of each type of zones
ggplot(GBR_Sites) + aes(x = Zone) +
  geom_bar()

#Histograms of % hard coral cover, % total coral cover, % mean fish density, and 
#mean fish species richness
ggplot(GBR_Sites) + aes(x = LHC_mean) +
  geom_histogram()

ggplot(GBR_Sites) + aes(x = LCC_mean) +
  geom_histogram()

ggplot(GBR_Sites) + aes(x = Total.Fish.Densit_mean) +
  geom_histogram()

ggplot(GBR_Sites) + aes(x = Fish.Species.richness_mean) +
  geom_histogram()

#Frequency polygon graph showing % hard and total coral cover
ggplot(GBR_Sites) +
  geom_freqpoly(aes(x = LHC_mean)) + 
  geom_freqpoly(aes(x = LCC_mean))

#Boxplots of % hard coral cover, % total coral cover, % mean fish density, and 
#mean fish species richness
ggplot(GBR_Sites) + aes(y = LHC_mean, x = Zone) +
  geom_boxplot()

ggplot(GBR_Sites) + aes(y = LCC_mean) +
  geom_boxplot()

###Both LHC_mean and LCC_mean are percentages, but they each have outliers over 100

ggplot(GBR_Sites) + aes(y = Total.Fish.Densit_mean, x = Zone) +
  geom_boxplot()

ggplot(GBR_Sites) + aes(y = Fish.Species.richness_mean, x = Zone) +
  geom_boxplot()


  







