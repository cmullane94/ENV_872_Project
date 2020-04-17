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

#This may include, but is not limited to, graphs illustrating the distributions of 
#variables of interest

#Scope: think about what information someone might want to know about the dataset 
#before analyzing it statistically. How might you visualize this information?

#Time series scatterplots
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
ggplot(GBR_Sites) + aes(y = LHC_mean) +
  geom_boxplot()

ggplot(GBR_Sites) + aes(y = LCC_mean) +
  geom_boxplot()

###Both LHC_mean and LCC_mean are percentages, but they each have outliers over 100

ggplot(GBR_Sites) + aes(y = Total.Fish.Densit_mean) +
  geom_boxplot()

ggplot(GBR_Sites) + aes(y = Fish.Species.richness_mean) +
  geom_boxplot()


  







