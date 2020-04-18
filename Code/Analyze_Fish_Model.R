#Checking working directory
getwd()

# Loading packages
library(tidyverse)
library(GGally)
library(nlme)
library(piecewiseSEM)
library(MASS)

# Setting ggplot theme
deftheme <- theme_classic(base_size = 14) + 
  theme(axis.text = element_text(color = "black"),
        legend.position = "right") 
theme_set(deftheme)

# Loading datasets
GBR_Sites_Processed_2004 <- read.csv("./Data/Processed/data.gov.au_fish_benthos_GBR_sites_2004_processed.csv")
GBR_Sites_Processed_1999 <- read.csv("./Data/Processed/data.gov.au_fish_benthos_GBR_sites_1999_processed.csv")

#Looking at column names; changing Year to a factor
colnames(GBR_Sites_Processed_2004)
str(GBR_Sites_Processed_2004)
GBR_Sites_Processed_2004$Year <- as.factor(GBR_Sites_Processed_2004$Year)

#Examining normality of dependent variable (LHC_mean)
#######Is this sufficient to test for normality?
ggpairs(GBR_Sites_Processed_2004, columns = c(1, 3, 9, 10, 11, 12))

#Testing for equal variances
##All population variances are not equal
bartlett.test(GBR_Sites_Processed_2004$Total.Fish.Densit_mean ~ GBR_Sites_Processed_2004$Zone)

########Continuing with model -- ANCOVA robust to departures from equal variance?

#Model selection
Fish_mixed_1 <- lme(data = GBR_Sites_Processed_2004, log(Total.Fish.Densit_mean) ~ Year + Zone + LHC_mean + 
                     LCC_mean + SCI_mean + MAC_mean, random = ~1|Site, method = "ML")

summary(Fish_mixed_1)
rsquared(Fish_mixed_1)
AIC(Fish_mixed_1)

stepAIC(Fish_mixed_1)
#Final Model: Total.Fish.Densit_mean ~ Year + LHC_mean + MAC_mean, random = ~1|Site

Fish_mixed_final <- lme(data = GBR_Sites_Processed_2004, log(Total.Fish.Densit_mean) ~ Year + LHC_mean, random = ~1|Site, method = "ML")

summary(Fish_mixed_final)
rsquared(Fish_mixed_final)
AIC(Fish_mixed_final)

#Checking model fit with residuals vs. fitted plot
#The line is approximately at zero and the points are evenly distributed around it; 
#no drastic asymmetry
plot(Fish_mixed_final)

co

eq_2004 <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1]}
eq_2006 <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1] + fixef(Fish_mixed_final)[2]}
eq_2007 <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1] + fixef(Fish_mixed_final)[3]}
eq_2008 <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1] + fixef(Fish_mixed_final)[4]}
eq_2009 <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1] + fixef(Fish_mixed_final)[5]}
eq_2011 <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1] + fixef(Fish_mixed_final)[6]}
eq_2012 <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1] + fixef(Fish_mixed_final)[7]}
eq_2013 <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1] + fixef(Fish_mixed_final)[8]}
eq_2014 <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1] + fixef(Fish_mixed_final)[9]}

ggplot(GBR_Sites_Processed_2004) + 
  aes (y = log(Total.Fish.Densit_mean), x = LHC_mean, color = Year) +
  geom_point() +
  scale_color_viridis_d(direction = -1) +
  labs(x = "Mean live hard coral % cover", 
       y = expression('Ln mean total fish density (per 1000 m'^"2"*')'),
       color = "") +
  #geom_smooth(method = lm, se = FALSE),
  stat_function(fun = eq_2004, color = "#FDE725FF", size = 1) + 
  stat_function(fun = eq_2006, color = "#AADC32FF", size = 1) +
  stat_function(fun = eq_2007, color = "#5DC863FF", size = 1) +
  stat_function(fun = eq_2008, color = "#27AD81FF", size = 1) +
  stat_function(fun = eq_2009, color = "#21908CFF", size = 1) +
  stat_function(fun = eq_2011, color = "#2C728EFF", size = 1) +
  stat_function(fun = eq_2012, color = "#3B528BFF", size = 1) +
  stat_function(fun = eq_2013, color = "#472D7BFF", size = 1) +
  stat_function(fun = eq_2014, color = "#440154FF", size = 1) 
