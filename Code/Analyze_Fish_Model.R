#Checking working directory
getwd()

# Loading packages
library(tidyverse)
library(GGally)
library(nlme)
library(lmtest)

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
ggpairs(GBR_Sites_Processed_2004, columns = c(3, 9, 10, 11, 12))

#Testing for equal variances
##All population variances are not equal
bartlett.test(GBR_Sites_Processed_2004$Total.Fish.Densit_mean ~ GBR_Sites_Processed_2004$Zone)

########Continuing with model -- ANCOVA robust to departures from equal variance?

#Model selection
#Take out year?
Fish_mixed_1 <- lme(data = GBR_Sites_Processed_2004, Total.Fish.Densit_mean ~ Zone + LHC_mean + 
                     LCC_mean + SCI_mean + MAC_mean, random = ~1|Site)

summary(Fish_mixed_1)
rsquared(Fish_mixed_1)
AIC(Fish_mixed_1)

Fish_mixed_2 <- update(Fish_mixed_1 , .~.-LCC_mean)

summary(Fish_mixed_2)
rsquared(Fish_mixed_2)
AIC(Fish_mixed_2)

Fish_mixed_3 <- update(Fish_mixed_2 , .~.-Zone)

summary(Fish_mixed_3)
rsquared(Fish_mixed_3)
AIC(Fish_mixed_3)

Fish_mixed_4 <- update(Fish_mixed_3 , .~.-SCI_mean)

summary(Fish_mixed_4)
rsquared(Fish_mixed_4)
AIC(Fish_mixed_4)

lrtest(Fish_mixed_1, Fish_mixed_2)
lrtest(Fish_mixed_1, Fish_mixed_3)
lrtest(Fish_mixed_1, Fish_mixed_4)
