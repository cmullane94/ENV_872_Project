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
ggpairs(GBR_Sites_Processed_2004, columns = c(1, 3, 7:8, 11, 12))

#Testing for equal variances
##All population variances are equal
bartlett.test(GBR_Sites_Processed_2004$LHC_mean ~ GBR_Sites_Processed_2004$Year)


##All population variances are not equal
bartlett.test(GBR_Sites_Processed_2004$LHC_mean ~ GBR_Sites_Processed_2004$Zone)

########Continuing with model -- ANCOVA robust to departures from equal variance?

#Model selection
#Take out year?
LHC_mixed_1 <- lme(data = GBR_Sites_Processed_2004, LHC_mean ~ Year + Zone + Grazers_mean + 
                   Corallivores_mean + MAC_mean, random = ~1|Site)

summary(LHC_mixed_1)
rsquared(LHC_mixed_1)
AIC(LHC_mixed_1)

LHC_mixed_2 <- update(LHC_mixed_1, .~.-Zone)

summary(LHC_mixed_2)
rsquared(LHC_mixed_2)
AIC(LHC_mixed_2)

LHC_mixed_3 <- update(LHC_mixed_2, .~.-Grazers_mean)

summary(LHC_mixed_3)
rsquared(LHC_mixed_3)
AIC(LHC_mixed_3)

LHC_mixed_4 <- update(LHC_mixed_3, .~.-MAC)

summary(LHC_mixed_4)
rsquared(LHC_mixed_4)
AIC(LHC_mixed_4)

#Likelihood ratio test to 
lrtest(LHC_mixed_1, LHC_mixed_2)
lrtest(LHC_mixed_1, LHC_mixed_3)
lrtest(LHC_mixed_1, LHC_mixed_4)

#Checking model fit with residuals vs. fitted plot
#The line is approximately at zero and the points are evenly distributed around it; 
#no drastic asymmetry
plot(LHC_mixed_1)

#Plotting analysis results
######ablines?
ggplot(GBR_Sites_Processed_2004) + 
  aes(y = LHC_mean, color = MAC_mean, x = Corallivores_mean) +
  geom_point() +
  ylim(0, 100) +
  facet_grid(Year ~ Zone)

ggplot(GBR_Sites_Processed_2004) + 
  aes(y = LHC_mean, color = MAC_mean, x = Grazers_mean) +
  geom_point() +
  ylim(0, 100) +
  facet_grid(Year ~ Zone)

