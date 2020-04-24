#Checking working directory
getwd()

# Loading packages
library(tidyverse)
library(GGally)
library(nlme)
library(piecewiseSEM)
library(MASS)
require(emmeans)
require(multcomp)

# Setting ggplot theme
deftheme <- theme_classic(base_size = 14) + 
  theme(axis.text = element_text(color = "black"),
        legend.position = "top") 
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
ggpairs(GBR_Sites_Processed_2004, columns = c(1, 3, 7, 8, 11, 12))

#Testing for equal variances
##All population variances are equal
bartlett.test(GBR_Sites_Processed_2004$LHC_mean ~ GBR_Sites_Processed_2004$Year)


##All population variances are not equal
bartlett.test(GBR_Sites_Processed_2004$LHC_mean ~ GBR_Sites_Processed_2004$Zone)

########Continuing with model -- ANCOVA robust to departures from equal variance?

#Model selection
#Take out year?
LHC_mixed_1 <- lme(data = GBR_Sites_Processed_2004, LHC_mean ~ Year + Zone + Grazers_mean + 
                   Corallivores_mean + MAC_mean, random = ~1|Site, method = "ML")

summary(LHC_mixed_1)
rsquared(LHC_mixed_1)
AIC(LHC_mixed_1)

stepAIC(LHC_mixed_1)
#Final model: LHC_mean ~ Year + Grazers_mean + Corallivores_mean + MAC_mean, random = ~1|Site
#AIC = 3606.53

LHC_mixed_final <- lme(data = GBR_Sites_Processed_2004, LHC_mean ~ Year + Grazers_mean + 
                         Corallivores_mean + MAC_mean, random = ~1|Site, method = "ML")

summary(LHC_mixed_final)
rsquared(LHC_mixed_final)
AIC(LHC_mixed_final)

#Checking model fit with residuals vs. fitted plot
#The line is approximately at zero and the points are evenly distributed around it; 
#no drastic asymmetry
plot(LHC_mixed_final)

#Examining pairwise relationships of years; forming groups
LHC_mixed_tukey <- summary(glht(LHC_mixed_final, linfct = mcp(Year = "Tukey")))
LHC_mixed_tukey

cld(LHC_mixed_tukey)
plot(LHC_mixed_tukey)

#Plotting analysis results
#Equations, corallivores vary
eq_2004_coral <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[11] * x + 
    fixef(LHC_mixed_final)[1] + 
    fixef(LHC_mixed_final)[10] * mean(Grazers_mean[Year == 2004]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2004])})
eq_2006_coral <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[11] * x + 
    fixef(LHC_mixed_final)[1] +
    fixef(LHC_mixed_final)[2] + 
    fixef(LHC_mixed_final)[10] * mean(Grazers_mean[Year == 2006]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2006])})
eq_2007_coral <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[11] * x + 
    fixef(LHC_mixed_final)[1] +
    fixef(LHC_mixed_final)[3] + 
    fixef(LHC_mixed_final)[10] * mean(Grazers_mean[Year == 2007]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2007])})
eq_2008_coral <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[11] * x + 
    fixef(LHC_mixed_final)[1] +
    fixef(LHC_mixed_final)[4] + 
    fixef(LHC_mixed_final)[10] * mean(Grazers_mean[Year == 2008]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2008])})
eq_2009_coral <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[11] * x + 
    fixef(LHC_mixed_final)[1] +
    fixef(LHC_mixed_final)[5] + 
    fixef(LHC_mixed_final)[10] * mean(Grazers_mean[Year == 2009]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2009])})
eq_2011_coral <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[11] * x + 
    fixef(LHC_mixed_final)[1] +
    fixef(LHC_mixed_final)[6] + 
    fixef(LHC_mixed_final)[10] * mean(Grazers_mean[Year == 2011]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2011])})
eq_2012_coral <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[11] * x + 
    fixef(LHC_mixed_final)[1] +
    fixef(LHC_mixed_final)[7] + 
    fixef(LHC_mixed_final)[10] * mean(Grazers_mean[Year == 2012]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2012])})
eq_2013_coral <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[11] * x + 
    fixef(LHC_mixed_final)[1] +
    fixef(LHC_mixed_final)[8] + 
    fixef(LHC_mixed_final)[10] * mean(Grazers_mean[Year == 2013]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2013])})
eq_2014_coral <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[11] * x + 
    fixef(LHC_mixed_final)[1] +
    fixef(LHC_mixed_final)[9] + 
    fixef(LHC_mixed_final)[10] * mean(Grazers_mean[Year == 2014]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2014])})

#Plot with corallivores on the x-axis
LHC_corallivores_plot <- ggplot(GBR_Sites_Processed_2004) + 
  aes(y = LHC_mean, color = MAC_mean, x = Corallivores_mean) +
  geom_point() +
  ylim(0, 100) +
  scale_color_viridis_c(option = "plasma") +
  labs(y = "Mean live hard coral % cover", 
       x = "Mean number of corallivore fish species",
       color = "Mean fleshy macroalgae % cover") +
  facet_wrap(vars(Year)) +
  stat_function(fun = eq_2004_coral, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2004)) + 
  stat_function(fun = eq_2006_coral, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2006)) +
  stat_function(fun = eq_2007_coral, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2007)) +
  stat_function(fun = eq_2008_coral, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2008)) +
  stat_function(fun = eq_2009_coral, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2009)) +
  stat_function(fun = eq_2011_coral, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2011)) +
  stat_function(fun = eq_2012_coral, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2012)) +
  stat_function(fun = eq_2013_coral, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2013)) +
  stat_function(fun = eq_2014_coral, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2014)) 

print(LHC_corallivores_plot)

#Equations, grazers vary
eq_2004_grazer <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[10] * x + 
    fixef(LHC_mixed_final)[1] + 
    fixef(LHC_mixed_final)[11] * mean(Corallivores_mean[Year == 2004]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2004])})
eq_2006_grazer <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[10] * x + 
    fixef(LHC_mixed_final)[1] +
    fixef(LHC_mixed_final)[2] + 
    fixef(LHC_mixed_final)[11] * mean(Corallivores_mean[Year == 2006]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2006])})
eq_2007_grazer <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[10] * x + 
    fixef(LHC_mixed_final)[1] +
    fixef(LHC_mixed_final)[3] + 
    fixef(LHC_mixed_final)[11] * mean(Corallivores_mean[Year == 2007]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2007])})
eq_2008_grazer <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[10] * x + 
    fixef(LHC_mixed_final)[1] +
    fixef(LHC_mixed_final)[4] + 
    fixef(LHC_mixed_final)[11] * mean(Corallivores_mean[Year == 2008]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2008])})
eq_2009_grazer <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[10] * x + 
    fixef(LHC_mixed_final)[1] +
    fixef(LHC_mixed_final)[5] + 
    fixef(LHC_mixed_final)[11] * mean(Corallivores_mean[Year == 2009]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2009])})
eq_2011_grazer <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[10] * x + 
    fixef(LHC_mixed_final)[1] +
    fixef(LHC_mixed_final)[6] + 
    fixef(LHC_mixed_final)[11] * mean(Corallivores_mean[Year == 2011]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2011])})
eq_2012_grazer <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[10] * x + 
    fixef(LHC_mixed_final)[1] +
    fixef(LHC_mixed_final)[7] + 
    fixef(LHC_mixed_final)[11] * mean(Corallivores_mean[Year == 2012]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2012])})
eq_2013_grazer <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[10] * x + 
    fixef(LHC_mixed_final)[1] +
    fixef(LHC_mixed_final)[8] + 
    fixef(LHC_mixed_final)[11] * mean(Corallivores_mean[Year == 2013]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2013])})
eq_2014_grazer <- with(GBR_Sites_Processed_2004, function(x){fixef(LHC_mixed_final)[10] * x + 
    fixef(LHC_mixed_final)[1] +
    fixef(LHC_mixed_final)[9] + 
    fixef(LHC_mixed_final)[11] * mean(Corallivores_mean[Year == 2014]) +
    fixef(LHC_mixed_final)[12] * mean(MAC_mean[Year == 2014])})

#Plot with grazers on the x axis
LHC_grazers_plot <- ggplot(GBR_Sites_Processed_2004) + 
  aes(y = LHC_mean, color = MAC_mean, x = Grazers_mean) +
  geom_point() +
  ylim(0, 100) +
  scale_color_viridis_c(option = "plasma") +
  labs(y = "Mean live hard coral % cover", 
       x = "Mean number of grazer fish species",
       color = "Mean fleshy macroalgae % cover") +
  facet_wrap(vars(Year)) +
  stat_function(fun = eq_2004_grazer, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2004)) + 
  stat_function(fun = eq_2006_grazer, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2006)) +
  stat_function(fun = eq_2007_grazer, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2007)) +
  stat_function(fun = eq_2008_grazer, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2008)) +
  stat_function(fun = eq_2009_grazer, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2009)) +
  stat_function(fun = eq_2011_grazer, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2011)) +
  stat_function(fun = eq_2012_grazer, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2012)) +
  stat_function(fun = eq_2013_grazer, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2013)) +
  stat_function(fun = eq_2014_grazer, 
                data = subset(GBR_Sites_Processed_2004, 
                              Year == 2014)) 

print(LHC_grazers_plot)
