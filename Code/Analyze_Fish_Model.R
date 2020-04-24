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
Fish_mixed_1 <- lme(data = GBR_Sites_Processed_2004, 
                    log(Total.Fish.Densit_mean) ~ 
                      Year + Zone + LHC_mean + LCC_mean + 
                      SCI_mean + MAC_mean,
                    random = ~1|Site, method = "ML")

summary(Fish_mixed_1)
rsquared(Fish_mixed_1)
AIC(Fish_mixed_1)

stepAIC(Fish_mixed_1)
#Final Model: Total.Fish.Densit_mean ~ Year + LHC_mean + MAC_mean, random = ~1|Site

Fish_mixed_final <- lme(data = GBR_Sites_Processed_2004, 
                        log(Total.Fish.Densit_mean) ~ Year + LHC_mean, 
                        random = ~1|Site, method = "ML")

summary(Fish_mixed_final)
rsquared(Fish_mixed_final)
AIC(Fish_mixed_final)

#Interpreting the LHC_mean coefficient in the context of the natural log transformed dependent variable
(exp(fixef(Fish_mixed_final)[10]) - 1) * 100

#Checking model fit with residuals vs. fitted plot
#The line is approximately at zero and the points are evenly distributed around it; 
#no drastic asymmetry
plot(Fish_mixed_final)

#Examining pairwise relationships of years; forming groups
fish_mixed_tukey <- summary(glht(Fish_mixed_final, linfct = mcp(Year = "Tukey")))
fish_mixed_tukey

cld(fish_mixed_tukey)
plot(fish_mixed_tukey)

#Plot using predict
##Mutating dataset to add a column of predicted values
GBR_Sites_Processed_2004_fish_lme <- mutate(GBR_Sites_Processed_2004, Model = predict(Fish_mixed_final, level = 0))
write.csv(GBR_Sites_Processed_2004_fish_lme, row.names = FALSE, 
          file = "./Data/Processed/data.gov.au_fish_lme_2004_processed.csv")

Fish_densit_plot <- ggplot(GBR_Sites_Processed_2004_fish_lme) + 
  aes(y = log(Total.Fish.Densit_mean), x = LHC_mean, color = Year) +
  geom_point() +
  geom_line(aes(x = LHC_mean, y = Model)) +
  labs(x = "Mean live hard coral % cover", 
       y = expression('Ln mean total fish density (per 1000 m'^"2"*')'),
       color = "") +
  scale_color_viridis_d(direction = -1, 
                        labels = c("2004 (a)", "2006 (ce)", "2007 (e)",
                                   "2008 (de)", "2009 (ce)", "2011 (acd)",
                                   "2012 (ac)", "2013 (b)", "2014 (ac)"))

print(Fish_densit_plot)



  