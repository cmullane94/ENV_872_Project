getwd()

# Loading packages
library(tidyverse)
library(agricolae)
library(cowplot)

deftheme <- theme_classic(base_size = 14) + 
  theme(axis.text = element_text(color = "black"),
        legend.position = "top") 
theme_set(deftheme)

# Loading datasets
GBR_Sites_Processed_2004 <- read.csv("./Data/Processed/data.gov.au_fish_benthos_GBR_sites_2004_processed.csv")
GBR_Sites_Processed_1999 <- read.csv("./Data/Processed/data.gov.au_fish_benthos_GBR_sites_1999_processed.csv")

#Checking for normality within categories
#Null hypothesis: sample came from a normally distributed population
shapiro.test(GBR_Sites_Processed_1999$LHC_mean[GBR_Sites_Processed_1999$Zone == "Fished"])
shapiro.test(GBR_Sites_Processed_1999$LHC_mean[GBR_Sites_Processed_1999$Zone == "NTR 1987"])
shapiro.test(GBR_Sites_Processed_1999$LHC_mean[GBR_Sites_Processed_1999$Zone == "NTR 2004"])
#Fished and NTR 1987: Not from normally distributed populations
#NTR 2004: From a normally distributed population

#Checking for equal variance
#All population variances are not equal
bartlett.test(GBR_Sites_Processed_1999$LHC_mean ~ GBR_Sites_Processed_1999$Zone)

# ANOVA is robust against departures from equal variance - continue

#ANOVA
#No difference in mean LHC means among zones
LHC_anova <- aov(data = GBR_Sites_Processed_1999, LHC_mean ~ Zone) 
summary(LHC_anova)

LHC_anova_lm <- lm(data = GBR_Sites_Processed_1999, LHC_mean ~ Zone) 
summary(LHC_anova_lm)

#Checking model assumptions
#Long tails, but fairly normal distribution of residuals; homoscedastic
par(mfrow = c(2, 2)) 
plot(LHC_anova_lm)

#Extracting groupings for pairwise relationships
LHC_groups <- HSD.test( LHC_anova, "Zone", group = TRUE) 
LHC_groups

#Plotting
LHC_zone_plot <- ggplot(GBR_Sites_Processed_1999) +
  aes(x = Zone, y = LHC_mean) +
  geom_boxplot() +
  stat_summary(geom = "text", fun.y = max, vjust = -1, size = 3.5,
               label = c("a", "a", "a")) +
  ylim(0, 100) +
  labs(y = "Mean live hard coral % cover") +
  theme(axis.title.y = element_text(size = 10))

print(LHC_zone_plot)
###############################################

#Checking for normality within categories
#Null hypothesis: sample came from a normally distributed population
shapiro.test(GBR_Sites_Processed_2004$Total.Fish.Densit_mean[GBR_Sites_Processed_2004$Zone == "Fished"])
shapiro.test(GBR_Sites_Processed_2004$Total.Fish.Densit_mean[GBR_Sites_Processed_2004$Zone == "NTR 1987"])
shapiro.test(GBR_Sites_Processed_2004$Total.Fish.Densit_mean[GBR_Sites_Processed_2004$Zone == "NTR 2004"])

#Dependent variable was log-transformed normalize the distribution of its residuals after examining model plot
shapiro.test(log(GBR_Sites_Processed_2004$Total.Fish.Densit_mean)[GBR_Sites_Processed_2004$Zone == "Fished"])
shapiro.test(log(GBR_Sites_Processed_2004$Total.Fish.Densit_mean)[GBR_Sites_Processed_2004$Zone == "NTR 1987"])
shapiro.test(log(GBR_Sites_Processed_2004$Total.Fish.Densit_mean)[GBR_Sites_Processed_2004$Zone == "NTR 2004"])
#None of these variables come from normally distributed populations

#Checking for equal variance
#All populations are not equal
bartlett.test(GBR_Sites_Processed_2004$Total.Fish.Densit_mean ~ GBR_Sites_Processed_2004$Zone)

#Log-transformed: All populations are equal
bartlett.test(log(GBR_Sites_Processed_2004$Total.Fish.Densit_mean) ~ GBR_Sites_Processed_2004$Zone)

# ANOVA is robust against departures from equal variance - continue

#ANOVA
#No difference in mean fish density means among zones
Fish_Densit_anova <- aov(data = GBR_Sites_Processed_2004, Total.Fish.Densit_mean ~ Zone) 
summary(Fish_Densit_anova)

Fish_Densit_anova_lm <- lm(data = GBR_Sites_Processed_2004, Total.Fish.Densit_mean ~ Zone) 
summary(Fish_Densit_anova_lm)

#Log-transformed: Zones have different mean fish density means
Fish_Densit_anova_log <- aov(data = GBR_Sites_Processed_2004, log(Total.Fish.Densit_mean) ~ Zone) 
summary(Fish_Densit_anova_log)

Fish_Densit_anova_lm_log <- lm(data = GBR_Sites_Processed_2004, log(Total.Fish.Densit_mean) ~ Zone) 
summary(Fish_Densit_anova_lm_log)

#Checking model assumptions
#Residuals vs. fitted line departs from zero; distribution of residuals is skewed
par(mfrow = c(2, 2)) 
plot(Fish_Densit_anova_lm)

#Log-transformed: Checking model assumptions
#Distribution of residuals has long tails but is more normal; Residuals vs. fitted has the red line at zero
par(mfrow = c(2, 2)) 
plot(Fish_Densit_anova_lm_log)

#Extracting groupings for pairwise relationships
Fish_densit_groups <- HSD.test(Fish_Densit_anova_log, "Zone", group = TRUE) 
Fish_densit_groups

#Plotting
Fish_densit_zone_plot <- ggplot(GBR_Sites_Processed_2004) +
  aes(x = Zone, y = log(Total.Fish.Densit_mean)) +
  geom_boxplot() +
  stat_summary(geom = "text", fun.y = max, vjust = -1, size = 3.5,
               label = c("b", "b", "a")) +
  ylim(4.5, 11) +
  labs(y = expression('Ln mean total fish density (per 1000 m'^"2"*')')) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10))

print(Fish_densit_zone_plot)

###############################################

#Checking for normality within categories
#Null hypothesis: sample came from a normally distributed population
shapiro.test(GBR_Sites_Processed_2004$Fish.Species.richness_mean[GBR_Sites_Processed_2004$Zone == "Fished"])
shapiro.test(GBR_Sites_Processed_2004$Fish.Species.richness_mean[GBR_Sites_Processed_2004$Zone == "NTR 1987"])
shapiro.test(GBR_Sites_Processed_2004$Fish.Species.richness_mean[GBR_Sites_Processed_2004$Zone == "NTR 2004"])
#None of these variables come from normally distributed populations

#Checking for equal variance
#All population variances are not equal
bartlett.test(GBR_Sites_Processed_2004$Fish.Species.richness_mean ~ GBR_Sites_Processed_2004$Zone)

# ANOVA is robust against departures from equal variance - continue

#ANOVA
#No difference in fish species richness means among zones
Fish_species_anova <- aov(data = GBR_Sites_Processed_2004, Fish.Species.richness_mean ~ Zone) 
summary(Fish_species_anova)

Fish_species_anova_lm <- lm(data = GBR_Sites_Processed_2004, Fish.Species.richness_mean ~ Zone) 
summary(Fish_species_anova_lm)

#Checking model assumptions
#Long tails but normal; fairly homescedastic; red line slightly departs from 0
par(mfrow = c(2, 2)) 
plot(Fish_species_anova_lm)

#Extracting groupings for pairwise relationships
Fish_species_groups <- HSD.test(Fish_species_anova_lm, "Zone", group = TRUE) 
Fish_species_groups

#Plotting
Fish_species_rich_plot <- ggplot(GBR_Sites_Processed_2004) +
  aes(x = Zone, y = Fish.Species.richness_mean) +
  geom_boxplot() +
  stat_summary(geom = "text", fun.y = max, vjust = -1, size = 3.5,
               label = c("a", "a", "a")) +
  ylim(5, 51) +
  labs(y = "Mean number of fish species observed") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
              axis.title.y = element_text(size = 10))

print(Fish_species_rich_plot)

#Plotting all three graphs in one plot
plot_grid(Fish_species_rich_plot, Fish_densit_zone_plot, LHC_zone_plot, ncol = 1)


