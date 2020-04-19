#Plot using manually created equations

eq_2004_fish <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1]}
eq_2006_fish <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1] + fixef(Fish_mixed_final)[2]}
eq_2007_fish <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1] + fixef(Fish_mixed_final)[3]}
eq_2008_fish <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1] + fixef(Fish_mixed_final)[4]}
eq_2009_fish <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1] + fixef(Fish_mixed_final)[5]}
eq_2011_fish <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1] + fixef(Fish_mixed_final)[6]}
eq_2012_fish <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1] + fixef(Fish_mixed_final)[7]}
eq_2013_fish <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1] + fixef(Fish_mixed_final)[8]}
eq_2014_fish <- function(x){fixef(Fish_mixed_final)[10] * x + fixef(Fish_mixed_final)[1] + fixef(Fish_mixed_final)[9]}

Fish_densit_plot <- ggplot(GBR_Sites_Processed_2004) + 
  aes (y = log(Total.Fish.Densit_mean), x = LHC_mean, color = Year) +
  geom_point() +
  scale_color_viridis_d(direction = -1) +
  labs(x = "Mean live hard coral % cover", 
       y = expression('Ln mean total fish density (per 1000 m'^"2"*')'),
       color = "") +
  #geom_smooth(method = lm, se = FALSE),
  stat_function(fun = eq_2004_fish, color = "#FDE725FF", size = 1) + 
  stat_function(fun = eq_2006_fish, color = "#AADC32FF", size = 1) +
  stat_function(fun = eq_2007_fish, color = "#5DC863FF", size = 1) +
  stat_function(fun = eq_2008_fish, color = "#27AD81FF", size = 1) +
  stat_function(fun = eq_2009_fish, color = "#21908CFF", size = 1) +
  stat_function(fun = eq_2011_fish, color = "#2C728EFF", size = 1) +
  stat_function(fun = eq_2012_fish, color = "#3B528BFF", size = 1) +
  stat_function(fun = eq_2013_fish, color = "#472D7BFF", size = 1) +
  stat_function(fun = eq_2014_fish, color = "#440154FF", size = 1) 

print(Fish_densit_plot)

#Plot using predict

GBR_Sites_Processed_2004_corallivores_lme <- mutate(GBR_Sites_Processed_2004, 
                                                    Grazers_mean = mean(Grazers_mean),
                                                    MAC_mean = mean(MAC_mean),
                                                    Model = predict(LHC_mixed_final, 
                                                                    level = 0))

GBR_Sites_Processed_2004_corallivores_lme <- with(GBR_Sites_Processed_2004, 
                                                  data.frame(Year = Year,
                                                             Corallivores_mean = Corallivores_mean,
                                                             Grazers_mean = mean(Grazers_mean),
                                                             MAC_mean = mean(MAC_mean)))

GBR_Sites_Processed_2004_corallivores_lme <- mutate(GBR_Sites_Processed_2004_corallivores_lme, 
                                                    Model = predict(LHC_mixed_final, 
                                                                    GBR_Sites_Processed_2004_corallivores_lme, 
                                                                    type = "response", level = 0))

LHC_corallivores_plot_mutate <- ggplot(GBR_Sites_Processed_2004_corallivores_lme) + 
  aes(y = LHC_mean, color = MAC_mean, x = Corallivores_mean) +
  geom_point() +
  geom_line(aes(x = Corallivores_mean, y = Model)) +
  ylim(0, 100) +
  scale_color_viridis_c(option = "plasma") +
  labs(y = "Mean live hard coral % cover", 
       x = "Mean number of corallizore fish species",
       color = "Mean fleshy macroalgae % cover") +
  facet_wrap(vars(Year))

print(LHC_corallivores_plot_mutate)


##############

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


#####################################3


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

#############

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


######################################3


LHC_mixed_1 <- lme(data = GBR_Sites_Processed_2004, LHC_mean ~ Year * Zone * Grazers_mean * 
                     Corallivores_mean * MAC_mean, random = ~1|Site)

summary(LHC_mixed_1)
rsquared(LHC_mixed_1)
AIC(LHC_mixed_1)

LHC_mixed_2 <- update(LHC_mixed_1, .~.-Year:Zone:Grazers_mean:Corallivores_mean:MAC_mean)

summary(LHC_mixed_2)
rsquared(LHC_mixed_2)
AIC(LHC_mixed_2)

LHC_mixed_3 <- update(LHC_mixed_2, .~.-Zone:Grazers_mean:Corallivores_mean:MAC_mean)

summary(LHC_mixed_3)
rsquared(LHC_mixed_3)
AIC(LHC_mixed_3)

LHC_mixed_3 <- update(LHC_mixed_2, .~.-Grazers_mean:Corallivores_mean:MAC_mean)

summary(LHC_mixed_3)
rsquared(LHC_mixed_3)
AIC(LHC_mixed_3)

LHC_mixed_4 <- update(LHC_mixed_3, .~.-Zone:Grazers_mean:Corallivores_mean)

summary(LHC_mixed_4)
rsquared(LHC_mixed_4)
AIC(LHC_mixed_4)

LHC_mixed_5 <- update(LHC_mixed_4, .~.-Zone:Corallivores_mean:MAC_mean)

summary(LHC_mixed_5)
rsquared(LHC_mixed_5)
AIC(LHC_mixed_5)

LHC_mixed_6 <- update(LHC_mixed_5, .~.-Zone:Grazers_mean:MAC_mean)

summary(LHC_mixed_6)
rsquared(LHC_mixed_6)
AIC(LHC_mixed_6)

LHC_mixed_7 <- update(LHC_mixed_6, .~.-Zone:Corallivores_mean)

summary(LHC_mixed_7)
rsquared(LHC_mixed_7)
AIC(LHC_mixed_7)

LHC_mixed_8 <- update(LHC_mixed_7, .~.-Grazers_mean:Corallivores_mean)

summary(LHC_mixed_8)
rsquared(LHC_mixed_8)
AIC(LHC_mixed_8)

LHC_mixed_9 <- update(LHC_mixed_8, .~.-Corallivores_mean:MAC_mean)

summary(LHC_mixed_9)
rsquared(LHC_mixed_9)
AIC(LHC_mixed_9)

LHC_mixed_10 <- update(LHC_mixed_9, .~.-Grazers_mean:MAC_mean)

summary(LHC_mixed_10)
rsquared(LHC_mixed_10)
AIC(LHC_mixed_10)

LHC_mixed_11 <- update(LHC_mixed_10, .~.-Zone:MAC_mean)

summary(LHC_mixed_11)
rsquared(LHC_mixed_11)
AIC(LHC_mixed_11)

LHC_mixed_12 <- update(LHC_mixed_11, .~.-Zone:Grazers_mean)

summary(LHC_mixed_12)
rsquared(LHC_mixed_12)
AIC(LHC_mixed_12)

LHC_mixed_13 <- update(LHC_mixed_12, .~.-Zone)

summary(LHC_mixed_13)
rsquared(LHC_mixed_13)
AIC(LHC_mixed_13)

LHC_mixed_14 <- update(LHC_mixed_13, .~.-Grazers_mean)

summary(LHC_mixed_14)
rsquared(LHC_mixed_14)
AIC(LHC_mixed_14)

lrtest(LHC_mixed_1, LHC_mixed_2, LHC_mixed_3, LHC_mixed_4, LHC_mixed_5, LHC_mixed_6, 
       LHC_mixed_7, LHC_mixed_8, LHC_mixed_9, LHC_mixed_10, LHC_mixed_11, LHC_mixed_12, LHC_mixed_13, LHC_mixed_14)