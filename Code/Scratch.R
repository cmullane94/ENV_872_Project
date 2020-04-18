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