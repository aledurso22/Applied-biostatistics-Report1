library(ggplot2)
air<- read.delim("air.txt", header=FALSE, sep="")
colnames(air) <- c("Airline", "Length", "Speed", "FlightTime", "Populationserved", "Cost", "Revenue", "TonMileloadfactor", "Capacity", "TotalAssets","InvestmentsandSpecialFunds","AdjustedAssets")
names(air)
#airmod <- lm(log(Cost) ~ Capacity+Length+TonMileloadfactor+FlightTime+Populationserved+Speed+AdjustedAssets, data=air)

airmod <- lm(log(Cost) ~ log(Capacity)+log(Length)+TonMileloadfactor+log(FlightTime)+log(Populationserved)+log(Speed)+log(TotalAssets)+log(InvestmentsandSpecialFunds)+log(AdjustedAssets)+log(Revenue), data=air)
coef(airmod)
summary(airmod)
layout(matrix(1:2,ncol=9))
plot(Cost ~ Capacity+Length+TonMileloadfactor+FlightTime+Populationserved+Speed+AdjustedAssets, data=air)
abline(airmod)
plot(airmod, which = 1) 





#model fitting, check if other fitting models
#plots : see report criteria. show same results as article