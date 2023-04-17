library(ggplot2)
air<- read.delim("air.txt", header=FALSE, sep="")
colnames(air) <- c("Airline", "Length", "Speed", "FlightTime", "Populationserved", "Cost", "Revenue", "TonMileloadfactor", "Capacity", "TotalAssets","InvestmentsandSpecialFunds","AdjustedAssets")
names(air)
#airmod <- lm(log(Cost) ~ Capacity+Length+TonMileloadfactor+FlightTime+Populationserved+Speed+AdjustedAssets, data=air)

airmod <- lm(log(Cost) ~ log(Capacity)+log(Length)+TonMileloadfactor+log(FlightTime)+log(Populationserved)+log(Speed)+log(TotalAssets)+log(InvestmentsandSpecialFunds)+log(AdjustedAssets)+log(Revenue), data=air)
airwithcorr <- lm(log(Cost) ~ log(Capacity) + TonMileloadfactor + log(FlightTime), data=air)
coef(airwithcorr)
summary(airwithcorr)
#
#plot(Cost ~ Capacity+Length+TonMileloadfactor+FlightTime+Populationserved+Speed+AdjustedAssets, data=air)
layout(matrix(1:2,ncol=2))
abline(airwithcorr)
plot(airwithcorr, which = 1)
grid()
abline(hmod)
plot(airwithcorr, which = 2)
grid()
 





#model fitting, check if other fitting models
#plots : see report criteria. show same results as article