print("IMPORTING THE DATA")
library(ggplot2) 
data_airplane <- read.delim("/Users/zoemajeux/Applied biostatistical/air.txt", header=FALSE, sep="")
colnames(data_airplane) <- c("Airline", "Length", "Speed", "Time", "Pop", "Cost","Revenue", "LoadFactor", "Capacity", "TotalAssets","InvestmentsFunds","AdjustedAssets")

print("CALCULING THE CORRELATIONS")
pairs(data_airplane[,2:12],pch=19)
cor(data_airplane[, c('Cost', 'Length', 'Speed','Time','Revenue', 'LoadFactor','Capacity','TotalAssets','InvestmentsFunds')])
cor(data_airplane[, c('Cost', 'LoadFactor','Capacity','TotalAssets','InvestmentsFunds')])
table_coeff=lm(Cost ~ Time+LoadFactor+Capacity+InvestmentsFunds, data = data_airplane)
print(summary(table_coeff))

print("PLOTTING THE DIFFERENT RELATIONS")
plot_LoadFactor <- data.frame( x=data_airplane$LoadFactor, y=data_airplane$Cost )
plot_Time <- data.frame( x=log(data_airplane$Time), y=log(data_airplane$Cost) )
plot_Capacity <- data.frame( x=data_airplane$Capacity, y=data_airplane$Cost )

print("MEASUREMENT OF THE FIT")
log_model <- lm(Cost ~ Time, data = data_airplane)
linear_model6 <- lm(Cost ~ LoadFactor, 6, raw = TRUE), data = data_airplane)
linear_model4 <- lm(Cost ~ poly(Capacity, 4, raw = TRUE), data = data_airplane)

print("CREATION OF SCATTERPLOT")
layout(matrix(1:3,nrow=2))
ggplot(plot_Time, aes(x=x, y=y), , fill=group) +
  geom_point()+
  theme(text = element_text(size = 20)) +
  labs(title='log-log Plot', x='Time', y='Cost')+
  stat_smooth(method = "lm", formula = y ~ x, col = "red") +
  annotate(geom="label", x = 1.2, y = 6.5,  label = as.expression(paste("logarithm regression:\n", "y = (", round(log_model$coefficients[1]*1.e-3,2)," + ",
                                                                        round(log_model$coefficients[2]*1.e-3,2), "x)*10^3")), fill="white",size=5.5, hjust = 0, vjust = 1)
ggplot(plot_LoadFactor, aes(x=x, y=y)) +
  geom_point()+
  labs(title='Plot', x='LoadFactor', y='Cost')+
  theme(text = element_text(size = 20)) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 6, raw = TRUE), col = "red") +
  annotate(geom="label", x = 0.2, y =600,
           label = as.expression(paste("polynomial regression:\n", "y = (", round(linear_model6$coefficients[1]*1.e-6,2)," + ",
                                round(linear_model6$coefficients[2]*1.e-6,2),"x +",
                                round(linear_model6$coefficients[3]*1.e-6,2), "x^2 +",
                                round(linear_model6$coefficients[4]*1.e-6,2), "x^3 +",
                                round(linear_model6$coefficients[5]*1.e-6,2), "x^4 +",
                                round(linear_model6$coefficients[6]*1.e-6,2), "x^5 +",
                                round(linear_model6$coefficients[7]*1.e-6,2), "x^6)*10^6")), fill="white",size=5.5, hjust = 0, vjust = 1)
           
ggplot(plot_Capacity, aes(x=x, y=y)) +
  geom_point()+
  labs(title='Plot', x='Capacity', y='Cost')+
  theme(text = element_text(size = 20)) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 4, raw = TRUE), col = "red")+
  annotate(geom="label", x = 2.0, y =600,
           label = as.expression(paste("polynomial regression:\n", "y = (", round(linear_model4$coefficients[1]*1.e-2,2)," + ",
                                       round(linear_model4$coefficients[2]*1.e-2,2),"x +",
                                       round(linear_model4$coefficients[3]*1.e-2,2), "x^2 +",
                                       round(linear_model4$coefficients[4]*1.e-2,2), "x^3 +",
                                       round(linear_model4$coefficients[5]*1.e-2,2), "x^4)*10^2")), fill="white",size=5.5, hjust = 0, vjust = 1)
 

print("CALCULATION OF THE RELATION BETWEEN: COST AND CAPACITY, TOTALASSETS, INVESTMENTS")
aiplane_model <-lm(Cost ~ exp(Time):poly(LoadFactor, 6, raw = TRUE):poly(Capacity, 4, raw = TRUE), data = data_airplane)
coef(aiplane_model)
summary(aiplane_model)

aiplane_model <-lm(log(Cost) ~ log(Time)+log(poly(LoadFactor, 6, raw = TRUE))+log(poly(Capacity, 4, raw = TRUE)), data = data_airplane)
coef(aiplane_model)
summary(aiplane_model)

print("CALCULATION OF THE RELATION BETWEEN: COST AND CAPACITY, TOTALASSETS, INVESTMENTS with interactions")
aiplane_model <-lm(Cost ~Time+poly(LoadFactor, 6, raw = TRUE)+poly(Capacity, 4, raw = TRUE) +log(Time):LoadFactor, data = data_airplane)
coef(aiplane_model)
summary(aiplane_model) #useless!!! doesn't work


print("CALCULATION OF THE REGRESSION BETWEEN ALL COEFFICIENTS")
airmod <- lm(log(Cost) ~ log(Capacity)+log(Length)+LoadFactor+log(Time)+log(Pop)+log(Speed)+log(TotalAssets)+log(InvestmentsFunds)+log(AdjustedAssets)+log(Revenue), data=data_airplane)
coef(airmod)
summary(airmod)

print("RESIDUAL VERSIS FIT with all terms")
layout(matrix(1:4,ncol=2))
abline(airmod)
plot(airmod, which = 1)
print("RESIDUAL VERSIS FIT with only with cost, capcacity, totalassets and investments")
abline(aiplane_model)
plot(aiplane_model, which = 1)


print("QQ-PLOT with all terms")
#layout(matrix(1:4,ncol=2))
abline(airmod)
plot(airmod, which = 2)
print("QQ-PLOT with only with cost, capcacity, totalassets and investments")
abline(aiplane_model)
plot(aiplane_model, which = 2)


print("HISTOGRAM PLOT")

fg <- fitdist(data_airplane$Cost, "gamma") 
summary(fg)
fln <- fitdist(data_airplane$Cost, "lnorm") 
fn <- fitdist(data_airplane$Cost, "norm") 
fgaus <- fitdist(data_airplane$Cost, "gauss") 
#fp <- fitdist(data_airplane$Cost[data_airplane$Cost<150], "pois")  DOESNT WORK
hist(data_airplane$Cost, breaks=20,xlab="Cost",main="",xlim=c(0,800),col="blue")
denscomp(list(fg,fln,fn),main="",xlab="Cost")
qqcomp(list(fg,fln,fn),xlab="Cost")
print(data_airplane$Cost)








print("TEST PARTIE -> NOT USED")
## test 
#Display all the bivariate relations between the three variables
pairs( ~ Time + Capacity + LoadFactor, data=data_airplane)
layout(matrix(1:1,ncol=1)) 
interaction.plot(data_airplane$Time, data_airplane$Capacity, data_airplane$Cost)

# test

model_full2=lm(Cost ~ log(Time)+poly(LoadFactor, 6, raw = TRUE)+poly(Capacity, 4, raw = TRUE), data = data_airplane)
print(summary(model_full2))
#layout(matrix(1:1,nrow=2))
#model_inter<-aov(Cost~Time*LoadFactor,data=data_airplane)
layout(matrix(1:1,nrow=2))
plot(log(data_airplane$Time),data_airplane$Cost, col = "green", pch = 19)
plot(data_airplane$LoadFactor^6,data_airplane$Cost, pch = 19, col = "gray52")
# define sample data frames
sample_data <- data.frame(x=c(1, 2, 3, 4, 5),
                          y1 = c(7, 10, 26, 39, 5),
                          y2 = c(4, 14, 16, 29, 15),
                          y3 = c(2, 13, 36, 19, 25),
                          y4 = c(8, 11, 6, 9, 35))


# create base scatter plot
plot(sample_data$x, sample_data$y1)

# overlay scatter plot 
points(sample_data$x, sample_data$y2, col='green', pch=12)
points(sample_data$x, sample_data$y3, col='red', pch=13)
points(sample_data$x, sample_data$y4, col='blue')

#persp(data_airplane$LoadFactor,data_airplane$Time,data_airplane$Cost)
#install.packages("scatterplot3d")

#gf <- goodfit(data_airplane$Cost, "poisson")
#summary(gf)
#plot(gf,main="Count data vs Poisson distribution")
layout(matrix(1:1,ncol=1))
library(fitdistrplus)
hist(data_airplane$Cost, breaks=20,xlab="Cost",main="",xlim=c(0,800),col="white",)
gf <- plotdist(data_airplane$Cost[data_airplane$Cost < 300], histo = TRUE, demp = TRUE)
summary(gf)
# Superposer la courbe de densité de probabilité ajustée sur l'histogramme
hist(data_airplane$Cost, breaks=20,xlab="Cost",main="",xlim=c(0,800),col="darkmagenta",)
lines(gf, xlim = c(0, max(data_airplane$Cost)), col = "blue")

