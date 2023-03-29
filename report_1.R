#http://users.stat.ufl.edu/~winner/data/airline_costs.txt #the text explaining the data
#http://users.stat.ufl.edu/~winner/data/airline_costs.dat #the data we are going to analyze
#########################################################################################################
install.packages("ggplot2")
rm(list=ls()) #to remove the previous environment/variables
library(ggplot2)


air <- read.delim("air.txt", header=FALSE, sep="")
colnames(air) <- c("Airline", "Length", "Speed", "Time", "Population", 
                   "Cost","Revenue", "Factor", "Capacity", "TotalAssets","Funds","AdjustedAssets")
print(air)

#random plot to toy --> you can try see the logarithm behaviour:
#Try to see which of these plots are the same and understand how data/axis are changing ;)
plot(air$`Factor`, log(air$`Cost`))
plot(exp(air$`Factor`), air$`Cost`)
plot(air$`Factor`, air$`Cost`,log = 'y') #putting log = y or x you change the meas of the axis
plot(log(air$`Length`), air$`Cost`) #here instead you change the numbers themselves of y or x
plot(air$`Length`, air$`Cost`)
plot(air$`Length`, air$`Cost`,log = 'x')

#let's do the linear regression to estimate the coefficients:
airmod <- lm(log(Cost) ~ log(Capacity)+log(Length)+log(Revenue)+Factor+
               log(Time)+log(Population)+log(Speed)+log(Funds)+log(AdjustedAssets), data=air)
#we can see all the results from linear regression
coef(airmod)
summary(airmod)

#let's try to plot the correlation table:
layout(matrix(1:8,nrow=2))
pairs(air[,2:12],pch=19)
#let's now look at specific correlations between variables:
cor(air[, c('Cost', 'Length', 'Speed','Time','Revenue')])
cor(air[, c('Cost', 'Factor','Capacity','TotalAssets','Funds')])

# convert dataframe data into Log data
plot_data <- data.frame( x=log(air$Time), y=log(air$Cost) )
# create scatterplot using geom_point function
ggplot(plot_data, aes(x=x, y=y)) +geom_point()+labs(title='Log-Log Plot', x='Time',y='Cost')



