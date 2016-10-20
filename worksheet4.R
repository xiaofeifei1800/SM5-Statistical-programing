#1.a
library(MASS)
head(beav1)
ma3_data = ma3(beav1$temp)

#1.b
plot(beav1$temp, type = "l")

#1.c
n <- length(beav1$temp)
points(2:(n - 1), ma3(beav1$temp), type = "l", col = 2)

#1.d
plotMA = function(x,k)
{
  plot(x, type = "l", col = "black")
  n <- length(beav1$temp)
  points((k-1):(n - k +2), ma3(x), type = "l", col = 2)
  
}
plotMA(beav1$temp,3)

#2.a
plot(ChickWeight$Time,log(ChickWeight$weight))

#2.b
plot(ChickWeight$Time,log(ChickWeight$weight),col = ChickWeight$Chick, type = "l")

#2.c
range(ChickWeight$Time)
range(log(ChickWeight$weight))

#2.d??
ChickWeight$Chick1 = as.numeric(ChickWeight$Chick)
plot(ChickWeight$Chick1, type = "l", col = ChickWeight$Diet)
