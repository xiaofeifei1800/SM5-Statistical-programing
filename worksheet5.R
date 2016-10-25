#1.a
list = list()
for (i in 1:50)
{
  list[[i]] = rt(10,5)
}

#1.b
list = list(1:20)

#1.c
set.seed(2014)
X <- matrix(rexp(200), 20, 10)

#1.d
apply(CO2, 2, is.numeric)

#2.a
data = data.frame(
  count = c(53,11,414,37,0,4,16,139),
  victim = rep(c("white","black"),each = 4),
  defendant = c("white","black"),
  cata = c("yes","yes","no","no")
  )

#2.b
aggregate(count ~ cata + defendant, sum,data = data)

#2.d
# The proportion of the death penalty for black and white are same which is the
#wrong, if we look at the victim race

#2.e
aggregate(count ~ victim + cata, sum,data = data)
# from this way we can see if the victim is white, the defendants are more to be 
# put into deeath penlty 

#2.f
# because there is a third factor affect the results of death penlty, if we 
#ignore the third factor, we will give a wrong conclusion

#3.a
fun1 = function(n,k)
{
  mean_value = numeric()
  for (i in 1:k)
  {
    mean_value[i] = mean(runif(n))
  }
  return(mean_value)
}

fun2 = function(n,k)
{
  return(replicate(k, mean(runif(n))))
}

fun3 = function(n,k)
{
  value = matrix(data = runif(n*k), nrow = k,ncol = n, byrow = T )
  return(rowMeans(value))
}

#3.b
system.time(fun1(100,3000))
system.time(fun2(100,3000))
system.time(fun3(100,3000))
# matrix is the fast one

#3.c
hist(fun3(10,10000))
# when n goes biger, distribution like normal

#3.d
g = fun3(10,10000)
h<-hist(g, col="lightgray", xlab="Accuracy", main="Overall") 
xfit<-seq(min(g),max(g),length=40) 
yfit<-dnorm(xfit,mean=mean(g),sd=1/12) 
yfit <- yfit*diff(h$mids[1:2])*length(g)
lines(xfit, yfit, col="black", lwd=2)

#4.a
CI = function(x)
{
  if (length(x) == 0)
    stop("input is None")
  if (length(x) == 1)
    warning("x only has one value")
  CI = numeric()
  mean_val = mean(x)
  sd_val = sd(x)
  CI[1] = mean_val - sd_val*qt(0.025,length(x)-1)
  CI[2] = mean_val + sd_val*qt(0.025,length(x)-1)
  return(CI)
}

#4.b
stand_nomral = function(n)
{
  if (n == 0)
    stop("input is None")
  if (n == 1)
    warning("x only has one value")
  matrix_val = matrix(rnorm(n*20), nrow = 20, ncol = n, byrow = T)
  mean_val = rowMeans(matrix_val)
  sd_val = apply(matrix_val,1,sd)
  lower = mean_val - sd_val*qt(0.025,n-1)
  upper = mean_val + sd_val*qt(0.025,n-1)
  return(list(lower,upper))
}

 




