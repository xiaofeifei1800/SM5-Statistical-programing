#1.a
metric = function(x)
{
  return(list(mean(x), median(x), var(x)))
}

#1.b
possion = function(x,n)
{
  cum = 0
  for(i in n)
  {
    temp = (e^(-x)*x^i)/factorial(i)
    cum = cum+temp
  }
  
  return(cum)
}

#1.c
charcheck = function(x)
{
  for(i in length(x))
  {
    if (is.character(x[i]))
    {
      print(x[i])
    }
  }
}

#1.d
random_walk = function(k)
{
  vector = numeric()
  vector[1] = 0
  while(TRUE)
  {
    vector[i] = vector[i-1]+sample(c(1,-1), size = 1,prob = c(0.5,0.5))
    if (vector[i] == k or vector[i] == -k)
    {
      break
    }
  }
  return(vector)
}

#2.a
ma3 = function(x)
{
  z = numeric()
  for(i in 1:length(x))
  {
    z[i] = mean(x[i],x[i+1],x[i+2])
  }
  return(z)
}

#2.b
ma3 = function(x,k)
{
  z = numeric()

  for(i in 1:length(x))
  {
    z[i] = mean(x[i:(i+k)])
  }
  return(z)
}

#2.c
ma3 = function(x,k)
{
  z = numeric()
  if (x<k)
  {
  
    stop("x is less than k")
    
  } else {
    for(i in 1:length(x))
    {
      z[i] = mean(x[i:(i+k-1)])
    }
  }
  return(z)
}

#2.d
# z = x
??

#3.a
poissonprocess = function(lamda,M)
{
  i = 2
  t = numeric()
  t[1] = rexp(n = 1, rate = lamda)
  while(t[i-1]<M)
  {
    
    t[i] = rexp(n = 1, rate = lamda) + t[i-1]
    i = i+1
  }
  return(t)
}

#3.b
sequen = numeric()
for(i in 1:10000)
{
  sequen[i]=length(poissonprocess(5, 1))
}
hist(sequen)
mean(sequen)
var(sequen)

#4.a
tayExp = function(x,i) x^i/factanal(i)
Exp = function(x,n)
{
  sequen = numeric()
  for(i in 1:n)
  {
    sequen[i] = tayExp(x,i)   
  }
  return(sequen)
}

#4.b
series = function(x,i) (-1)^(i-1)*x^i/i
Exp = function(x,n)
{
  sequen = numeric()
  for(i in 1:n)
  {
    sequen[i] = series(x,i)   
  }
  return(sequen)
}

#4.c
?

#5.a
my_ellipsis_function = function(...) 
{
  myargs = list(...)
  adiag(myargs[[1]],myargs[[2]])
}

my_ellipsis_function(a <- matrix(1:6, 2, 3),b <- matrix(7:10, 2, 2))

#5.b
my_ellipsis_function = function(...) 
{
  myargs = list(...)
  newmatrix = matrix()
  for(i in 1:length(myargs))
  {
    newmatrix = adiag(newmatrix,myargs[[i]])
  }
  newmatrix = newmatrix[-1,-1]
  return(newmatrix)
}

my_ellipsis_function(a <- matrix(1:6, 2, 3),b <- matrix(7:10, 2, 2))
