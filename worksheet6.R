#1.a
any(c(TRUE, FALSE, FALSE))
all(c(TRUE, FALSE, FALSE))

TF = function(x)
{
  n = 1:x
  size = length(n)
  idx = sample(size,size)
  
  return(any(idx == n))
}
  
#1.b
dist = logical()
for (i in 5:10)
{
  dist = append(dist,replicate(10,TF(i)))
}

table(dist)/length(dist)

#1.c
trunc01 = function(x)
{
  lower = rep(0,length(x))
  upper = rep(1, length(x))
  temp = pmax(x,lower)
  trunc = pmin(temp,upper)
  return(trunc)
}

#1.d
match(x %in% y)

#1.e
rmv = function(x,y)
{
  # input check
  common = match(y, x)
  y = y[is.na(common)]
  #output check
  return(y)
}

#2.a
library(MASS)
plot(faithful)
# it separat in two groups

#2.b
faithful$group_e = cut(faithful$eruptions, 2)
levels(faithful$group_e) = c("short_e", "long_e")
plot(faithful$eruptions,faithful$waiting, col = faithful$group_e)
faithful$group_w = cut(faithful$waiting, 2)
levels(faithful$group_w) = c("short_w", "long_w")


#2.c
table(faithful$group_e,faithful$group_w)

#3.a
bootsamp = function(x)
{
  new_sample = sample(x,length(x), replace = T)
  return(new_sample)
}

#3.b
x = rgamma(100,shape = 2, rate = 3)
boot = bootsamp(x)
length(unique(boot))
# 59

#3.c
x = rgamma(1000,shape = 2, rate = 3)
boot = bootsamp(x)
length(unique(boot))
#620

x = rgamma(10000,shape = 2, rate = 3)
boot = bootsamp(x)
length(unique(boot))
#6303

### 60%

#3.f
bootsd = function(x,B = 1000)
{
  stand = numeric()
  for(i in 1:B)
  {
    boot = bootsamp(x)
    append(stand, sd(boot))
  }
  return(stand)
}

#3.g
a = bootsd(Nile)
plot(a)




















