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
