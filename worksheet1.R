#1.1
seq(1,21,by = 2)

#1.2
rev(seq(11,50,by = 3))

#1.3
exp = seq(0:log2(1024))-1
vector = sapply(exp, function(x) 2^x)

#1.4
matrix(1:16,nrow = 4, ncol = 4, byrow = T)

#2.1
rep(1:4, each = 5)

#2.2
sample(1:4, size = 20, replace = T)

#2.3
condition = factor(c("A","B","C","D"))
sample(condition, size = 20, replace = T)

#3.1
A = matrix(rnorm(10*11), nrow = 10, ncol = 11)

#3.2
apply(A, 1, max)

#3.3
apply(A, 2, sd)

#3.4
b = A[,ncol(A)]
A = A[,-ncol(A)]

#3.5
lm(b ~ -1+A)

#3.6
apply(A, 1, sum)

#3.7
B = A[,1]
for (i in 2:10)
{
  temp = apply(A[,1:i], 1, sum)
  B = cbind(B,temp)
}

#4.1
random_walk = function(times)
{
  vector = numeric()
  vector[1] = 0
  for (i in 2:times)
  {
    vector[i] = vector[i-1]+sample(c(1,-1), size = 1,prob = c(0.5,0.5)) 
  }
  return(vector)
}
random_walk(25)
#4.2
vector = numeric()
vector[1] = 0
for (i in 2:25)
{
  vector[i] = cumsum(vector)+sample(c(1,-1), size = 1) 
}

#4.3
plot(random_walk(1000), type = "l")

#4.4
rbinom(1,25,0.5)-25

#4.5
d = 0
for(i in 1:100000)
{
  if (rbinom(1,25,0.5)>10){
    d = d+1
  }
}
pro = d/100000

#4.6
1-pbinom(10,25,0.5)

#5.1
D = diag(1/1:10,10,10)

#5.2
U = diag(5,10,10)-1

#5.3
length(U[,1])

#5.4
U = apply(U, 2, function(x) x/sqrt(sum(x^2)) )
apply(U, 2, sum)
apply(U, 2, var)*10

#5.5
X = U%*%D%*%t(U)

#5.6
eigen(X)
