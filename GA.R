##################################
## Optimizing the sphere function
# define sphere function as objective function
sphere <- function(x){
  return(sum(x^2))
}
#Schwefel 2.22:
  schewefels2.22 <- function(x){return(sum(abs(x)+prod(abs(x))))}
  
#  Rotated:
  rothyp <- function(x)
  { 
    d <- length(x)
    
    xmat <- matrix(rep(x,times=d), d, d, byrow=TRUE)
    xmatlow <- xmat
    xmatlow[upper.tri(xmatlow)] <- 0	
    
    inner <- rowSums(xmatlow^2)
    outer <- sum(inner)
    
    y <- outer
    return(y)
  }
 #step2 
  step2 <- function(x){ 
    acum<-0
    for(i in x){
      acum <- acum + (i+0.5)^2 
    }
    return(acum)
  }
  
 #   Schwefel 2.21:
      schewefels2.21 <- function(x)
      {
        maximum <- 0
        for(i in x){
          if(abs(i)>maximum) {
            maximum <- abs(i)
          }
        }
        return(maximum)
      }
#      Rosenbrock
      
      rosen <- function(xx)
      {
        d <- length(xx)
        xi <- xx[1:(d-1)]
        xnext <- xx[2:d]
        
        sum <- sum(100*(xnext-xi^2)^2 + (xi-1)^2)
        
        y <- sum
        return(y)
      }
      
# define Quartic with noise function as objective function
quartic <- function(x){
  dim <- length(x)
  result <- sum(c(1:dim)*(x^4))+runif(1)
  return(result)
}
#Schwefel

schwef <- function(xx)
{
  d <- length(xx)
  
  sum <- sum(xx*sin(sqrt(abs(xx))))
  
  y <- 418.9829*d - sum
  return(y)
}
#Rastrigin

rastrigin <- function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))

#Ackley 1 
ackley1 <- function(xx, a=20, b=0.2, c=2*pi)
{
  d <- length(xx)
  
  sum1 <- sum(xx^2)
  sum2 <- sum(cos(c*xx))
  
  term1 <- -a * exp(-b*sqrt(sum1/d))
  term2 <- -exp(sum2/d)
  
  y <- term1 + term2 + a + exp(1)
  return(y)
}

#Griewank 
griewank <- function(xx)
{
  ii <- c(1:length(xx))
  sum <- sum(xx^2/4000)
  prod <- prod(cos(xx/sqrt(ii)))
  
  y <- sum - prod + 1
  return(y)
}

Generalized1 <- function(x){
  y=1 + 1/4*(x +1)
  a <-10
  k <- 100
  m<- 4
  u = x*a*k*m
  result <- 3.1416/30*(10*sin(3.1416*y)+sum(y-1)^2*(1+10*sin(x*y+1))+(y-1)^2)+sum(u(x,a,k,m))
  return(result)
}
## Define parameter
Pm <- 0.1
Pc <- 0.8
numVar <- 30
rangeVar <- matrix(c(-50, 50), nrow=2)
acum<-0
minimos<-c()
for(i in 1:30){
  resultGA <- GA(  Generalized1, optimType="MIN", numVar, numPopulation=40,
                 maxIter=1000, rangeVar, Pm, Pc)
  ## calculate the optimum value using sphere function
  optimum.value <- Generalized1(resultGA)
  minimos[i]<-optimum.value
  
}
for(i in 1:30){
print(minimos[i])
}
## calculate the optimum solution using Genetic Algorithm
mean(minimos)
sd(minimos)