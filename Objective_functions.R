
#OBJECTIVE FUNCTIONS

#Sphere (f1):
  sphere <- function(x){ return(sum(x^2)) }
  
#Schwefel 2.22 (f2):
    schewefels2.22 <- function(x){return(sum(abs(x)+prod(abs(x))))}
  
#Rotated (f3):
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
  
#Schwefel 2.21 (f4):
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
  
#Rosenbrock (f5):
    
    rosen <- function(xx)
    {
      d <- length(xx)
      xi <- xx[1:(d-1)]
      xnext <- xx[2:d]
      
      sum <- sum(100*(xnext-xi^2)^2 + (xi-1)^2)
      
      y <- sum
      return(y)
    }
  
  
#Step2 (f6):
    
    step2 <- function(x){ 
      acum<-0
      for(i in x){
        acum <- acum + (i+0.5)^2 
      }
      return(acum)
    }
  
#Quartic with noise (f7): 
    
    quartic <- function(x){
      dim <- length(x)
      result <- sum(c(1:dim)*(x^4))+runif(1)
      return(result)
    }
  
#Schwefel (f8):
    
    schwef <- function(xx)
    {
      d <- length(xx)
      
      sum <- sum(xx*sin(sqrt(abs(xx))))
      
      y <- 418.9829*d - sum
      return(y)
    }
  
#Rastrigin (f9):
    
    rastrigin <- function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))
  
#Ackley 1 (f10):
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
  
#Griewank (f11):
    griewank <- function(xx)
      
      
    {
      ii <- c(1:length(xx))
      sum <- sum(xx^2/4000)
      prod <- prod(cos(xx/sqrt(ii)))
      
      y <- sum - prod + 1
      return(y)
    }
  