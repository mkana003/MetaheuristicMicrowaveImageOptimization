

#install.packages("metaheuristicOpt")
library(metaheuristicOpt)


#Fitness Function
FO <- function(xx)
{
  ii <- c(1:length(xx))
  sum <- sum(xx^2/4000)
  prod <- prod(cos(xx/sqrt(ii)))
  
  y <- sum - prod + 1
  return(y)
}



#Parameter definition
Vmax <- 2
ci <- 1.5
cg <- 1.5
w <- 0.7
#Problem's lenght
numVar <- 30
rangeVar <- matrix(c(-600,600), nrow=2)
minimos<-c()

for (i in 1:30) {
  # Optimum vector searched by PSO function 
  resultPSO <- PSO(FO, optimType="MIN", numVar, numPopulation=40,
                   maxIter=1000, rangeVar, Vmax, ci, cg, w)
  
  # Optimum value obtanined evaluating the best particle in the fitness function
  optimum.value <- FO(resultPSO)
  minimos[i] <- optimum.value
}

#Mean value of the 30 observations
mean(minimos)
#S.D value of the 30 observations
sd(minimos)

print(minimos)
