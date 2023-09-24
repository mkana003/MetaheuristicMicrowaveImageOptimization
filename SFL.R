#install.packages("metaheuristicOpt")
library(metaheuristicOpt)

# define objective function
schewefels2.22 <- function(x){return(sum(abs(x)+prod(abs(x))))}



## Define parameter
numVar <- 30
rangeVar <- matrix(c(-10,10), nrow=2)
minimos<-c()

for (i in 1:30) {
  ## calculate the optimum solution shuffled frog leaping algorithm
  resultSFL <- SFL(schewefels2.22, optimType="MIN", numVar, numPopulation=40,
                   maxIter=1000, rangeVar)
  
  ## calculate the optimum value using objective function
  optimum.value <- schewefels2.22(resultSFL)
  minimos[i] <- optimum.value
}

#Mean value of the 30 observations 
mean(minimos)
#S.D value of the 30 observations
sd(minimos)