

#install.packages("metaheuristicOpt")
library(metaheuristicOpt)


#FUNCION OBJETIVO
FO <- function(x){
  dim <- length(x)
  result <- sum(c(1:dim)*(x^4))+runif(1)
  return(result)
}






#Definici�n de par�metros
Vmax <- 2
ci <- 1.5
cg <- 1.5
w <- 0.7
numVar <- 30
rangeVar <- matrix(c(-1.28,1.28), nrow=2)
minimos<-c()

for (i in 1:30) {
  # C�lculo de la soluci�n �ptima usando PSO 
  resultPSO <- PSO(FO, optimType="MIN", numVar, numPopulation=40,
                   maxIter=1000, rangeVar, Vmax, ci, cg, w)
  
  # Calcular el valor �ptimo usando la funci�n y el vector de mejor soluci�n
  optimum.value <- FO(resultPSO)
  minimos[i] <- optimum.value
}

#PROMEDIO DE LOS 30 VALORES 
mean(minimos)
#D.E DE LOS 30 VALORES
sd(minimos)

