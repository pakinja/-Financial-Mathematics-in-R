# R code
#Function to create an amortization fund table

options(digits = 7) #We establish the precision with which we want to work
options(scipen = 999) #we say we do not want to work in scientific notation

I <- numeric();CA <<- numeric();SF <<- numeric() #We declare vectors to store the values of the different variables

#We give values for the amount, number of periods and interest rate
M <- 2400000
n <- 50 
i <- 3.7/100

# We create function
f.amort <- function(M,i,n) { 
  
  R <<- M*i/(((1 + i)**n)-1)
  I[1] <<- 0
  I[2] <<- R*i
  CA[1] <<- R
  SF[1] <<- R
  
  for (k in 1:(n-1)) {  
  
  CA[k+1] <<- R + I[k+1]
  SF[k+1] <<- SF[k] + CA[k+1]
  
    if (k < n-1){
    I[k+2] <<- SF[k+1]*i  
    }
  }
}

f.amort(M, i, n)

tabla <- cbind(I,CA,SF)
rtotal <- R*n
totales <- c(rtotal,sum(I),sum(CA),000)
renta <- c(R, recursive=TRUE)

tabla <- cbind(renta, tabla)
tabla <- rbind(tabla, totales)
colnames(tabla)<-c("Renta","Interes","Cantidad Acumulada", "Saldo Final")
