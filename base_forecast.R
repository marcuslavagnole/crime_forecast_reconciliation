## Load libraries
library(KFAS)
library(parallel)
library(doParallel)

## Set working directory to file location and load utils
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("data_treated/matriz_y.RData")

## Define parallel processing configurations
n.cores <- parallel::detectCores() - 1
# Create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
# Check cluster definition (optional)
print(my.cluster)
# Register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
# Check if it is registered (optional)
foreach::getDoParRegistered()
# How many workers are available? (optional)
foreach::getDoParWorkers()

## Set non-Gaussian state space model configurations
Modelo  <- function(serie,n_prev){
  Zt <- matrix(c(1, 0), 1, 2)
  Ht <- matrix(NA)
  Tt <- matrix(c(1, 0, 1, 1), 2, 2)
  Rt <- matrix(c(1, 0), 2, 1)
  Qt <- matrix(NA)
  a1 <- matrix(c(1, 0), 2, 1)
  P1 <- matrix(0, 2, 2)
  P1inf <- diag(2)
  
  model_poisson <- SSModel(serie ~ -1 +
                           SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, P1inf = P1inf),
                           distribution = "poisson")
  
  fit_poisson <- fitSSM(model_poisson, inits = c(0, 0), method = "BFGS")
  out_poisson <- KFS(fit_poisson$model)
  
  residuo  = residuals(out_poisson, type = c("response"))
  previsao = predict(fit_poisson$model,n.ahead=n_prev,interval="prediction",level=0.9,nsim=100)
  return(c(previsao[,1],residuo))
}

## Forecast settings
# Set the sample size for model training
tam_treino = 9*12
# Set the sample size for model testing
tam_teste  = dim(vetor_y)[2] - tam_treino 
# Set the number of steps ahead to forecast
h_max      = 3
# Set the number of observations
n          = dim(vetor_y)[1]

## Forecast and calculate residuals for all time series
m_prev1 = NULL
m_prev2 = NULL
m_prev3 = NULL
for(i in 1:tam_teste){
  prev = foreach(j = 1:n, .combine = 'c', .packages='KFAS') %dopar% {
    Modelo(as.vector(vetor_y[j,1:(tam_treino+i-1)]),h_max)
  }
  print(i)
  prev_aux= matrix(prev,n,(3+(tam_treino+i-1)),byrow=TRUE)
  m_prev1 = cbind(m_prev1,prev_aux[,1])
  m_prev2 = cbind(m_prev2,prev_aux[,2])
  m_prev3 = cbind(m_prev3,prev_aux[,3])
  residuo_aux = prev_aux[,4:((tam_treino+i-1)+3)]
  
  # Save residuals
  write.table(residuo_aux, file=paste('residuals/residuo',tam_treino+i-1,'.csv',sep=''),sep=';',row.names=FALSE,col.names=FALSE)
  gc()
} 

# Save forecasts
write.table(m_prev1,file='base_forecast/base_forecast_h1.csv',sep=';',row.names=FALSE,col.names=FALSE)
write.table(m_prev2,file='base_forecast/base_forecast_h2.csv',sep=';',row.names=FALSE,col.names=FALSE)
write.table(m_prev3,file='base_forecast/base_forecast_h3.csv',sep=';',row.names=FALSE,col.names=FALSE)
