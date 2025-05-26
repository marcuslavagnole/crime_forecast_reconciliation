## Load libraries
library(hts)

## Set working directory to file location and load utils
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("data_treated/matriz_y.RData")
load("data_treated/hierarchy.RData")

# Set the number of steps ahead to forecast
h      = c(1:3)
# Set the sample size for model training
tam_treino = 9*12
# Auxiliary objects
recon_ols  = NULL
recon_atha = NULL
recon_wls  = NULL
recon_mint = NULL
recon_ols_nonneg  = NULL
recon_atha_nonneg = NULL
recon_wls_nonneg  = NULL
recon_mint_nonneg = NULL

## Run reconciliation
for(j in h){
  
  # Load predicted values
  y_hat  = read.csv(paste0('base_forecast/base_forecast_h',j,'.csv'),header=FALSE,sep=';')
  # Set the sample size for model testing
  tam_teste  = dim(vetor_y)[2] - tam_treino - j + 1
  # Format predicted values 
  y_hat  = y_hat[,1:(ncol(y_hat)-j+1)]

  for(i in 1:tam_teste){
    m_res    = read.csv(paste('residuals/residuo',(tam_treino+i-1),'.csv',sep=''),header=FALSE,sep=';')
    grupos   = gts(y = t(vetor_y[87:271,1:(tam_treino+i-1)]),groups = m_groups)
    matriz_s = smatrix(grupos)
  
    # WLS_s covariance matrix
    aux_atha   = c(matriz_s%*%matrix(1,185,1))
    aux_recon_atha = combinef(fcasts = matrix(y_hat[,i],1,byrow=TRUE), groups = grupos$groups, 
                              weights = aux_atha, nonnegative = FALSE, keep = 'all', algorithms = "lu")
  
    # WLS_upsilon covariance matrix
    aux_wls    = diag(as.matrix(m_res)%*%t(as.matrix(m_res)))/(tam_treino+i-1)
    aux_recon_wls  = combinef(fcasts = matrix(y_hat[,i],1,byrow=TRUE), groups = grupos$groups, 
                              weights = aux_wls, nonnegative = FALSE, keep = 'all', algorithms = "lu")
  
    # MinT covariance matrix
    aux_recon_mint = MinT(fcasts = matrix(y_hat[,i],1,byrow=TRUE), groups = grupos$groups, 
                          residual = t(m_res), covariance = "shr", nonnegative = FALSE, keep = 'all', 
                          algorithms = "lu")
  
    # OLS covariance matrix
    aux_recon_ols = combinef(fcasts = matrix(y_hat[,i],1,byrow=TRUE), groups = grupos$groups, 
                             weights = rep(1,271), nonnegative = FALSE, keep = 'all', algorithms = "lu")
    
    # WLS_s covariance matrix - Non-negative
    aux_recon_atha_nonneg = combinef(fcasts = matrix(y_hat[,i],1,byrow=TRUE), groups = grupos$groups, 
                                     weights = aux_atha, nonnegative = TRUE, keep = 'all', algorithms = "lu")
    
    # WLS_upsilon covariance matrix - Non-negative
    aux_recon_wls_nonneg  = combinef(fcasts = matrix(y_hat[,i],1,byrow=TRUE), groups = grupos$groups, 
                                     weights = aux_wls, nonnegative = TRUE, keep = 'all', algorithms = "lu")
    
    # MinT covariance matrix - Non-negative
    aux_recon_mint_nonneg = MinT(fcasts = matrix(y_hat[,i],1,byrow=TRUE), groups = grupos$groups, 
                                 residual = t(m_res), covariance = "shr", nonnegative = TRUE, keep = 'all', 
                                 algorithms = "lu")
    
    # OLS covariance matrix - Non-negative
    aux_recon_ols_nonneg = combinef(fcasts = matrix(y_hat[,i],1,byrow=TRUE), groups = grupos$groups, 
                                    weights = rep(1,271), nonnegative = TRUE, keep = 'all', algorithms = "lu")
  
    recon_atha = cbind(recon_atha,t(aux_recon_atha))
    recon_wls  = cbind(recon_wls,t(aux_recon_wls))
    recon_mint = cbind(recon_mint,t(aux_recon_mint))
    recon_ols  = cbind(recon_ols,t(aux_recon_ols))
    recon_atha_nonneg = cbind(recon_atha_nonneg,t(aux_recon_atha_nonneg))
    recon_wls_nonneg  = cbind(recon_wls_nonneg,t(aux_recon_wls_nonneg))
    recon_mint_nonneg = cbind(recon_mint_nonneg,t(aux_recon_mint_nonneg))
    recon_ols_nonneg  = cbind(recon_ols_nonneg,t(aux_recon_ols_nonneg))
  
  print(i)
}

}

save(recon_atha,file='reconciled_forecast/recon_atha.RData')
save(recon_wls,file='reconciled_forecast/recon_wls.RData')
save(recon_mint,file='reconciled_forecast/recon_mint.RData')
save(recon_ols,file='reconciled_forecast/recon_ols.RData')
save(recon_atha_nonneg,file='reconciled_forecast/recon_atha_nonneg.RData')
save(recon_wls_nonneg,file='reconciled_forecast/recon_wls_nonneg.RData')
save(recon_mint_nonneg,file='reconciled_forecast/recon_mint_nonneg.RData')
save(recon_ols_nonneg,file='reconciled_forecast/recon_ols_nonneg.RData')
