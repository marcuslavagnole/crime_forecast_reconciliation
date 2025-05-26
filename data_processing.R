### Libraries
library(dplyr)

### Reading data set
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dados = read.csv2("data_base/base_crimes.csv")
dados = dados[which(dados$mcirc=='3304557'),]

### Formatting data set
# Formatting date
dados$data = format(ISOdate(year = dados$ano, month = dados$mes, day = 01),"%m/%Y")

# Selecting years of interest
dados = dados[which(dados$ano<2020),]

# Selecting CISP with complete observations
CISP  = sort(unique(dados$CISP))

series_CISP = NULL
for(i in CISP){
  series_CISP = c(series_CISP,length(dados$CISP[which(dados$CISP==i)]))
}
CISP_selec = CISP[which(series_CISP == 204)]
CISP_selec = CISP_selec[-which(CISP_selec==26)]

dados_CISP26 = dados[which(dados$CISP == 26),]
dados = dados[which(dados$CISP %in% CISP_selec),]

### Formating hierarchies
CISP  = sort(unique(dados$CISP))

# AISP
dados$AISP_atual = NA
for(i in CISP){
  if (i == 37){
    dados$AISP_atual[which(dados$CISP==i)] = 22
  } else if (i == 36){
    dados$AISP_atual[which(dados$CISP==i)] = 40
  } else if (i == 16){
    dados$AISP_atual[which(dados$CISP==i)] = 23
  } else{
    dados$AISP_atual[which(dados$CISP==i)] = dados$AISP[which(dados$CISP==i & dados$mes_ano=='2019m12')]
  }
}
#AISP   = sort(unique(dados$AISP_atual))
AISP   = unique(dados$AISP_atual)

#RISP
dados$RISP_atual = NA
for(i in CISP){
  if (i == 16){
    dados$RISP_atual[which(dados$CISP==i)] = 1
  } else{
    dados$RISP_atual[which(dados$CISP==i)] = dados$RISP[which(dados$CISP==i & dados$mes_ano=='2019m12')]
  }
}
#RISP   = sort(unique(dados$RISP_atual))
RISP   = unique(dados$RISP_atual)

### Building Matrix b
b = NULL
for(i in CISP){
  if(i == 24){
    b = rbind(b,t(dados[which(dados$CISP == i),c(13,16,18,28,44)]+dados_CISP26[,c(13,16,18,28,44)]))
  } else{
    b = rbind(b,t(dados[which(dados$CISP == i),c(13,16,18,28,44)]))
  }
}
b = as.matrix(b)

### Building Matrix S
num_hierarquia = 5
tam_hierarquia = c(1,5,length(RISP),length(AISP),length(CISP))

h5 = diag(1,dim(b)[1])
dim(h5)

h4 = matrix(0,tam_hierarquia[2]*tam_hierarquia[4],dim(b)[1])
contador = 1
for(j in AISP){
  aux1   = unique(dados$CISP[which(dados$AISP_atual==j)])
  aux2   = pmatch(aux1,CISP)
  #print(list(j,aux1,aux2))
  #print(length(aux1))
  for(i in 1:tam_hierarquia[2]){
    h4[contador,(tam_hierarquia[2]*(aux2-1)+i)] = 1
    contador = contador + 1  
  }
}
dim(h4)
rowSums(h4)

h3 = matrix(0,tam_hierarquia[2]*tam_hierarquia[3],dim(b)[1])
contador = 1
for(j in RISP){
  aux1   = unique(dados$CISP[which(dados$RISP_atual==j)])
  aux2   = pmatch(aux1,CISP)
  #print(list(aux1,aux2))
  #print(length(aux1))
  for(i in 1:tam_hierarquia[2]){
    h3[contador,(tam_hierarquia[2]*(aux2-1)+i)] = 1
    contador = contador + 1  
  }
}
dim(h3)
rowSums(h3)

h2 = kronecker(matrix(1,1,tam_hierarquia[5]),diag(1,5))
dim(h2)

h1 = matrix(1,1,dim(b)[1])
dim(h1)

S = rbind(h1,h2,h3,h4,h5)
dim(S)

### Saving objects
save(S,file='data_treated/matriz_S.RData')
write.table(S,file='data_treated/matriz_S.csv',sep=';',row.names=FALSE,col.names=FALSE)

vetor_y = S%*%b
save(vetor_y,file='data_treated/matriz_y.RData')

#vetor_y_test = vetor_y[,(1:(dim(vetor_y)[2]-3))]
#save(vetor_y_test,file='data_treated/matriz_y_teste.RData')

save(CISP,file='data_treated/CISP.RData')
save(AISP,file='data_treated/AISP.RData')
save(RISP,file='data_treated/RISP.RData')
save(dados,file='data_treated/base.RData')

############# Building the hierarchy ##############
tam_hierarquia = c(1,5,length(RISP),length(AISP),length(CISP))

m_groups = matrix(NA,3,185)
rownames(m_groups) = c('Crime','RISP','AISP') 

for(i in 1:tam_hierarquia[2]){
  m_groups[1,seq(i,185,by=5)]=paste('C',i,sep='')
}
sum(is.na(m_groups[1,]))

for(j in RISP){
  aux1   = unique(dados$CISP[which(dados$RISP_atual==j)])
  aux2   = pmatch(aux1,CISP)
  print(list(aux1,aux2))
  #print(length(aux1))
  for(i in 1:tam_hierarquia[2]){
    m_groups[2,(tam_hierarquia[2]*(aux2-1)+i)] = paste(j,'-C',i,sep='')
  }
}
sum(is.na(m_groups[2,]))

for(j in AISP){
  aux1   = unique(dados$CISP[which(dados$AISP_atual==j)])
  aux2   = pmatch(aux1,CISP)
  #print(list(aux1,aux2))
  print(length(aux1))
  for(i in 1:tam_hierarquia[2]){
    m_groups[3,(tam_hierarquia[2]*(aux2-1)+i)] = paste(j,'-C',i,sep='') 
  }
}
sum(is.na(m_groups[3,]))

save(m_groups,file='data_treated/hierarchy.RData')
###################################################
