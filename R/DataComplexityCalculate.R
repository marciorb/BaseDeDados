CalculateF1 <-function(data1)
{
  library("dplyr")
  library("tidyr")
  library("fields")
  
  
  grupoClasseMean <- data1 %>% group_by(Y) %>% summarise_all(funs(mean))
  grupoClasseVar <- data1 %>% group_by(Y) %>% summarise_all(funs(var))
  
  
  F1Array <- (grupoClasseMean[grupoClasseMean$Y==0,2:ncol(grupoClasseMean)] - 
                grupoClasseMean[grupoClasseMean$Y==1,2:ncol(grupoClasseMean)])^2/
    (grupoClasseVar[grupoClasseVar$Y==0,2:ncol(grupoClasseVar)] +
       grupoClasseVar[grupoClasseVar$Y==1,2:ncol(grupoClasseVar)])
  
  
  F1 <- max(F1Array)
  F1Parameter <- which.max(F1Array)
  
  df1 <- data.frame(F1, F1Parameter)
  
  return(df1)
}

CalculateF2 <-function(ds1)
{
  library("dplyr")
  library("tidyr")
  library("fields")
  
  
  grupoClasseMax <- ds1 %>% group_by(Y) %>% summarise_all(funs(max))
  grupoClasseMin <- ds1 %>% group_by(Y) %>% summarise_all(funs(min))
  
  minGrupoClasseMax <- grupoClasseMax[,2:ncol(grupoClasseMax)] %>% summarise_all(funs(min))
  maxGrupoClasseMax <- grupoClasseMax[,2:ncol(grupoClasseMax)] %>% summarise_all(funs(max))
  
  minGrupoClasseMin <- grupoClasseMin[,2:ncol(grupoClasseMin)] %>% summarise_all(funs(min))
  maxGrupoClasseMin <- grupoClasseMin[,2:ncol(grupoClasseMin)] %>% summarise_all(funs(max))
  
  
  F2<-prod((minGrupoClasseMax - maxGrupoClasseMin)/(maxGrupoClasseMax-minGrupoClasseMin))
  
  df1 <- data.frame(F2)
  
  return(df1)
}

CalculateF3 <- function(ds1)
{
  grupoClasseMax <- ds1 %>% group_by(Y) %>% summarise_all(funs(max))
  grupoClasseMin <- ds1 %>% group_by(Y) %>% summarise_all(funs(min))
  ncolarrayX <- c(1:(ncol(ds1)-1))
  F3_0 <-apply(ds1[ds1$Y==0,ncolarrayX], 1, VerificaIntervaloD, y=0, grupoClasseMin = grupoClasseMin, grupoClasseMax = grupoClasseMax)
  F3_1 <-apply(ds1[ds1$Y==1,ncolarrayX], 1, VerificaIntervaloD, y=1, grupoClasseMin = grupoClasseMin, grupoClasseMax = grupoClasseMax)
  
  
  F3Array <-(sum(F3_0) + sum(F3_1))/nrow(ds1)
  

  F3 <- max(F3Array)

  F3Parameter <- which.max(F3Array)
  
  return(F3)
}

CalculateD2D3 <- function(ds1, kD2D3)
{
  ncolarrayX <- c(1:(ncol(ds1)-1))

  Xds1 <- ds1[,ncolarrayX]
  Yds1 <- ds1[,ncol(ds1)]
  
  distancia <- rdist(Xds1)
  diag(distancia) = NA
  
  mKnn <-t(apply(distancia,1,GetKNearestIndex, k = kD2D3))
  
  
  D2Array <-t(apply(mKnn,1,CalculaD2Single, XFull = Xds1))
  
  D2 <- sum(D2Array)/nrow(Xds1)
  
  mD3_0 <-t(apply(mKnn[Yds1 == 0,],1,CalculaD3Single, Y = Yds1, yAlvo = 0))
  mD3_1 <-t(apply(mKnn[Yds1 == 1,],1,CalculaD3Single, Y = Yds1, yAlvo = 1))
  
  D3_0 <- sum(mD3_0)
  D3_1 <- sum(mD3_1)
  
  D3 <- (D3_0 + D3_1)/nrow(ds1)
  
  return(cbind(D2,D3_0, D3_1, D3))
}


CalculaD2Single <- function(xIndexKnnArray, XFull)
{
  #obtenho o X do array
  XD2 <- XFull[xIndexKnnArray,]
  
  #obtenho o minimo e maximo para cada feature
  minGrupoClasseD2 <- XD2 %>% summarise_all(funs(min))
  maxGrupoClasseD2 <- XD2 %>% summarise_all(funs(max))
  
  #retorno o produto do max-min entre todos as frequencias
  return(prod(maxGrupoClasseD2 - minGrupoClasseD2))
}

CalculaD3Single <- function(xIndexKnnArray, Y, yAlvo)
{
  #obtenho o Y do array
  arrayvote <- Y[xIndexKnnArray]
  
  dfVote <- plyr::count(arrayvote)
  
  
  
  #retorno o que tem mais frequencia com o que ele passou, se diferente do que passou eu marco
  return(dfVote[which.max(dfVote$freq),1]!=yAlvo)
}

CalculateN2 <-function(ds1)
{
  library("dplyr")
  library("tidyr")
  library("fields")
  
  #ds1 <- as.data.frame(ds1)
  ncolarrayX <- c(1:(ncol(ds1)-1))
  Xds1 <- ds1[,ncolarrayX]
  Yds1 <- ds1[,ncol(ds1)]
  
  distancia <- rdist(Xds1)
  diag(distancia) = NA
  array1 <- which(Yds1 == 1)
  array0 <- which(Yds1 == 0)
  
  IntraClass0 <- distancia[array0,array0]
  IntraClass1 <- distancia[array1,array1]
  InterClass0 <- distancia[array0,array1]
  InterClass1 <- distancia[array1,array0]
  
  
  avgIntra <- base::mean(t(cbind(data.frame(IntraClass0) %>%  summarise_all(funs(min(., na.rm = TRUE))),
                                 data.frame(IntraClass1) %>%  summarise_all(funs(min(., na.rm = TRUE))))))
  
  avgInter <- base::mean(t(cbind(data.frame(InterClass0) %>%  summarise_all(funs(min(., na.rm = TRUE))),
                                 data.frame(InterClass1) %>%  summarise_all(funs(min(., na.rm = TRUE))))))
  
  
  N2 <- avgIntra/avgInter
  
  df1 <- data.frame(N2)
  
  return(df1)
}

GetKNearestIndex<-function(distmatrix, k)
{
  return(order(distmatrix)[c(1:k)])
}

VerificaIntervaloD<-function(x,y, grupoClasseMin, grupoClasseMax)
{
  return(x<grupoClasseMin[grupoClasseMin$Y!=y,2:ncol(grupoClasseMin)]||x>grupoClasseMax[grupoClasseMax$Y!=y,2:ncol(grupoClasseMax)])
}