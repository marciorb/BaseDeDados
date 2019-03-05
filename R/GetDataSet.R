#Obtem um DataSet no Formato:
#X -> Matriz com as variaveis de entrada
#Y -> Vetor com a variavel de saida
GetDataSet <- function(nameDataSet)
{
  library(readr)
  library("plyr")
  
  X <- numeric(0)
  Y <- numeric(0)
  
  if (nameDataSet == "Iris")
  {
    dfIris <- iris
    X <- cbind(as.numeric(dfIris$Petal.Length), as.numeric(dfIris$Petal.Width), 
               as.numeric(dfIris$Sepal.Length), as.numeric(dfIris$Sepal.Width))
    colnames(X) <- c("PetalLength", "PetalWidth","SepalLength", "SepalWidth")
    Y <- dfIris$Species
    Y <- mapvalues(Y, from= c("setosa", "versicolor","virginica"), to=c(1,0,1))
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "IrisAlterado")
  {
    DataSet<-iris
    #plot(DataSet$Sepal.Length,DataSet$Sepal.Width,pch=c(16,19),col=DataSet$Species)
    
    DataSet <-  DataSet[sample.int(nrow(DataSet)),]
    
    #DataSet$Species[DataSet$Sepal.Length < 5.7] <- "virginica"
    #DataSet$Species[DataSet$Sepal.Length > 6.5] <- "virginica"
    
    #head(DataSet[which(DataSet$Species=="setosa"),],n=40)[1] 
    
    DataSet[which(DataSet$Species=="virginica"),][1:20,5] <- "setosa"
    X <-cbind(as.numeric(DataSet$Sepal.Length),as.numeric(DataSet$Sepal.Width))
    
    colnames(X) <- c("SepalLength","SepalWidth")
    Y <- as.numeric(DataSet$Species)
    Y <- mapvalues(Y, from=c(1,2,3),to=as.numeric(c(1,1,0)))
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "ArtificialSobreposicao075")
  {
    sobreposicao075 <- read_csv("DataSets/ArtificialLeandro/sobreposicao075.csv", 
                                col_names = FALSE)
    X <- cbind(as.numeric(sobreposicao075$X1), as.numeric(sobreposicao075$X2))
    colnames(X) <- c("X1", "X2")
    Y <- as.numeric(sobreposicao075$X3)
    Y <- mapvalues(Y, from= c("1", "2"), to=c(0,1))
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "ArtificialSobreposicao000")
  {
    sobreposicao075 <- read_csv("DataSets/ArtificialLeandro/sobreposicao000.csv", 
                                col_names = FALSE)
    X <- cbind(as.numeric(sobreposicao075$X1), as.numeric(sobreposicao075$X2))
    colnames(X) <- c("X1", "X2")
    Y <- as.numeric(sobreposicao075$X3)
    Y <- mapvalues(Y, from= c("1", "2"), to=c(0,1))
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "ArtificialSobreposicao050")
  {
    sobreposicao075 <- read_csv("DataSets/ArtificialLeandro/sobreposicao050.csv", 
                                col_names = FALSE)
    X <- cbind(as.numeric(sobreposicao075$X1), as.numeric(sobreposicao075$X2))
    colnames(X) <- c("X1", "X2")
    Y <- as.numeric(sobreposicao075$X3)
    Y <- mapvalues(Y, from= c("1", "2"), to=c(0,1))
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "ArtificialJain2")
  {
    ds <- read_csv("DataSets/ArtificialLeandro/Jain2.csv", 
                                col_names = FALSE)
    X <- cbind(as.numeric(ds$X1), as.numeric(ds$X2))
    colnames(X) <- c("X1", "X2")
    Y <- as.numeric(ds$X3)
    Y <- mapvalues(Y, from= c("1", "2"), to=c(0,1))
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "ArtificialJain2")
  {
    ds <- read_csv("DataSets/ArtificialLeandro/Jain2.csv", 
                   col_names = FALSE)
    X <- cbind(as.numeric(ds$X1), as.numeric(ds$X2))
    colnames(X) <- c("X1", "X2")
    Y <- as.numeric(ds$X3)
    Y <- mapvalues(Y, from= c("1", "2"), to=c(0,1))
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "ArtificialSpiral1")
  {
    ds <- read_csv("DataSets/ArtificialLeandro/spiral.csv", 
                   col_names = FALSE)
    X <- cbind(as.numeric(ds$X1), as.numeric(ds$X2))
    colnames(X) <- c("X1", "X2")
    Y <- as.numeric(ds$X3)
    Y <- mapvalues(Y, from= c("1", "2","3"), to=c(0,1,1))
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "ArtificialSpiral2")
  {
    ds <- read_csv("DataSets/ArtificialLeandro/spiral.csv", 
                   col_names = FALSE)
    X <- cbind(as.numeric(ds$X1), as.numeric(ds$X2))
    colnames(X) <- c("X1", "X2")
    Y <- as.numeric(ds$X3)
    Y <- mapvalues(Y, from= c("1", "2","3"), to=c(0,1,0))
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "ArtificialSpiral3")
  {
    ds <- read_csv("DataSets/ArtificialLeandro/spiral.csv", 
                   col_names = FALSE)
    X <- cbind(as.numeric(ds$X1), as.numeric(ds$X2))
    colnames(X) <- c("X1", "X2")
    Y <- as.numeric(ds$X3)
    Y <- mapvalues(Y, from= c("1", "2","3"), to=c(1,1,0))
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "ArtificialSpiral4")
  {
    ds <- read_csv("DataSets/ArtificialLeandro/spiral.csv", 
                   col_names = FALSE)
    X <- cbind(as.numeric(ds$X1), as.numeric(ds$X2))
    colnames(X) <- c("X1", "X2")
    Y <- as.numeric(ds$X3)
    Y <- mapvalues(Y, from= c("1", "2","3"), to=c(1,0,1))
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "GlassLabel6")
  {
    ds <- read_csv("DataSets/UCI/glass.csv")
    X <- cbind(ds$RI,ds$Na,ds$Mg,ds$Al,ds$Si,ds$K,ds$Ca,ds$Ba,ds$Fe)
    colnames(X) <- cbind("RI","Na","Mg","Al","Si","K","Ca","Ba","Fe")
    Y <- as.numeric(ds$Label)
    Y <- mapvalues(Y, from= c(1,2,3,5,7,6), to=c(1,1,1,1,1,0))
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "ecoli")
  {
    ds <-  read_csv("DataSets/UCI/ecoli.csv")
    X <- cbind(ds$mcg,ds$gvh,ds$lip,ds$chg,ds$aac,ds$alm1,ds$alm2)
    colnames(X) <- cbind("mcg","gvh","lip","chg","aac","alm1","alm2")
    Y <- as.numeric(ds$labelpp)
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "haberman")
  {
    ds <-  read_csv("DataSets/UCI/haberman.csv")
    X <- cbind(ds$Age,ds$Year,ds$Node)
    colnames(X) <- cbind("Age","Year","Node")
    Y <- as.numeric(ds$Survival)
    Y <- mapvalues(Y, from= c(1,2), to=c(1,0))
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "wine")
  {
    ds <-  read_csv("DataSets/UCI/wine.csv")
    X <- cbind(ds$X1,ds$X2,ds$X3,ds$X4,ds$X5,ds$X6,ds$X7,ds$X8,ds$X9,ds$X10,ds$X11,ds$X12,ds$X13)
    colnames(X) <- cbind("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","x13")
    Y <- as.numeric(ds$label)
    Y <- mapvalues(Y, from= c(1,2,3), to=c(1,0,1))
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "pima")
  {
    ds <-  read_csv("DataSets/UCI/pima.csv")
    X <- cbind(ds$X1,ds$X2,ds$X3,ds$X4,ds$X5,ds$X6,ds$X7,ds$X8)
    colnames(X) <- cbind("X1","X2","X3","X4","X5","X6","X7","X8")
    Y <- as.numeric(ds$label)
    Y <- mapvalues(Y, from= c(0,1), to=c(1,0))
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "libra1" || nameDataSet == "libra123")
  {
    ds <-  read_csv("DataSets/UCI/libra.csv")
    X <- as.matrix(ds[,(1:(ncol(ds) -1))])
    
    colnamesx <- numeric(0)
    for (icolName in 1:ncol(X))
    {
      if (icolName == 1)
      {
        colnamesx <- c("X1")
      }
      else
      {
        colnamesx <- cbind(colnamesx, paste("X",icolName,sep = ''))
      }
    }
    
    colnames(X) <- colnamesx
    Y <- as.numeric(ds$label)
    if (nameDataSet == "libra1")
    {
      Y <- mapvalues(Y, from= c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), to=c(0,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
    }
    else if (nameDataSet == "libra123")
    {
      Y <- mapvalues(Y, from= c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), to=c(0,0,0,1,1,1,1,1,1,1,1,1,1,1,1))
    }
    else
    {
      stop("Dataset libra nao encontrado")
    }
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "vowel")
  {
    ds <-  read_csv("DataSets/UCI/vowel.csv")
    X <- as.matrix(ds[,(1:(ncol(ds) -1))])
    
    colnamesx <- numeric(0)
    for (icolName in 1:ncol(X))
    {
      if (icolName == 1)
      {
        colnamesx <- c("X1")
      }
      else
      {
        colnamesx <- cbind(colnamesx, paste("X",icolName,sep = ''))
      }
    }
    
    colnames(X) <- colnamesx
    Y <- as.numeric(ds$label)
    Y <- mapvalues(Y, from= c(0,1,2,3,4,5,6,7,8,9,10), to=c(0,1,1,1,1,1,1,1,1,1,1))
    
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "mamographic")
  {
    ds <-  read_csv("DataSets/UCI/mamographic.csv")
    X <- as.matrix(ds[,(1:(ncol(ds) -1))])
    
    colnamesx <- numeric(0)
    for (icolName in 1:ncol(X))
    {
      if (icolName == 1)
      {
        colnamesx <- c("X1")
      }
      else
      {
        colnamesx <- cbind(colnamesx, paste("X",icolName,sep = ''))
      }
    }
    
    colnames(X) <- colnamesx
    Y <- as.numeric(ds$label)
    Y <- mapvalues(Y, from= c(0,1), to=c(1,0))
    
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "heart4" || nameDataSet == "heartnot0")
  {
    ds <-  read_csv("DataSets/UCI/heart.csv")
    X <- as.matrix(ds[,(1:(ncol(ds) -1))])
    
    colnamesx <- numeric(0)

    for (icolName in 1:ncol(X))
    {
      if (icolName == 1)
      {
        colnamesx <- c("X1")
      }
      else
      {
        colnamesx <- cbind(colnamesx, paste("X",icolName,sep = ''))
      }
    }
    
    colnames(X) <- colnamesx
    Y <- as.numeric(ds$label)

      if (nameDataSet == "heart4")
      {
        Y <- mapvalues(Y, from= c(0,1,2,3,4), to=c(1,1,1,1,0))
      }
      else if (nameDataSet == "heartnot0")
      {
        Y <- mapvalues(Y, from= c(0,1,2,3,4), to=c(1,0,0,0,0))
      }
      else
      {
        stop("Dataset heart nao encontrado")
      }
    
    
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "wiscosin")
  {
    ds <-  read_csv("DataSets/UCI/wiscosin.csv")
    X <- as.matrix(ds[,(1:(ncol(ds) -1))])
    
    colnamesx <- numeric(0)
    for (icolName in 1:ncol(X))
    {
      if (icolName == 1)
      {
        colnamesx <- c("X1")
      }
      else
      {
        colnamesx <- cbind(colnamesx, paste("X",icolName,sep = ''))
      }
    }
    
    colnames(X) <- colnamesx
    Y <- as.numeric(ds$label)
    Y <- mapvalues(Y, from= c(2,4), to=c(1,0))
    
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "newthyroid")
  {
    ds <-  read_csv("DataSets/UCI/newthyroid.csv")
    X <- as.matrix(ds[,(1:(ncol(ds) -1))])
    
    colnamesx <- numeric(0)
    for (icolName in 1:ncol(X))
    {
      if (icolName == 1)
      {
        colnamesx <- c("X1")
      }
      else
      {
        colnamesx <- cbind(colnamesx, paste("X",icolName,sep = ''))
      }
    }
    
    colnames(X) <- colnamesx
    Y <- as.numeric(ds$label)
    Y <- mapvalues(Y, from= c(1,2,3), to=c(1,1,0))
    
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "SPECTFheart")
  {
    ds <-  read_csv("DataSets/UCI/SPECTFheart.csv")
    X <- as.matrix(ds[,(1:(ncol(ds) -1))])
    
    colnamesx <- numeric(0)
    for (icolName in 1:ncol(X))
    {
      if (icolName == 1)
      {
        colnamesx <- c("X1")
      }
      else
      {
        colnamesx <- cbind(colnamesx, paste("X",icolName,sep = ''))
      }
    }
    
    colnames(X) <- colnamesx
    Y <- as.numeric(ds$label)
    Y <- mapvalues(Y, from= c(0,1), to=c(0,1))
    
    Y <- as.factor(Y)
  }
  else if (nameDataSet == "hepatitis")
  {
    ds <-  read_csv("DataSets/UCI/hepatitis.csv")
    X <- as.matrix(ds[,(1:(ncol(ds) -1))])
    
    colnamesx <- numeric(0)
    for (icolName in 1:ncol(X))
    {
      if (icolName == 1)
      {
        colnamesx <- c("X1")
      }
      else
      {
        colnamesx <- cbind(colnamesx, paste("X",icolName,sep = ''))
      }
    }
    
    colnames(X) <- colnamesx
    Y <- as.numeric(ds$label)
    Y <- mapvalues(Y, from= c(1,2), to=c(0,1))
    
    Y <- as.factor(Y)
  }
  else if (startsWith(x = nameDataSet, prefix = "overlap"))
  {
    ds <-  read_csv(paste("DataSets/OverlapArtificial/", nameDataSet, ".csv", sep = ""))
    X <- as.matrix(ds[,(1:(ncol(ds) -1))])
    
    colnamesx <- numeric(0)
    for (icolName in 1:ncol(X))
    {
      if (icolName == 1)
      {
        colnamesx <- c("X1")
      }
      else
      {
        colnamesx <- cbind(colnamesx, paste("X",icolName,sep = ''))
      }
    }
    
    colnames(X) <- colnamesx
    Y <- as.numeric(ds$Y)
    Y <- mapvalues(Y, from= c(0,1), to=c(1,0))
    
    Y <- as.factor(Y)
  }
  else
  {
    stop("Dataset nao encontrado")
  }
  
  
  newList <- list("X" = X, "Y" = Y)
  
  return(newList)
}