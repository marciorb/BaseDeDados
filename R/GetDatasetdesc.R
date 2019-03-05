GetDatasetdesc <- function(dataset)
{
  desc <-""
 
  if (dataset == "GlassLabel6")
  {
    desc <- "Glass"
  }
  else if(dataset == "ArtificialSobreposicao075")
  {
    desc <- "Artificial Overlap 75%"
  }
  else if(dataset == "ArtificialSobreposicao050")
  {
    desc <- "Artificial Overlap 50%"
  }
  else if(dataset == "ecoli")
  {
    desc <- "Ecoli"
  }
  else if(dataset == "haberman")
  {
    desc <- "Haberman"
  }
  else if(dataset == "wine")
  {
    desc <- "Wine"
  }
  else if(dataset == "pima")
  {
    desc <- "Pima"
  }
  else if(dataset == "libra123")
  {
    desc <- "Libra"
  }
  else if(dataset == "Iris")
  {
    desc <- "Iris"
  }
  else if(dataset == "vowel")
  {
    desc <- "Vowel"
  }
  else if(dataset == "mamographic")
  {
    desc <- "Mamographic"
  }
  else if(dataset == "heartnot0")
  {
    desc <- "Heart"
  }
  else if(dataset == "wiscosin")
  {
    desc <- "Wiscosin"
  }
  else if(dataset == "newthyroid")
  {
    desc <- "New-Thyroid"
  }
  else if(dataset == "SPECTFheart")
  {
    desc <- "SPECTF-Heart"
  }
  else if(dataset == "hepatitis")
  {
    desc <- "Hepatitis"
  }
  else
  {
    desc <- dataset
  }

  
  return(desc)
}


GetMetodoDesc <- function(metodo)
{
  desc <-""
  
  if (metodo == "Metodo1")
  {
    desc <- "SOMEntropyKnn1"
  }
  else if(metodo == "Metodo2")
  {
    desc <- "SOMEntropyKnn2"
  }
  else if(metodo == "metodo3")
  {
    desc <- "SOMEntropyKnn3"
  }
  else if(metodo == "Original")
  {
    desc <- "Original"
  }
  else
  {
    desc <- metodo
  }
  
  
  return(desc)
}

GetDatasetdescArray <- function(dataset)
{
  retorno <- NA
  
  for (i in c(1:length(dataset)))
  {
    if (i == 1)
      retorno <- GetDatasetdesc(dataset[i])
    else
      retorno <- c(retorno, GetDatasetdesc(dataset[i]))
  }
  
  return(retorno)
}

GetMetododescArray <- function(metodos)
{
  retorno <- NA
  
  for (i in c(1:length(metodos)))
  {
    if (i == 1)
      retorno <- GetMetodoDesc(metodos[i])
    else
      retorno <- c(retorno, GetMetodoDesc(metodos[i]))
  }
  
  return(retorno)
}