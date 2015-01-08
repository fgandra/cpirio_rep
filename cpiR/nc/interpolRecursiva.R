subInterpol <- function(intPath, outPath) {
  listFiles <- list.files(intPath, pattern = ".nc",ignore.case = TRUE)               
  ncFiles   <- lapply( X = listFiles,
                       FUN = function(x) brick(paste0(intPath,x)))
  mediaInterpoladaUniversalAMC(ncFiles = ncFiles, municipios = municipios,
                               outputdir = outPath, layerName = nomeAmc) 
}


interpolRecursiva <- function(pathInt, pathOut){
  
  listInside <- list.files(path = pathInt)
  
  if ( is.na(listInside[1]) ) {
    return
  } else {
    lapply(X = listInside, FUN = function(k) {    
      if( testNc( paste0(pathInt,k,"/") ) ) {
        subInterpol(intPath = paste0(pathInt,k,"/") ,outPath = paste0(pathOut,k,"/") )
      } else {
        interpolRecursiva( pathInt = paste0(pathInt,k,"/"), pathOut = paste0(pathOut,k,"/") )
      }                
    })
  }  
}