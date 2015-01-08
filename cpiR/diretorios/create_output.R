create_output <- function(intPath, outPath) {  
  
  listInside <- list.dirs(path = intPath)  
  paths      <- strsplit(listInside, split = "/")
  ini        <- length(paths[[1]])
  outPath    <- paste0(outPath,"Output_",paths[[1]][ini])
  ini        <- ini + 1
  
  if(!is.na(paths[1])) {
    lapply(X = paths[-1], FUN = function (x){
      path_add <- x[ini:length(x)]
      ntmp <- character()
      for(i in path_add) ntmp <- paste0(ntmp,"/",i)
      dir.create(path = paste0(outPath,ntmp) , recursive = TRUE)
    })
  } else return
}