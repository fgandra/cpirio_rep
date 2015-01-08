path <- "P:/User Documents/jvilela/Analises_NC/Data/Output/"
tabCerta <- as.numeric(as.character(municipios$CD_GEOCODM))
fileNames <- list.files(path = path)

redifineTable <- function(x) {
  
  tmp <- read.table(file = paste0("P:/User Documents/jvilela/Analises_NC/Data/Output/",x)   ,header = TRUE)
  tmp$geoCod_Mun <- tabCerta
  write.table(tmp, paste0("P:/User Documents/jvilela/Analises_NC/Data/Output/",x), row.names = FALSE)
   
  
}

lapply(X = fileNames, FUN = redifineTable)
