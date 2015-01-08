library(raster)
library(rgdal)

source("P:/User Documents/blupiac/Projetos_R/Hidreletrica_Terraclass/Scripts/rasterByPoly.R")

brasil <- readOGR(dsn = "P:/User Documents/blupiac/Projetos_R/AMC_chuva/Dados/Input/Shapes/Brasil", layer = "Brasil_Limites" )

lista <- list.files("D:/Reanalisys_II/Analise_resolucao/", pattern = ".nc")
rst <- lapply( X = lista, FUN = function(x) {
  
         tmp = brick( paste0("D:/Reanalisys_II/Analise_resolucao/",x))
         rotacionado <- rotate(tmp[[1]])
         
         if (abs(rotacionado@extent@ymin) != abs(rotacionado@extent@ymax)) {
           tmpBricks <- shift(rotacionado, x = tmp[[1]]@extent@xmin, y = -rotacionado@extent@ymax + 90 )
         }else{
           tmpBricks <- shift(rotacionado, x = tmp[[1]]@extent@xmin)
         }
         
         brasil <- spTransform(x = brasil, CRSobj = CRS(projargs = projection(tmpBricks)))
         brCrop <- rasterByPoly(poly = brasil, mapbit = tmpBricks)         
         somaPixels <- sum(1*(!is.na(values(brCrop))))
         
         print(tmp@file@name)
         print(paste("resolucao: ",res(tmp)[1],res(tmp)[2]))
         print(paste("numero de pixels: ",somaPixels))
         
         return(list(tmp@file@name,res(tmp),somaPixels))
         
         }  
  )



rotacionado <- rotate(brick(file.choose())[[1]])

if (abs(rotacionado@extent@ymin) != abs(rotacionado@extent@ymax)) {
  tmpBricks <- shift(rotacionado, x = 180 - rotacionado@extent@xmax, y = -rotacionado@extent@ymax + 90 )
}else{
  tmpBricks <- shift(rotacionado, x = 180 - rotacionado@extent@xmax)
}

brasil <- spTransform(x = brasil, CRSobj = CRS(projargs = projection(tmpBricks)))
brCrop <- rasterByPoly(poly = brasil, mapbit = tmpBricks)
