mediaInterpoladaUniversalAMC <- function(ncFiles, municipios, outputdir,layerName="") {
  
  # arruma projecao
  if( projection(ncFiles[[1]]) != proj4string(municipios) ) {
    municipiosProj <- spTransform(municipios, CRS(projection(ncFiles[[1]])))
  } else municipiosProj <- municipios
  
  pb <- winProgressBar( title = "progress bar", min = 0,  max = length(ncFiles) , width = 300 )
  
  for (i in 1:length(ncFiles)) {
    
    x = ncFiles[[i]]
    
    dimNames  <- c("geoCod_Mun",x@data@names)
    
    tabAno <- data.frame()
    
    if(x@extent@xmax > 180) {
      rotacionado <- rotate(x)
      if (abs(rotacionado@extent@ymin) != abs(rotacionado@extent@ymax)) {
        tmpBricks <- shift(rotacionado, x = x@extent@xmin, y = -rotacionado@extent@ymax + 90 )
      }else{
        tmpBricks <- shift(rotacionado, x = x@extent@xmin)
      }
      
    } else tmpBricks <- x
    
    
    
    interpol <- extract(x = tmpBricks, 
                        y = municipiosProj, 
                        method = 'bilinear', 
                        along = TRUE,
                        fun = mean,
                        weigth = TRUE,
                        na.rm = TRUE)
    
    geoCodigos <- as.character(municipiosProj@data[,1])
    tabAno <- cbind(geoCodigos,interpol)
    
    colnames(tabAno) <- dimNames
    
    fileName <-  strsplit(x@file@name, split = "[\\]")
    fileName <- fileName[[1]][length(fileName[[1]])]
    fileName <-  strsplit(fileName, split = ".nc")[[1]][1]
    
    write.table(x =  tabAno, file = paste0(outputdir,layerName,fileName,".txt"), row.names = FALSE)
    
    gc()
    
    removeTmpFiles(h = 12)
    
    setWinProgressBar(pb, i, title=paste( round(i/length(ncFiles)*100, 0),"% done"))
    
  }
  
  close(pb)
}