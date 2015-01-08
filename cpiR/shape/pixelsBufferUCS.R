bufferUCS <- function(shape , raster , rInt, rExt, file_name) {
  
  if( proj4string(shape) != SAD_metros() ) shape <- spTransform(x = shape, CRSobj = CRS(SAD_metros()) )
  
  pb <- winProgressBar( title = "progress bar", min = 0,  max = nrow(shape) , width = 300 )  
    
  for (j in 1:nrow(shape)) {
    
    i_tab <- data.frame()
    e_tab <- data.frame()
    
    c <- 0    
    for(i in rInt) {
      if ( !is.null( gBuffer( spgeom = shape[j, ], width = -(i) ) ) ) {
        buf <- gDifference(spgeom1 = gBuffer(spgeom = shape[j, ], width = -(c)),
                           spgeom2 = gBuffer(spgeom = shape[j, ], width = -(i)),
                           byid = TRUE)
        buf <- spTransform(buf, CRS(projection(raster)))
        c <- i
        
        cells <- extract(x = raster, y = buf, na.rm = TRUE, cellnumbers = TRUE)
        xy <- xyFromCell(object = raster, cell = cells[[1]][,1])
        
        tmp <- cbind( xy,cells[[1]][,2],rep(x = i, times = nrow(cells[[1]])) )
        i_tab <- rbind( e_tab , tmp )
      } else break
      
    }
    
    c <- 0
    for(i in rExt) {
      buf <- gDifference(spgeom1 = gBuffer(spgeom = shape[j, ], width = (i)),
                         spgeom2 = gBuffer(spgeom = shape[j, ], width = (c)),
                         byid = TRUE)
      buf <- spTransform(buf, CRS(projection(raster)))
      c <- i
      
      cells <- extract(x = raster, y = buf, na.rm = TRUE, cellnumbers = TRUE)
      xy <- xyFromCell(object = raster, cell = cells[[1]][,1])
      
      tmp <- cbind( xy,cells[[1]][,2],rep(x = i, times = nrow(cells[[1]])) )
      e_tab <- rbind( e_tab , tmp )
    }
    
    colnames(i_tab) <- c("long","lat","atrib","buffer")
    colnames(e_tab) <- c("long","lat","atrib","buffer")
    
    write.table(e_tab,
                paste0(OutDir,file_name,"_",as.character(shape@data$ID_UC0[j]),"_bufferExt_",raster@data@names,".txt"),
                row.names = FALSE)
    
    write.table(i_tab,
                paste0(OutDir,file_name,"_",as.character(shape@data$ID_UC0[j]),"_bufferInt_",raster@data@names,".txt"),
                row.names = FALSE)
    
    setWinProgressBar(pb, j, title=paste("shape: ",file_name," - ",
                                         "raster: ", raster@data@names," - ",
                                         "percentage: ", round(j/nrow(shape)*100, 0),"% done"))
  }
  
  close(pb)
}