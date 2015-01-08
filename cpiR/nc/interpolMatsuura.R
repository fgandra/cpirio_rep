interpolMatsuura <- function(dataList ,input ,output ,municipios ,projection = "+proj=longlat +ellps=GRS80 +no_defs", shpName = "", is.amc = FALSE ) {
  
  require(raster)
  require(rgdal)
  require(rgeos)
  
  brasil <- readOGR(dsn = "P:/User Documents/jvilela/Analises_NC/Data/Input/Shapefiles/Brasil", layer = "Brasil_Limites")
  brasil <- spTransform(brasil, CRS(projection))
  
  municipiosProj <- spTransform(x = municipios, CRSobj = CRS(projection))
  
  lapply(X = dataList , FUN = function(x) {
    
    dados <- read.table( paste0(input,x)  )
    
    grid <- SpatialPointsDataFrame(coords = dados[,1:2], data = as.data.frame(dados[,3:ncol(dados)]))
    proj4string(grid) <- projection
    
    inter <- gIntersects(spgeom1 = brasil, spgeom2 = grid, byid = TRUE)
    
    grid <- grid[as.vector(inter),]
    
    a <- extent(x = grid) # RECEBE AS DIMENSOES DA REGIAO
    r <- raster(a) # CRIA UM "FORMATO" RASTER QUE IRA RECEBER O SHAPE
    res(r) <- 0.5
    r <- rasterize(grid, r, grid@data[,1])
    r@crs <- CRS(projection)
    for(i in 2:ncol(grid)) r <- stack( r ,rasterize(grid, r, grid@data[,i]))
    for(i in 1:length(r@layers)) r[[i]]@crs <- CRS(projection)
    
    b <- brick(r)
    fileName <-  strsplit(x, split = ".txt")[[1]][1]
    b@file@name <- fileName
    
    if(is.amc) {
      mediaInterpoladaUniversalAMC(ncFiles = list(b), municipios = municipiosProj, outputdir = output , layerName = shpName )    
    } else {mediaInterpoladaUniversal(ncFiles = list(b), municipios = municipiosProj, outputdir = output , layerName = shpName )}
  }) 
}