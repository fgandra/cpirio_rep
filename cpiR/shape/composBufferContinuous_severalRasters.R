## Da a composicao de classes de um raster dentro de um buffer de um ponto dado

#-------------------------------------------
# argumentos:
# shapefile: arquivo com os pontos
# mapabit: arquivo raster
# width: tamanho do buffer (em metros)
# FUN: função a ser aplicada dentro do buffer
# file: nome do arquivo a ser escrito
# cnames: nome das colunas no arquivo a ser escrito
#-------------------------------------------

#-------------------------------------------
# retorno:
# um arquivo composBuffer.txt onde:
# - uma linha é um ponto no arquivo shapefile
# - uma coluna é a porcentagem de cada atributo
#   dentro do buffer
#-------------------------------------------

source(paste0(CodeDir, "rasterByPoly.R"))

composBufferContinuousSR <- function(shapefile, mapabit, width, FUNC, cnames, bufwd , tmpdr = rasterOptions()$tmpdir) {
  
  require(rgeos)
  require(raster)
  
  rasterOptions(tmpdir = tmpdr)
  
  # matrix onde serao armazenados os resultados
  final = matrix(nrow=0, ncol=length(cnames))
  
  # cria buffer
  shapefileBuf <- gBuffer(spgeom = shapefile, byid = TRUE, width = width)
  #  shapefileBuffer <- spTransform(shapefileBuffer, CRS(projection(mapabit[[1]])))
  
  # projeta o shapefile
  shapefileProj <- spTransform(shapefile, CRS(projection(mapabit[[1]])))
  
  # loop over buffers
  for(i in 1:nrow(shapefile)) {
    
    if (bufwd != 0) {
      tmpBuffer <- gBuffer(spgeom = shapefile[i, ], byid = TRUE, width = bufwd)
      shapefileBuffer <- gDifference(spgeom1 = shapefileBuf[i, ] ,spgeom2 = tmpBuffer ,byid = T)
    }
    
    if (bufwd != 0) {
      shapefileBuffer <- spTransform(shapefileBuffer, CRS(projection(mapabit[[1]])))
    } else {
      shapefileBuffer <- spTransform(shapefileBuf[i, ], CRS(projection(mapabit[[1]])))
    }
    
    rasterCropped <- lapply (X = mapabit ,
                             FUN = function (x) {rasterByPoly(poly = shapefileBuffer, mapbit = x)}
    )
    
    # use the cropped rater, perform and store the 'cal'culation, 'tot'al
    # number of pixels, and number of 'mis'sing pixels
    cal = matrix(data = sapply (X = rasterCropped,
                                FUN =  function (x) {FUNC(as.vector( x[!is.na(x)] ) ) }),
                 ncol = length(mapabit))

#   tot = length(as.vector(rasterCropped))
    tot = length(as.vector(rasterCropped[[1]][!is.na(rasterCropped[[1]])]))

#     mis = sum(as.vector(is.na(rasterCropped)))
    
    # check how many points are inside de buffer
    # obs: the number of TRUE's is the number of points inside the buffer
#     mat <- gContains(spgeom1 = shapefileBuffer, spgeom2 = shapefileProj, byid = T)
    if (bufwd != 0) {
      tmp <- spTransform(shapefileBuf[i, ] , CRS(projection(mapabit[[1]])))
      mat <- gContains(spgeom1 = tmp, spgeom2 = shapefileProj, byid = T)
    } else {
      mat <- gContains(spgeom1 = shapefileBuffer, spgeom2 = shapefileProj, byid = T)
    }

    hidros <- table(mat)["TRUE"][[1]]
    
#     tab = cbind(cal,tot,mis,hidros)
    tab = cbind(cal,tot,hidros)
    
    final = rbind(final, tab)
    
    # remove temporary files, to clear the hard drive
    if (i%%120 == 0) removeTmpFiles(h = 12)
  }
  
  row.names(final) <- NULL
  colnames(final) <- cnames
  
  # adiciono id's
  final <- data.frame(
    idbig = shapefile@data$ID_USI_BIG,
    data_inicio_sigel = shapefile@data$INIC_OPER,
    nome = shapefile@data$Nome,
    tipo = shapefile@data$Tipo,
    lat_sigel = shapefile@data$Latitude,
    lon_sigel = shapefile@data$Longitude,
    estagio = shapefile@data$ESTAGIO,
    final
  )
  
  return(final)
  
}