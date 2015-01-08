# BERNARD JEAN LUPIAC 

rasterByPoly <- function(poly, mapbit) {
  require(rgeos)
  require(raster)
  
  # Da o crop
  rasterBB <- crop(x = mapbit, y = poly)
  
  # Corta o contorno
  rasterCropped <- mask(x = rasterBB, mask = poly)
  
  rasterCropped
  
}