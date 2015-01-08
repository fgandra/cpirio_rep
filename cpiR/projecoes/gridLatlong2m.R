#### PROJETO: HIDROELETICAS TERRACLASS

### BERNARD LUPIAC
### JOAO VILELA

## Corta o grid para o contorno do shape dado e retorna a tabela com os pontos.
## /!\ o shape deve ter 1 só poligono

gridLatlong2m <- function( tabCoords )
{
  ## projecao pelo brasil na unidade 'metros' e em latlong
  proj_em_latlong <- "+proj=longlat +ellps=GRS80 +no_defs"
  proj_em_metro <- "+proj=poly +lat_0=0 +lon_0=-54 +x_0=5000000 +y_0=10000000 +ellps=aust_SA +units=m +no_defs"
  
  ## transforma a tabela em shapefile
  grid <- SpatialPointsDataFrame(coords = tabCoords, data = as.data.frame(tabCoords))
  proj4string(grid) <- proj_em_latlong
  
  grid_m <- spTransform(x = grid, CRSobj = CRS(proj_em_metro))
                         
  ## retorna o vetor
  return (coordinates(grid_m))
}