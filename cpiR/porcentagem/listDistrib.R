## listDistrib <- function(shp, attrib) {
##   table(paste0("shp$",attrib))/length(paste0("shp$", attrib))*100  
## }

listDistrib <- function(x) {
  table(x)/length(x[!is.na(x)])*100
}