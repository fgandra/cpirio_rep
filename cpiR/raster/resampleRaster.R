resampRast <- function(father, son, method) {
  
  resampled <- list()
  
  if((father@extent != son@extent) || (res(father) != res(son))) {
      if(projection(father) != projection(son))
        son <- projectRaster(son, father, method = method)
      newRast <- resample(son, father, method = method)
    }
  
}