  
library(raster)

for(i in 1:23){
  value=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/cslabel.tif")
  classlabel=as.matrix(value)
  band=raster("C:/Users/matti/Google Drive/Classes 2016/Fall 2016/Machine Learning/assignment2/dat/evi_stack/evi_stack.tif", band=i)
  image=as.matrix(band)
  plot(band, axes=FALSE, main=paste(paste("GLB Imagery - Band",i),"\nSSAB Land Cover Classification"))
}