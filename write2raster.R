library(oro.nifti)
library(RNiftyReg)
library(jpeg)
library(mmand)
library(Thermimage)
library(imager)
library(raster)
library(rgdal)

#save the ir, h, s, v raster stack!

image.path <- 'c:/Users/RandyCocks/Desktop/R_Projects/Images/'
source.path <- 'c:/Users/RandyCocks/Desktop/R_Projects/TransformedImages/'
raster.path <- 'c:/Users/RandyCocks/Desktop/R_Projects/RasterStacks/'

file.names <- dir(source.path, pattern = '.jpg')

for (i in file.names){
	psource <- paste0(source.path, i)
	fileNum <- as.numeric(regmatches(i,gregexpr('[[:digit:]]+',i)))
	im.target <- load.image(paste0(image.path, 'FLIR', fileNum + 1, '.jpg'))
	im.target.rgb <- (crop.borders(im.target, nx = 250, ny = 120))
	
	source <- readJPEG(psource)
	source <- matrix(source, ncol = ncol(source), nrow = nrow(source))
	im.target.rgb <- imsub(im.target.rgb, y <= nrow(source), x <= ncol(source))

	wasRGB <- RGBtoHSV(im.target.rgb)
	nowHSV <- channels(wasRGB)
	h <- raster(t(as.matrix(nowHSV[[1]])))
	s <- raster(t(as.matrix(nowHSV[[2]])))
	v <- raster(t(as.matrix(nowHSV[[3]])))
	ir <- raster(source)
	im.r <- stack(h,s,v,ir)
	writeRaster(im.r, filename = paste0(raster.path,fileNum, 'Stack.tif'),format='GTiff',overwrite = TRUE)
	
	
}


