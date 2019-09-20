library(oro.nifti)
library(RNiftyReg)
library(jpeg)
library(mmand)
library(Thermimage)
library(imager)
library(raster)

#see the alignment of your images!

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
	im.target.c.g <- grayscale(im.target.rgb)

	kernel <- shapeKernel(c(3,3), type='diamond')
	gradient <- mmand::dilate(source,kernel) - mmand::erode(source,kernel)
	mmand::display(t(as.matrix(im.target.c.g)))
	mmand::display(mmand::threshold(gradient, method='kmeans'),add = TRUE, col = 'green')

}

	
	




	