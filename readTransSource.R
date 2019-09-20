library(oro.nifti)
library(RNiftyReg)
library(jpeg)
library(mmand)
library(Thermimage)
library(imager)
library(raster)


image.path <- 'c:/Users/RandyCocks/Desktop/R_Projects/Images/'
source.path <- 'c:/Users/RandyCocks/Desktop/R_Projects/TransformedImages/'

filenum = 1744
source <- paste0(source.path, 'Transformedsource1744.jpg')

im.target <- load.image(paste0(image.path, 'FLIR', filenum +1, '.jpg'))
im.target.c.g <- (grayscale(crop.borders(im.target, nx = 250, ny = 120)))

plot(im.target.c.g)

source <- (readJPEG(source))
source <- (matrix(source, ncol = ncol(source), nrow = nrow(source)))

kernel <- shapeKernel(c(3,3), type = 'diamond')
gradient <- mmand::dilate(source,kernel) - mmand::erode(source,kernel)
mmand::display(t(as.matrix(im.target.c.g)))
mmand::display(mmand::threshold(gradient, method = 'kmeans'), add=TRUE, color = 'green')










