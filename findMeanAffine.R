library(oro.nifti)
library(RNiftyReg)
library(jpeg)
library(mmand)
library(Thermimage)
library(imager)
library(raster)
library(opencv)




affine.path <- 'c:/Users/RandyCocks/Desktop/R_Projects/affines/'
image.path <- "c:/Users/RandyCocks/Desktop/R_Projects/Images/"
source('image reg functions.R')


############################# get affines and determine the mean ###########################
Astart <- 1748
Aend <- 2198

goodsults <- list(1:55)


index <- 1
for (filenum in Astart:Aend){
	affine.file <- paste0(affine.path, 'AffineMatrixFor_',filenum,'.nii')
	if (file.exists(affine.file)){
		goodsults[[index]] <- readAffine(affine.file)
		index  <- index + 1
	}
}

mean.ft <- goodsults[[1]]

for (i in 2:43){
	mean.ft <- mean.ft + goodsults[[i]]
}
mean.ft <- mean.ft/length(goodsults)

#**************************************************************************************





