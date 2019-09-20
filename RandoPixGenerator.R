library(oro.nifti)
library(RNiftyReg)
library(jpeg)
library(mmand)
library(Thermimage)
library(imager)
library(raster)

#################################
# Issues: 
#
# -The blue dots appear in two places for each call to points() and sometimes outside of the image
# 

folder.path <- "c:/Users/RandyCocks/Desktop/R_Projects/"

fileNumbers = list(1748,1750)#,1756,1762,1764,1768,1774,1776,1780,1782,1786,1788,1794,1809,1813,1817,
                    #1831,1843,1955,1859,1869,1873,1881,1924,1928,1930,1962,1974,1984,1990,2008,2018,
                    #2022,2034,2112,2134,2138,2146,2178,2182,2186,2194,2198)

for (imageNumber in fileNumbers){
  
  IRImage <- readNifti(paste0(folder.path, "nifti", imageNumber, ".nii"))

  im.target <- load.image(paste0(folder.path,"FLIR",imageNumber + 1, ".jpg"))

  im.target.c<- crop.borders(im.target, nx = 250, ny = 120)

  
  test.hsv <- RGBtoHSV(im.target.c)
  chan <- channels(test.hsv) #Extract the channels as a list of images
  layout(1)

  chan.t <- t(chan)

  
  h <- raster(as.matrix(chan[[1]]))
  s <- raster(as.matrix(chan[[2]]))
  v <- raster(as.matrix(chan[[3]]))
  #new.s[new.s < 2] <- NA
  ir <- raster(as(IRImage, "matrix"))
  ir <- t(ir)
  im.r <- raster(h)
  im.r <- stack(h, s, v, ir)

  goodPixels <- list()
  badPixels <- list()

  plot(im.r$layer.4)

  goodIndex = 1
  badIndex = 1
  
  continue <- FALSE

  for (i in 0:2){
  
    x_val <- sample(im.r,1)
    y_val <- sample(im.r,1)
  
    while (all(is.na(x_val)) || all(is.na(y_val)) || x_val[[4]] == 0 || y_val[[4]] == 0) {
      x_val <- sample(im.r,1)
      y_val <- sample(im.r,1)
    }
    #plot(x_val[[4]],y_val[[4]],type="p",pch = 16, col = "blue")
    points(x_val,y_val,type = "p", pch = 16, col = "blue")
  
    continue = FALSE
    userInput <- readline(prompt = "Was that pixel good or bad?('g' or 'b')")
    if (userInput == "g"){
      continue = TRUE
      goodPixels[[goodIndex]] <- list(x_val)
    
      goodIndex = goodIndex + 1
      next
    }
  
    if (userInput == "b"){
      continue = TRUE
      badPixels[[badIndex]] <- list(x_val)
    
      badIndex = badIndex + 1
      next
    }
  }
}
