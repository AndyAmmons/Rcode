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
  
  plot(im.target.c)
  
  
  goodIndex = 1
  badIndex = 1
  
  for (i in 1:10){
    
    if(i %% 10 == 0) plot(im.target.c)		# refresh image every 10 points
    
    x_val <- sample(1:780, 1)	# before you had this as the image, and it therefore samples the image *values*.
    # but what it needs is to sample random x and y *locations* in the image.
    y_val <- sample(1:720, 1)
    
    cells <- cellFromRowCol(im.r, x_val, y_val)
    z_stack <- extract(im.r, cells)
    
    if(!anyNA(z_stack)){	# this just skips over any randomly chosen pixels that have an NA in extracted vector
      
      #plot(x_val[[4]],y_val[[4]],type="p",pch = 16, col = "red")
      points(x_val,y_val,type = "p", pch = 16, col = "red")
      
      userInput <- readline(prompt = "Was that pixel good or bad?('g' or 'b')")
      
      if (userInput == "g"){
        goodPixels[[goodIndex]] <- list(z_stack)
        goodIndex = goodIndex + 1
        
      }
      
      if (userInput == "b"){
        badPixels[[badIndex]] <- list(z_stack)
        badIndex = badIndex + 1
      }
    }
  }
  write.csv(badPixels,paste0(folder.path,"BadVals",imageNumber,".csv"),row.names = FALSE)
  write.csv(goodPixels,paste0(folder.path,"GoodVals",imageNumber,".csv"),row.names = FALSE)
  
}





