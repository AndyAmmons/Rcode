

###### need to automate steps for image registration and
###### extraction.

# Steps:
# 1. Read in IR image and extract pixel temperatures using Glenn's code
# 2. Load associated visual image, crop, convert to grayscale (package imager)
# 3. Register images using niftyreg
# 4. Use info in visual image to extract IR temperatures.

# first install the packages using install.packages()
# e.g.,
#install.packages("raster")	#etc. Some packages may need a few extra commands.

# e.g., I know that thermimage https://github.com/gtatters/Thermimage requires something like

#install.packages("Thermimage", repos='http://cran.us.r-project.org')
#install.packages("mmand")
#install.packages("jpeg")
#install.packages("imager")
#install.packages("ExifToolGUI")
#


# then activate them with library()
library(oro.nifti)
library(RNiftyReg)
library(jpeg)
library(mmand)
library(Thermimage)
library(imager)
library(raster)


# put all functions in here; this mainly contains code from thermimage turned into functions

source("image reg functions.R")


## step 1: get IR image

# pairs in folder 1756, 1924, 2233 # these numbers refer to the number of the IR imaged. The associated viz image is num + 1
filenum <- 1924

folder.path <- "c:/Users/RandyCocks/Desktop/R_Projects/"
#folder.path <- "C:/Users/art.woods/Dropbox/uClim/UWyoming_bio_station_2018/IR_images/"
source <- get.IR(filenum, folder.path)
mmand::display(source)

## step 2: load associated viz image, crop, and convert
## Andrew, I don't know for sure that cropping matters, but it seemed like a good idea to make the
## images approximately the same field of view for starters.

im.target <- load.image(paste0(folder.path, "FLIR", filenum + 1, ".jpg"))
im.target.c.g<- grayscale(crop.borders(im.target, nx = 250, ny = 120))

plot(image(t(flip(im.target.c.g))))

#mmand::display(t(as.matrix(im.target.c.g)))

## step 3: register images
## I had some email correspondence with Jon Clayden, the author of the nifty reg package, which I'll forward to you.
## He suggested that nLevels command was important. I played around it, and it seems like nLevels = 8 is good for some
## pairs but not others. So one general-ish problem will be to find a flexible, general way of getting this to work for
## all pairs.

result <- niftyreg(source, t(as.matrix(im.target.c.g)), nLevels = 8) 
kernel <- shapeKernel(c(3,3), type="diamond")
gradient <- mmand::dilate(result$image,kernel) - mmand::erode(result$image,kernel)
mmand::display(t(as.matrix(im.target.c.g)))
mmand::display(mmand::threshold(gradient,method="kmeans"), add=TRUE, col="red")

mmand::display(result$image)

## step 4: pick out IR pixels based on viz info.
# see code chunk at bottom: transform images into HSV from RGB, then 
# take them apart into the three layers, make a rasterStack, then add the aligned
# IR image (4 layers total).

# then need some process for picking out good pixels and bad and then doing statistics
# on those to find ways to discriminate them using hte 4 values for each pixel.
# Andrew, check out the documentation on the imager package--Awesome stuff!: https://dahtah.github.io/imager/imager.html


###############################
######### the chunk below takes the affine matrices for the two good fits and calculates an
# average matrix, which I then apply to a new image. Works.

results1924 <- result	# good one
results1756 <- result	# good one
results2233 <- result	# bad one

# take the mean of the forwardTransforms for the good results and apply to the bad.

x <- forward(results1924)
y <- forward(results1756)
m <- rowMeans(cbind(as.vector(x), as.vector(y)), na.rm=TRUE)
fT.mean <- x
fT.mean[1:4, 1:4] <- m

new.s <- applyTransform(fT.mean, source)
kernel <- shapeKernel(c(3,3), type="diamond")
gradient <- mmand::dilate(new.s,kernel) - mmand::erode(new.s,kernel)
mmand::display(mmand::threshold(gradient,method="kmeans"), add=TRUE, col="green")
mmand::display(target)
mmand::display(new.s, add = TRUE)
## works! 


######################### good talk today with Doug Brinkerhoff
"
1. He suggested that probably the transformation (affine) is close to being the same for every
pair of images. So I can do a set and then get the average affine forward transformation (which is
what I do in teh block above), and then apply it to the rest of the images. Think this is good for
whenever the geometry of the vis and IR cameras doesn't change during a session.

2. We then talked over the subsequent problem of how to separate good from unwanted pixels. He suggested
transforming into HSV (hue, saturation, value) space then adding the aligned IR image (so stack of 4).
Then pick out a 'training set' of good leaf pixels and a set of everything else. He thought H and S
would be especially powerful at discriminating leaf from nonleaf across both sunny and shady conditions.
He'll help with figuring out statistics to do this well.
"


##### next task is to transform pictures into HSV space, which package imager can do.

filenum <- 2233
folder.path <- "C:/Users/art.woods/Dropbox/uClim/UWyoming_bio_station_2018/IR_images/"
im.target <- load.image(paste0(folder.path, "FLIR", filenum + 1, ".jpg"))
display(im.target)

im.target.c<- crop.borders(im.target, nx = 250, ny = 120)


test.hsv <- RGBtoHSV(im.target.c)
chan <- channels(test.hsv) #Extract the channels as a list of images
mmand::display(chan[[1]])
mmand::display(chan[[2]], add = TRUE)
layout(1)

chan.t <- t(chan)

mmand::display(t(flip(as.matrix(chan[[3]]))))


mmand::display(new.s)

# can I crop chan?
##chan.c<- crop.borders(chan, nx = 250, ny = 120)


#### next task is to interactively click on pixels to extract info.
## possibly use raster package

#### now can I make a raster object with all four layers?

h <- raster(as.matrix(chan[[1]]))
s <- raster(as.matrix(chan[[2]]))
v <- raster(as.matrix(chan[[3]]))
#ir <- raster(t(as.matrix(result$image, nrow = 720)))


#im.r <- stack(h, s, v, ir)
im.r <- stack(h,s,v)

plot(im.r)


while (all(is.na(x_val)) || all(is.na(y_val))) {
  x_val <- sample(im.r,1)
  y_val <- sample(im.r,1)
}



# collect 'good' values (blue = good).
pxy <- locator(1) # click on plot where you want to query pixel value
#extract(im.r, cbind(x_val, y_val),cellnumbers = TRUE) # returns value associated with click query

#pxy <- locator(15) # click on plot where you want to query pixel value
points(y_val,y_val, pch = 16, col = "red")

#GOOD <- extract(im.r,SpatialPoints(pxy))

# collect 'bad' values (red = bad).

#qxy <- locator(1) # click on plot where you want to query pixel value
#extract(im.r, cbind(qxy$x, qxy$y),cellnumbers = TRUE) # returns value associated with click query

#qxy <- locator(15) # click on plot where you want to query pixel value
#points(qxy$x, qxy$y, pch = 16, col = "red")

#BAD <- extract(im.r, SpatialPoints(qxy))



"
plot(im.r, 3)
plot(im.r, 4, zlim = c(6, 11))
plot(im.r, 3, add = TRUE, alpha = 0.2)
"





