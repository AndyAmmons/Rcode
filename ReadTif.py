#this deals with .tif images from the ~/RasterStacks/ directory.
import numpy as np
import rasterio

raster_path = 'C:/Users/RandyCocks/Desktop/R_Projects/RasterStacks/'

testfilNum = 1734

img = rasterio.open(raster_path + str(testfilNum) + 'Stack.tif')


h = img.read(1)
s = img.read(2)
v = img.read(3)
ir = img.read(4)

