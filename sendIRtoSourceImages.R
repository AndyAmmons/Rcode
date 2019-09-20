
library(filesstrings)
library(oro.nifti)
library(RNiftyReg)
library(jpeg)
library(mmand)
library(Thermimage)
library(imager)
library(raster)

#### the purpose of this script is to send all ir images in the Images folder
#### to the SourceImages folder, based on the fact that ir images are labeled
#### with an even number.
#### I was a bit reckless when I put this together and wrote the code after I had 
#### changed the file names and locations after extracting source from the IR 
#### images. but hopefully it will work out! (it works)


image.path <- 'c:/Users/RandyCocks/Desktop/R_Projects/Images/'
source.path <- 'c:/Users/RandyCocks/Desktop/R_Projects/SourceImages/'

file.names <- dir(source.path, patter = '.jpg')
source('image reg functions.R')


plantPicFlNum = list(1734,1744,1748,1750,1754,1756,1762,1764,1768,1770,1774,1776,1780,1782,1786,
                     1788,1792,1794,1798,1802,1804,1809,1813,1817,1821,1827,1831,1835,1839,1843,
                     1847,1851,1855,1859,1865,1869,1873,1877,1881,1916,1920,1924,1928,1930,1934,
                     1938,1942,1948,1952,1958,1962,1964,1968,1974,1980,1984,1990,1994,2000,2004,
                     2004,2008,2012,2018,2022,2026,2030,2034,2038,2042,2046,2050,2050,2070,2074,
                     2076,2080,2084,2088,2092,2096,2100,2104,2108,2112,2134,2138,2146,2150,2154,
                     2158,2162,2166,2170,2174,2178,2182,2186,2190,2194,2198,2202,2206,2210)


for (i in plantPicFlNum){
	checkFile <- paste0(image.path,'FLIR',i,'.jpg')
	if (file.exists(checkFile){
		source <- get.IR(i,image.path)
		source <- as.cimg(source)
		save.image(source,paste0(source.path,'source',i,'.jpg'))
	}
}	
