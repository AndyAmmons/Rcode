


#### use Thermimage to read in FLIR IR photo and return matrix of pixel values.


# read in IR file.

get.IR <- function(filenum, folderpath){					#hand function the file number only
	ir.name <- paste0("FLIR", filenum, ".jpg")
	file.in <- paste0(folderpath, ir.name)
	ir.img<-readflirJPG(file.in)
	x <- get.IR.temperatures(ir.img, file.in)
	ir.img <- x[[2]]
	#imgr<-t(ir.img)
	return(ir.img)
}




## Turn Glenn's code into a function

get.IR.temperatures <- function(imgr, file.in){

	cams<-flirsettings(file.in, exiftool="installed", camvals="")

	# Set variables for calculation of temperature values from raw A/D sensor data  ####
	Emissivity<-cams$Info$Emissivity      # Image Saved Emissivity - should be ~0.95 or 0.96
	ObjectEmissivity<-0.96                # Object Emissivity - should be ~0.95 or 0.96
	dateOriginal<-cams$Dates$DateTimeOriginal
	dateModif<-   cams$Dates$FileModificationDateTime
	PlanckR1<-    cams$Info$PlanckR1                      # Planck R1 constant for camera  
	PlanckB<-     cams$Info$PlanckB                       # Planck B constant for camera  
	PlanckF<-     cams$Info$PlanckF                       # Planck F constant for camera
	PlanckO<-     cams$Info$PlanckO                       # Planck O constant for camera
	PlanckR2<-    cams$Info$PlanckR2                      # Planck R2 constant for camera
	OD<-          cams$Info$ObjectDistance                # object distance in metres
	FD<-          cams$Info$FocusDistance                 # focus distance in metres
	ReflT<-       cams$Info$ReflectedApparentTemperature  # Reflected apparent temperature
	AtmosT<-      cams$Info$AtmosphericTemperature        # Atmospheric temperature
	IRWinT<-      cams$Info$IRWindowTemperature           # IR Window Temperature
	IRWinTran<-   cams$Info$IRWindowTransmission          # IR Window transparency
	RH<-          cams$Info$RelativeHumidity              # Relative Humidity
	h<-           cams$Info$RawThermalImageHeight         # sensor height (i.e. image height)
	w<-           cams$Info$RawThermalImageWidth          # sensor width (i.e. image width)

	temperature<-raw2temp(imgr,ObjectEmissivity,OD,ReflT,AtmosT,IRWinT,IRWinTran,RH,
                      PlanckR1,PlanckB,PlanckF,PlanckO,PlanckR2)
	colnames(temperature)<-NULL
	rownames(temperature)<-NULL

	return(list(dateOriginal, temperature))

}
