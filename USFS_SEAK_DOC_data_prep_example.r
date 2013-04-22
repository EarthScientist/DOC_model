
# Some R snippets that will aid in the pre-processing of different data sets
## install.packages
# first we need to read in some spatial libraries 
# this can be done in a couple of ways:
# 	 this is a list of the most common spatial packages I use regularly
pkgList <- c("raster","sp","maptools","rgdal","rgeos")

# the packages can be downloaded using the following command
install.packages(pkgList, dependencies=TRUE, repos="http://cran.cnr.Berkeley.edu") 

# then you can use the following command to actually load that library into the current R session
# for a single package...
require(raster) # quotes are not needed for this command

# for a list of packages already installed you can use the following to load them all with a one liner
for(i in pkgList) require(i, character.only=TRUE)


## Read in data
# there are a few ways to read in the various types of raster data
# I will show a couple here

# first and foremost you can read in a single rasterLayer file using the command raster()
# GDAL supported file types can be easily read in using this command
r <- raster("<path_to_file_with_extension>")

# if you have a folder of rasters (as in a time series) you can read in the series to a stack object
# list the files in the folder using the base R list.files()
l <- list.files("<some_path_to_the_files>", pattern=".tif", full.names=TRUE)
# then if you have a new list object with elements for each file in the folder you can stack that with
s <- stack(l)

# stack data
# you can also stack rasterLayer files using the same stack() command
s <- stack(r,r)

# brick data
# this command is particularly useful when working with banded imagery, or NetCDF files
# it will read in a rasterBrick objects with a layer for each band or file in the set
b <- brick("<path_to_a_file_with_multiple_bands_with_extension>")

# get XYZ 
# sometimes it is nice to get the raster data in a tabular format with the XY data attached
# the following command will produce an output matrix with 3 columns and as many rows as there are values
# columns:  lon, lat, values_at_that_location
xyz <- cbind(coordinates(r), getValues(r))

# crop data
# to crop a raster file by an extent you can use the crop() command
r.cropped <- crop(r, <any object that has an extent>) 

# select values 
# to select values within the raster or to find the locations of certain values (particularly useful for reclassification)
ind <- which(values(r) == 1) # returns a list cell locations where that location is TRUE

# can be used in reclassification by doing:
values(r)[ind] <- 2 # where the values will be replaced at those locations with the number 2

# do some basic math with primitives
# 	you can perform some basic math with raster objects in a very native R kind of a way
# +,-,*,/
r2 <- r + r # can also be used with other primitive operators

# there are also ways of getting min/max/mean/sum/etc on stack or brick objects
s.mean <- mean(s)


# shapefiles read
#  shapefile data can be read in using the simple:
shp <- shapefile("<path_to_shapefile.shp>")
•••••
# shapefiles data access
#  accessing the data within a shapefile can be performed with R's subsetting tools
# 	if you want to get to the columns in the attribute table stored in the shapefile
shp[,1] # this will select the 1st column in that shapefile and set the values of each of the polygons to that column

# the next command will allow you to get the underlying polygon information from the shapefile.  These are S4 style objects and you can
#  get into the files attributes using the @ symbol
shp@polygons[[1]] # this will get you the spatial attributes of the 1st polygon listed in the shapefile

# shapefile extract() from raster*
#  if you want to use a shapefile to extract values from a raster* object you can use:
e <- extract(r,shp) # different results returned for different vector objects (points/lines/polygons)

# ISSUES THAT ARCGIS WILL SMOOTH OVER TO SOME EXTENT
# 	extents
#  		R requires that the rasters being processed have the same spatial extent / origin / resolution / reference system
#		use the cropping command to clip a raster* to a smaller extent
		r.crop <- crop(r, extent(r2))
#		if the extent needs to be shifted (meaning all other attributes are identical)
		extent(r) <- extent(r2)
# 	resolutions
# 		there are a number of ways to do this here are a couple
		r.resample <- resample(r, r2) # where r and r2 have different resolutions
		r.aggregate <- aggregate(r,fact=2, fun=mean) # which will aggregate the data by a factor of 2 using the mean
		r.disaggregate <- disaggregate(r.aggregate, fact=2, method='bilinear') # this will use a bilinear interpolation method to diaggregate cells

# 	projection systems
# 		using proj4 strings you can reproject raster* objects
# 		www.spatialreference.org is a wonderful resource to find the right proj4string
		projectRaster(r, res=res(r), crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", method='bilinear')

# 	origins
# this can be tricky and should only be used by someone who understands the potential error involved with modifying these sorts of file attributes
# what I do is convert the raster to points and rasterize them using a raster with a desired origin as the template
# the template map must have the same number of pixels and nrow and ncol as the raster being modified --  THIS IS A TRICKY CONVERSION AND NOT FOR THE FAINT OF HEART
r.pts <- rasterToPoints(r)
r.newOrigin <- rasterize(r.pts,r.desiredOrigin, field=r.pts[,3])

# Please inquire with any questions you have or clarifications needed.  malindgren@alaska.edu



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # #          THAT BEING SAID, HERE IS HOW I PREPROCESSED DATA TO GET IT TO WORK IN THE MODEL                        # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# first we need to read in some spatial libraries 
# this can be done in a couple of ways:
# 	 this is a list of the most common spatial packages I use regularly
pkgList <- c("raster","sp","maptools","rgdal","rgeos")

# the packages can be downloaded using the following command
install.packages(pkgList, dependencies=TRUE, repos="http://cran.cnr.Berkeley.edu") 

# then you can use the following command to actually load that library into the current R session
# for a single package...
require(raster) # quotes are not needed for this command

# for a list of packages already installed you can use the following to load them all with a one liner
for(i in pkgList) require(i, character.only=TRUE)

# here lets bring in the data files that we need for the project and we will need to do some pre-processing to get the data to play nice
# the watershed shapefile can be read in using the shapefile() command.
ws <- shapefile("/workspace/Shared/00_Shared_Project_data/DOC_model/project_data/Extracted_shapefile/ws_pCTRF_N_NAD83Albers.shp")
dem <- raster("/workspace/Shared/00_Shared_Project_data/DOC_model/project_data/trimmed_dem/AKCanada_PRISM_2km_DEM_trimmed_extentFix.tif")
lc <- raster("/workspace/UA/malindgren/projects/NALCMS_Veg_reClass/FINAL/na_landcover_2005_ebm_1km_MASTER.tif")

lc <- aggregate(lc, fact=2, method='modal')

# we have different extents in each of the input rasters which will need to be resolved 
# this can be done with a command called crop in the raster package
# we are cropping the data using the minimum bounding box from the shapefile
dem <- crop(dem,extent(ws)) 
lc <- crop(lc, extent(ws))

# read in the prism precipitation climatology for the period 1961-1990
# this is done here by using an anonymous list.files() function that is listing all of the GeoTiff filenames, 
#  which are being passed to the raster package function stack(), which will create a rasterStack() object
prism.pr <- stack(list.files("/Data/Base_Data/Climate/AK_CAN_2km/historical/singleBand/prism/AK_CAN_2km_PRISM/AK_CAN_geotiffs/pr/ak83albers", pattern=".tif", full.names=TRUE))

# now read in the pet data that Stephanie has created.  These data are monthly pet values and span a period longer than 
# 1961-1990 and therefore we need to loop through the years and months we want to get a chronological list 
pet.clim.files <- character()

for(y in 1961:1990){
	for(m in c("01","02","03","04","05","06","07","08","09","10","11","12")){
		pet.clim.files<-append(pet.clim.files,paste("/workspace/Shared/smcafee4/PET/akcan2km/PEThamon/cruts31/PEThamon_mm.mon_cru_TS31_akcan_2km_",m, "_", y , ".tif",sep=""))
	}
}
# stack the list into a rasterStack
prism.pet <- stack(pet.clim.files)

# here we aggregate the pet data to the 1961-1990 climatology
prism.pet.mean <- stack()
for(i in 1:12){
	print(i)
	prism.pet.mean <- addLayer(prism.pet.mean,mean(prism.pet[[(seq(i,nlayers(prism.pet),12))]]))
}

prism.pet.mean <- crop(prism.pet.mean, extent(ws))
prism.pr <- crop(prism.pr, extent(ws))

