# this function uses an R command called switch that will switch out elements in some lapply() functions that require
#  different metrics be run on different subsets of the list of matrices created in the DOC function.
# 	this was developed by Matthew Leonawicz (mfleonawicz@alaska.edu) to clean up some duplicated lapply() calls in the DOC
#   script.  This is best practice when working with duplicated functions with slightly different metrics being returned for 
#	different groups of extracted data.
f <- function(fun,y,ind=0){
	L <- switch( fun, min=list(min,1), max=list(max,1), elev=list(mean,1), slope=list(mean,2), sum=list(sum,ind),
		npixels=list(nrow,1),pct=list(function(x,na.rm=T) 100*length(which(x == 19))/length(x),3) )
	unlist(lapply(y, function(x) if(!is.null(x)) L[[1]](x,na.rm=T) else NA),,L[[2]])
}

# THIS FUNCTION WILL PERFORM THE NEEDED ANALYSIS FOR THE DOC MODELLING PROJECT:
# the inputs to the function need to be prepped prior to being put as arguments to the function
# by "prepped" I mean they must have the same extent/origin/resolution/coordinate reference system
# the inputs to the function are:
# dem = rasterLayer - a digital elevation model covering the AOI
# lc = rasterLayer - a landcover map that shows a glacier / snow & ice class
# watersheds = shapefile - of the watershed map for the AOI
# pr.stack = rasterStack of precipitation layers that will be analyzed for total water available
# pet.stack = rasterStack of pet layers that will be analyzed for total water available
#   ** the pr.stack and the pet.stack must have the same nlayers and be in the same chronological order**
# >> this version of the DOC function requires the above function (currently named f()) is available in the environment
DOC <- function(dem, lc, watersheds, pr.stack, pet.stack){
	# generate slope from the dem
	slope <- terrain(dem, opt='slope', unit='degrees', neighbors=8)

	# calculate the total available water by subtracting the common timesteps in pr - pet = total available water
	waterAvail <- pr.stack - pet.stack

	# now lets stack the data together
	terrainStack <- stack(dem, slope, lc, waterAvail)

	# now lets extract the data that exist in the maps into a nested list object
	e <- extract(terrainStack,watersheds)
	
	# calculate the summary stats from the extracted data in each of the watersheds
	# here we use the lapply() function in R's base package to loop through the elements
	# in the list and calculate some metrics based on columns in the nested matrices 
	min.elev <- f("min",e); max.elev <- f("max",e); mean.elev <- f("elev",e); mean.slope <- f("slope",e); pct.glacier <- f("pct",e)	; count.pixels <- f("npixels",e)	
	# total water available at each time step
	total.water <- c()
	for(i in 1:nlayers(waterAvail))	total.water <- cbind(total.water, f("sum",e,i))
	# add column names to the total water matrix
	colnames(total.water) <- paste("total.water.",1:nlayers(waterAvail),sep="")
	# cbind the vectors and the total.water matrix together
	output.metrics.matrix <- cbind(min.elev, max.elev, mean.elev, mean.slope, pct.glacier, count.pixels, total.water)
	# return the matrix from the function
	return(output.metrics.matrix) 
}


# EXAMPLE BELOW:

# to run the script on SNAP's servers you can run these lines below.
library(raster); library(maptools)
path <- "/workspace/Shared/00_Shared_Project_data/DOC_model/project_data/inputs/Preprocessed_data"
dem <- raster(file.path(path,"dem.tif"))
lc <- raster(file.path(path,"lc.tif"))
s.pr <- brick(file.path(path,"pr_61_90_monthly.tif"))
s.pet <- brick(file.path(path,"pet_61_90_monthly.tif"))
ws <- readShapePoly(file.path(path,"ws_pCTRF_N_NAD83Albers"))
output <- DOC(dem,lc,ws,s.pr,s.pet)



# TESTING AREA


# here is where we will use what we have learned above and classify the output polygons based on some criteria 
# IF AvgElev < 250 AND MaxElev < 900 AND %SnowIce = 0, THEN QType = 1
# IF AvgElev >= 250 AND AvgElev < 300 AND MaxElev < 900 AND %SnowIce = 0 AND AvgSlope <=20°, THEN QType = 1

# IF AvgElev >=250 AND AvgElev < 300 AND MaxElev < 900 AND %SnowIce = 0 AND AvgSlope > 20°, THEN QType = 2
# IF AvgElev < 300 AND%SnowIce > 0, THEN QType = 2
# IF AvgElev < 300 AND MaxElev >= 900, THEN QType = 2
# IF AvgElev >= 300 AND %SnowIce < 5, THEN QType = 2

# IF AvgElev >= 300 AND % SnowIce >= 5, THEN QType = 3
# IF AvgElev >= 700 AND MaxElev >= 1600, THEN QType = 3

classify.watersheds <- function(output.metrics.matrix){
	output.metrics.matrix <- cbind(output.metrics.matrix,NA)
	for(i in 1:nrow(output.metrics.matrix)){
		if(all(complete.cases(output[i,]) == TRUE)){
			if((output.metrics.matrix[i,3] < 250 & output.metrics.matrix[i,2] < 900 & output.metrics.matrix[i,5] == 0)|	(output.metrics.matrix[i,3] >= 250 & output.metrics.matrix[i,3] < 300 & output.metrics.matrix[i,2] < 900 & output.metrics.matrix[i,5] == 0 & output.metrics.matrix[i,4] <= 20)){
				output.metrics.matrix[i,ncol(output.metrics.matrix)] <- 1
			}else if((output.metrics.matrix[i,3] >= 250 & output.metrics.matrix[i,3] < 300 & output.metrics.matrix[i,2] < 900 & output.metrics.matrix[i,5] == 0 & output.metrics.matrix[i,4] > 20)| (output.metrics.matrix[i,3] < 300 & output.metrics.matrix[i,5] > 0)| (output.metrics.matrix[i,3] < 300 & output.metrics.matrix[i,2] >= 900)|(output.metrics.matrix[i,3] >= 300 & output.metrics.matrix[i,5] < 5)){
				output.metrics.matrix[i,ncol(output.metrics.matrix)] <- 2
			}else if((output.metrics.matrix[i,3] >= 300 & output.metrics.matrix[i,5] >= 5)| (output.metrics.matrix[i,3] >= 700 & output.metrics.matrix[i,2] >= 1600)){
				output.metrics.matrix[i,ncol(output.metrics.matrix)] <- 3
			}
		}
	}
	return(output.metrics.matrix)
}


# so now we have a new output.metrics.matrix that can be run through the other suite of equations for the total DOC to be calculated seasonally and annually

# create a lookup list:
propList <- list(QTYPE.1=read.csv('/workspace/Shared/00_Shared_Project_data/DOC_model/project_data/lookup_table/QType1_proportion_annual_discharge.csv'),QTYPE.2=read.csv('/workspace/Shared/00_Shared_Project_data/DOC_model/project_data/lookup_table/QType2_proportion_annual_discharge.csv'),QTYPE.3=read.csv('/workspace/Shared/00_Shared_Project_data/DOC_model/project_data/lookup_table/QType3_proportion_annual_discharge.csv'))


# res(in meters)^2 * npixels
water.sqm <- cellVal*0.001*npixels

