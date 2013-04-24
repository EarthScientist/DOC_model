# this function uses an R command called switch that will switch out elements in some lapply() functions that require
#  different metrics be run on different subsets of the list of matrices created in the DOC function.
# 	this was developed by Matthew Leonawicz (mfleonawicz@alaska.edu) to clean up some duplicated lapply() calls in the DOC
#   script.  This is best practice when working with duplicated functions with slightly different metrics being returned for 
#	different groups of extracted data.
f <- function(fun,y,ind=0){
	L <- switch( fun, max=list(max,1), elev=list(mean,1), slope=list(mean,2), sum=list(sum,ind),
		npixels=list(nrow,1),pct=list(function(x,na.rm=T) 100*length(which(x == 19))/length(x),3) )
	unlist(lapply(y, function(x) if(!is.null(x)) L[[1]](x) else NA),,L[[2]])
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
getMetrics <- function(dem, lc, watersheds, pr.stack, pet.stack){
	# generate slope from the dem
	slope <- terrain(dem, opt='slope', unit='degrees', neighbors=8)

	# calculate the total available water by subtracting the common timesteps in pr - pet = total available water
	waterAvail <- pr.stack - pet.stack

	# now lets stack the data together
	terrainStack <- stack(dem, slope, lc, waterAvail)

	# NEW VARIABLE FROM FRANCES Proportion of each watershed in 0 to 5 degree slopes(0 <= slope <= 5) 
	ws.Slp0_5 <- lapply(e,function(x) if(is.null(x[,2])) NA else length(which(x[,2]<=5))/length(x[,2]))

	# now lets extract the data that exist in the maps into a nested list object
	e <- extract(terrainStack,watersheds)
	
	# calculate the summary stats from the extracted data in each of the watersheds
	# here we use the lapply() function in R's base package to loop through the elements
	# in the list and calculate some metrics based on columns in the nested matrices 
	max.elev <- f("max",e); mean.elev <- f("elev",e); mean.slope <- f("slope",e); count.pixels <- f("npixels",e); pct.glacier <- f("pct",e); 
	# total water available at each time step
	total.water <- c()
	for(i in 1:nlayers(waterAvail))	total.water <- cbind(total.water, f("sum",e,i))
	# add column names to the total water matrix
	colnames(total.water) <- paste("total.water.",1:nlayers(waterAvail),sep="")
	# cbind the vectors and the total.water matrix together
	output.metrics.matrix <- cbind(max.elev, mean.elev, mean.slope, pct.glacier, count.pixels, total.water)
	# return the matrix from the function
	return(output.metrics.matrix) 
}

# -------------------------------------------------------------------------------------------------------------------------------------------------
# This function will classify the watersheds based on given metrics derived from the getMetrics() function
#  classification parameters from Frances Biles, being applied functionally below
# here is where we will use what we have learned above and classify the output polygons based on some criteria 
# IF AvgElev < 250 AND MaxElev < 900 AND %SnowIce = 0, THEN QType = 1
# IF AvgElev >= 250 AND AvgElev < 300 AND MaxElev < 900 AND %SnowIce = 0 AND AvgSlope <=20°, THEN QType = 1
# IF AvgElev >=250 AND AvgElev < 300 AND MaxElev < 900 AND %SnowIce = 0 AND AvgSlope > 20°, THEN QType = 2
# IF AvgElev < 300 AND%SnowIce > 0, THEN QType = 2
# IF AvgElev < 300 AND MaxElev >= 900, THEN QType = 2
# IF AvgElev >= 300 AND %SnowIce < 5, THEN QType = 2
# IF AvgElev >= 300 AND % SnowIce >= 5, THEN QType = 3
# IF AvgElev >= 700 AND MaxElev >= 1600, THEN QType = 3

classify.watersheds <- function(metrics.output){
	metrics.output <- cbind(metrics.output,ws.class=NA)
	for(i in 1:nrow(metrics.output)){
		if(all(complete.cases(output[i,]) == TRUE)){
			if((metrics.output[i,2] < 250 & metrics.output[i,1] < 900 & metrics.output[i,4] == 0)|	
				(metrics.output[i,2] >= 250 & metrics.output[i,2] < 300 &
				metrics.output[i,1] < 900 & metrics.output[i,4] == 0 & metrics.output[i,3] <= 20)){

				metrics.output[i,ncol(metrics.output)] <- 1

			}else if((metrics.output[i,2] >= 250 & metrics.output[i,2] < 300 & metrics.output[i,1] < 900 &
				metrics.output[i,4] == 0 & metrics.output[i,3] > 20)| (metrics.output[i,2] < 300 & metrics.output[i,4] > 0)|
				(metrics.output[i,2] < 300 & metrics.output[i,1] >= 900)|(metrics.output[i,2] >= 300 & metrics.output[i,4] < 5)){
				
				metrics.output[i,ncol(metrics.output)] <- 2

			}else if((metrics.output[i,2] >= 300 & metrics.output[i,4] >= 5)| (metrics.output[i,2] >= 700 & metrics.output[i,1] >= 1600)){
				
				metrics.output[i,ncol(metrics.output)] <- 3

			}
		}
	}
	return(metrics.output)
}


# -------------------------------------------------------------------------------------------------------------------------------------------------
# THIS FUNCTION WILL CALCULATE THE MONTHLY AND ANNUAL DISCHARGE IN cubic meters
# !the input is the output matrix from the function classify.watersheds()!
discharge.watersheds <- function(output.metrics.matrix){
	ws.area.m2 <- unlist(lapply(output.metrics.matrix[,5], function(x) x*1000))
	# move the columns of months into a list of numerics
	l=list(); for(i in 6:(ncol(output.metrics.matrix)-1)) l<-append(l,list(output.metrics.matrix[,i]))
	# calculate the monthly water in m3
	water.m3.month <- do.call("cbind",lapply(l,FUN=function(x,y) x*0.001*y, y=ws.area.m2))
	# give the colnames from the input back to the calculated water in m3
	colnames(water.m3.month) <- paste("month",1:ncol(water.m3.month),"m3",sep=".")
	# colnames(water.m3.month) <- colnames(output.metrics.matrix)[7:ncol(output.metrics.matrix)]
	discharge.matrix <- cbind(output.metrics.matrix[,1:5], water.m3.month,year.sum.m3=rowSums(water.m3.month))
	return(discharge.matrix)
}

# -------------------------------------------------------------------------------------------------------------------------------------------------






# -------------------------------------------------------------------------------------------------------------------------------------------------
