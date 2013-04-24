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


# EXAMPLE BELOW:

# to run the script on SNAP's servers you can run these lines below.
library(raster); library(maptools)
path <- "/workspace/Shared/00_Shared_Project_data/DOC_model/project_data/inputs/Preprocessed_data"
dem <- raster(file.path(path,"dem.tif"))
lc <- raster(file.path(path,"lc.tif"))
s.pr <- brick(file.path(path,"pr_61_90_monthly.tif"))
s.pet <- brick(file.path(path,"pet_61_90_monthly.tif"))
ws <- readShapePoly(file.path(path,"ws_pCTRF_N_NAD83Albers"))
output <- getMetrics(dem,lc,ws,s.pr,s.pet)



# TESTING AREA

# this is the classification information from Frances that I am applying in the below function 
# here is where we will use what we have learned above and classify the output polygons based on some criteria 
# IF AvgElev < 250 AND MaxElev < 900 AND %SnowIce = 0, THEN QType = 1
# IF AvgElev >= 250 AND AvgElev < 300 AND MaxElev < 900 AND %SnowIce = 0 AND AvgSlope <=20°, THEN QType = 1

# IF AvgElev >=250 AND AvgElev < 300 AND MaxElev < 900 AND %SnowIce = 0 AND AvgSlope > 20°, THEN QType = 2
# IF AvgElev < 300 AND%SnowIce > 0, THEN QType = 2
# IF AvgElev < 300 AND MaxElev >= 900, THEN QType = 2
# IF AvgElev >= 300 AND %SnowIce < 5, THEN QType = 2

# IF AvgElev >= 300 AND % SnowIce >= 5, THEN QType = 3
# IF AvgElev >= 700 AND MaxElev >= 1600, THEN QType = 3

classifyWS <- function(metrics.output){
	metrics.output <- cbind(metrics.output,ws.class=NA)
	for(i in 1:nrow(metrics.output)){
		if(all(complete.cases(output[i,]) == TRUE)){
			if((metrics.output[i,3] < 250 & metrics.output[i,2] < 900 & metrics.output[i,5] == 0)|	
				(metrics.output[i,3] >= 250 & metrics.output[i,3] < 300 &
				metrics.output[i,2] < 900 & metrics.output[i,5] == 0 & metrics.output[i,4] <= 20)){

				metrics.output[i,ncol(metrics.output)] <- 1

			}else if((metrics.output[i,3] >= 250 & metrics.output[i,3] < 300 & metrics.output[i,2] < 900 &
				metrics.output[i,5] == 0 & metrics.output[i,4] > 20)| (metrics.output[i,3] < 300 & metrics.output[i,5] > 0)|
				(metrics.output[i,3] < 300 & metrics.output[i,2] >= 900)|(metrics.output[i,3] >= 300 & metrics.output[i,5] < 5)){
				
				metrics.output[i,ncol(metrics.output)] <- 2

			}else if((metrics.output[i,3] >= 300 & metrics.output[i,5] >= 5)| (metrics.output[i,3] >= 700 & metrics.output[i,2] >= 1600)){
				
				metrics.output[i,ncol(metrics.output)] <- 3

			}
		}
	}
	return(metrics.output)
}


# so now we have a new output.metrics.matrix that can be run through the other suite of equations for the total DOC to be calculated seasonally and annually

# create a lookup list with the monthly scaling factors for each watershed type:
lookup.path <- "/workspace/Shared/00_Shared_Project_data/DOC_model/project_data/lookup_table" # this is the path to the folder containing the proportion annual discharge tables for each QType
propList <- list(QTYPE.1=read.csv(file.path(lookup.path,'QType1_proportion_annual_discharge.csv')),QTYPE.2=read.csv(file.path(lookup.path,'QType2_proportion_annual_discharge.csv')),QTYPE.3=read.csv(file.path(lookup.path,'QType3_proportion_annual_discharge.csv')))

dischargeWS <- function(classify.output){
	ws.area.m2 <- unlist(lapply(classify.output[,5], function(x) x*1000))
	# move the columns of months into a list of numerics
	l=list(); for(i in 6:(ncol(classify.output)-1)) l<-append(l,list(classify.output[,i]))
	# calculate the monthly water in m3
	water.m3.month <- do.call("cbind",lapply(l,FUN=function(x,y) x*0.001*y, y=ws.area.m2))
	# give the colnames from the input back to the calculated water in m3
	colnames(water.m3.month) <- paste("month",1:ncol(water.m3.month),"m3",sep=".")
	# colnames(water.m3.month) <- colnames(classify.output)[7:ncol(classify.output)]
	discharge.matrix <- cbind(classify.output[,c(1:5,ncol(classify.output))], water.m3.month, year.sum.m3=rowSums(water.m3.month))
	return(discharge.matrix)
}

f2 <- function(fun,y,ind=0){
	L <- switch(fun, winter.1.2=list(c(1,2,3,11,12)), summer.1.2=list(c(4:7)), fall.1.2=list(c(8:10)), winter.3=list(c(1,2,3,4,11,12)), summer.3=list(c(5:10)))
	rowSums(y[,L[[1]]])
	# apply(y[,L[[2]]], 1, L[[1]](x)) 
}

dischargePropWS <- function(discharge.output,propList){
	propDis <- lapply(propList,function(x) x[,3])
	f <- function(x,y) if(is.na(x[1])) NA else x[2] * y[[x[1]]]
	out <- do.call("rbind",apply(discharge.output[,c(6,19)], 1, FUN=f, y=propDis))
	colnames(out) <- paste("month",1:12,"discharge.prop",sep=".")
	out <- cbind(out,discharge.output[,6])
	# create the columnSums needed for different subsets
	winterQ.1.2<-f2("winter.1.2",out); summerQ.1.2<-f2("summer.1.2",out); fallQ.1.2<-f2("fall.1.2",out)
	winterQ.3<-f2("winter.3",out); summerQ.3<-f2("summer.3",out)
	out <- cbind(out,winterQ.1.2=winterQ.1.2,summerQ.1.2=summerQ.1.2,fallQ.1.2=fallQ.1.2,winterQ.3=winterQ.3,summerQ.3=summerQ.3)
	return(out)
}

dischargePropWS(discharge.output,propList)




for(i in 1:length(out)){
if(length(out[[i]])==0){
out[[i]]<-rep(NA,12)
}}


apply(discharge.output[,7:18],1,function(x,y){ propList[[discharge.output[,6]]][,3]},y=discharge.output[,7:18])

f <- function(output.metrics.matrix){
	q.meters3.Mo = apply(output.metrics.matrix[,7:ncol(output.metrics.matrix)], 1, function(x) waterMeters3.Yr * propList[[output.metrics.matrix[,19]]])
}

# TESTING AREA
ws.area.m2 <- unlist(lapply(output.classify[,5], function(x) x*1000))

f2 <- function(x,y){
	count=0
	if(is.na(x) == FALSE){
		count=count+1
		x*0.001*y[count]
	}else{
		count=count+1
		NA
	}
}

f3 <- function(x,y){
	if(is.na(x) == TRUE){
		c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
	}else{
		x[2] * y[[x[1]]]
	}
}

water.m3.month <- apply(output.classify[,7:(ncol(output.classify)-1)], 1,f2, y=ws.area.m2)

Vectorize(f2, vectorize.args = output.classify[,7:(ncol(output.classify)-1)], SIMPLIFY = TRUE, USE.NAMES = TRUE)




	l=list()
	for(i in 7:ncol(df)) append(l,list(df[,i]))

	# water.m3.month <- apply(output.metrics.matrix[,7:(ncol(output.metrics.matrix)-1)], 1, function(x,y) x*0.001*y, y=ws.area.m2)




ws.area.m2 <- unlist(lapply(df[,5], function(x) x*1000))
# move the columns of months into a list of numerics
l=list(); for(i in 6:(ncol(df)-1)) l<-append(l,list(df[,i]))
# calculate the monthly water in m3
water.m3.month <- do.call("cbind",lapply(l,FUN=function(x,y) x*0.001*y, y=ws.area.m2))
# new <- t(water.m3.month)
# give the colnames from the input back to the calculated water in m3
# colnames(water.m3.month) <- paste("total.water.m3.",1:ncol(water.m3.month),sep="")
colnames(water.m3.month) <- colnames(df)[7:ncol(df)]
discharge.matrix <- cbind(df[,1:5], water.m3.month,rowSums(water.m3.month))









