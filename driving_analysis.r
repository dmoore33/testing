#################################################
#												#
#	Analysis of sensum driving data (phase 1)	#
#												#
#################################################

# Library's Required

library(zoo)
library(RPostgreSQL)
library(DBI)
library(RHRV)
library(zoo)
library(mgcv)
library(MARSS)
library(scales)
library(pracma)

require("readr")
require("dplyr")
require("ggplot2")
require("leaflet")
require("tidyr")
require("lubridate")
require("stringi")
require("RColorBrewer")
require("osmdata")
require("magrittr")
require("osmar")
require("geosphere")
require("mapview")

##################################################
# If we only have session ID 'create_all_files' \\
# seperates each metric into individual file
create_all_files <- function(session_id){
	session_id <- as.character(session_id)
	pg <- DBI::dbDriver("PostgreSQL") 
	db <- DBI::dbConnect(drv = pg, 
	                  	 user="eaidevmaster",
	                  	 password="eaidevmaster",
	                  	 host="eai-dev-rr.cbxpfybyjgtq.eu-west-1.rds.amazonaws.com",
	                  	 dbname="eai")
	qr <- paste0("SELECT metric, value, EXTRACT(EPOCH FROM time AT TIME ZONE 'UTC') *1000 AS time FROM ts_combined WHERE session_id = '",session_id,"';")
	api_data <- DBI::dbGetQuery(db, qr) # database, SQLquery
	DBI::dbDisconnect(db)
	#
	metric_names <- unique(api_data$metric)
	#
	for(i in 1:length(metric_names))
	{
		data <- api_data %>%
			dplyr::filter(metric == metric_names[i]) %>%
			dplyr::select(time, value)
		file_name <- paste0(metric_names[i], ".csv")
		write.csv(data, file_name, row.names=FALSE)
	}
}

# **Sessions that needed to be  downloaded** #
# setwd(wrkdir); setwd(dirs[4])
# create_all_files("e1ebb17e-bd12-4c28-814c-d281dbb1f5af")
# setwd(wrkdir); setwd(dirs[9])
# create_all_files("4bc5521e-5c2c-46e2-bcc2-383b1cb27b2a")
# setwd(wrkdir); setwd(dirs[11])
# create_all_files("d97b7d97-d300-4cac-9713-119ee0f41efc")
# setwd(wrkdir); setwd(dirs[12])
# create_all_files("1eb2d1b2-6d3a-43ab-a69b-57154dba9406")
# setwd(wrkdir); setwd(dirs[5])
# create_all_files("d659bc43-b602-47d4-8819-4582c62fbb9e")

calc_route_dist <- function(mat)
{
	earth.radius <- 6378.137
	rad          <- pi/180
#	RadMat       <- mat * rad
	vec1 <- vector(length=length(mat[,1])-1)
#	from = RadMat[1:((dim(RadMat)[1])-1),]; colnames(from) <- c("lon", "lat")
#	to   = RadMat[2:(dim(RadMat)[1]),];		colnames(to)   <- c("lon", "lat")
	for(i in 1:(dim(mat)[1])-1)
	{
		# signif corrects the deviation in radians 
		lat_1 <- signif(mat[i, 2]  , 7) * rad
		lon_1 <- signif(mat[i, 1]  , 7) * rad
		lat_2 <- signif(mat[i+1, 2], 7) * rad
		lon_2 <- signif(mat[i+1, 1], 7) * rad
		dlon <- lon_2 - lon_1
		dlat <- lat_2 - lat_1
		a <- (sin(dlat/2))^2 + cos(lat_1) * cos(lat_2) * (sin(dlon/2))^2
		c <- 2 * atan2(sqrt(a), sqrt(1 - a))
		d <- earth.radius * c
		vec1[i] <- d * 1000		
	}
	return(vec1)
}


calc_feature_dist <- function(mat, gps)
{
	earth.radius <- 6378.137
	rad          <- pi/180
#	RadMat       <- mat * rad
	vec1 <- vector(length=length(gps[,1]))
#	from = RadMat[1:((dim(RadMat)[1])-1),]; colnames(from) <- c("lon", "lat")
#	to   = RadMat[2:(dim(RadMat)[1]),];		colnames(to)   <- c("lon", "lat")
	for(i in 1:(dim(gps)[1]))
	{
		DistMat <- vector(length=length(mat[,1]))

		lat_1 <- signif(gps[i, 2]  , 7) * rad	
		lon_1 <- signif(gps[i, 1]  , 7) * rad
		
		for(j in 1:dim(mat)[1])
		{
			lat_2 <- signif(mat[j, 2], 7) * rad
		    lon_2 <- signif(mat[j, 1], 7) * rad
		    
		    dlon <- lon_2 - lon_1
			dlat <- lat_2 - lat_1
			a <- (sin(dlat/2))^2 + cos(lat_1) * cos(lat_2) * (sin(dlon/2))^2
			c <- 2 * atan2(sqrt(a), sqrt(1 - a))
			d <- earth.radius * c
			DistMat[j] <- d * 1000
		}
		vec1[i] <- min(DistMat)
	}
	return(vec1)
}

data <- calc_feature_dist(trunk, DriverRoute)



		# signif corrects the deviation in radians 
		lat_1 <- signif(mat[i, 2]  , 7) * rad
		lon_1 <- signif(mat[i, 1]  , 7) * rad
		lat_2 <- signif(mat[i+1, 2], 7) * rad
		lon_2 <- signif(mat[i+1, 1], 7) * rad
		dlon <- lon_2 - lon_1
		dlat <- lat_2 - lat_1
		a <- (sin(dlat/2))^2 + cos(lat_1) * cos(lat_2) * (sin(dlon/2))^2
		c <- 2 * atan2(sqrt(a), sqrt(1 - a))
		d <- earth.radius * c
		vec1[i] <- d * 1000		
	}
	return(vec1)
}


time_convert <- function(x, format="hours", start=FALSE)
{
	# Avilable Formats: "hours", "minutes", "seconds"
	# Format to be in POSIXct [x]
	# if start == TRUE time is indexed from 0; else not
	x     <- as.vector(as.character(x))
	yr_tm <- unlist(strsplit(x, " "))
	PS_tm <- grep(":", yr_tm, value=TRUE)
	if(format == "hours")
	{
		tm_hr <- unlist(sapply(strsplit(PS_tm, ":"), function(x) (as.numeric(x[1])) + (as.numeric(x[2])/60) + ((as.numeric(x[3])/60)/60), simplify=FALSE))
		if(start==TRUE){ return(tm_hr - tm_hr[1])} else {return(tm_hr)}
	}
	if(format == "minutes")
	{
		tm_mn <- unlist(sapply(strsplit(PS_tm, ":"), function(x) ((as.numeric(x[1])) + (as.numeric(x[2])/60) + ((as.numeric(x[3])/60)/60))*60, simplify=FALSE))
		if(start==TRUE){ return(tm_mn - tm_mn[1])} else {return(tm_mn)}
	}
	if(format == "seconds")
	{
		tm_sc <- unlist(sapply(strsplit(PS_tm, ":"), function(x) (((as.numeric(x[1])) + (as.numeric(x[2])/60) + ((as.numeric(x[3])/60)/60))*60)*60, simplify=FALSE))
		if(start==TRUE){ return(tm_sc - tm_sc[1])} else {return(tm_sc)}
	}
}


##################################################

# Local Data
wrkdir <- "~/KTP/data/driver_studyP2/raw_data/"
setwd(wrkdir); dirs <- list.files()

# Time setting to milliseconds (for merge)
options(digits.secs=3)

# List to store ALL files
raw_data <- list()

# Clean data and aggregate
# Read ALL data in each folder into a LIST | in LIST format
for(i in 1:length(dirs))
{
	setwd(dirs[i])
	driver_l <- list()
	files    <- list.files() 
	files    <- files[grep("csv", files)]
		for(j in 1:length(files))
		{
			temp_data     <- read.delim(files[j], sep=",", header=FALSE, stringsAsFactors=FALSE)
			driver_l[[j]] <- temp_data
		}
	driver_l[[j+1]] <- files		
	raw_data[[i]]   <- driver_l
	setwd(wrkdir)
}

# Metrics We Want
m1 <- "heartrate" 
m2 <- "gsr"
m3 <- "location" # multiple outputs (6)

merged_data <- list()

for(i in 1:length(raw_data))
{
	gps_list <- list() # required to store each gps column
	#
	hr.i  <- grep(m1, raw_data[[i]][[length(raw_data[[i]])]])
	gsr.i <- grep(m2, raw_data[[i]][[length(raw_data[[i]])]])
	if(sum(gsr.i)==0){print("No GSR data"); print(i); gsr_present="NO"} else {gsr_present="YES"}
	gps.i <- grep(m3, raw_data[[i]][[length(raw_data[[i]])]])
	#
	for(j in 1:length(gps.i))
	{
		temp <- raw_data[[i]][[gps.i[j]]] %>%
			dplyr::distinct(V1, .keep_all = TRUE)
		if(class(temp[1,1]) == "character"){ temp <- temp[-1,]} 
		temp$V1 <- as.POSIXct(as.numeric(as.character(temp$V1))/1000, origin = "1970-01-01", tz="Europe/London")			
		gps_list[[j]] <- zoo::zoo(x = temp$V2, order.by = temp$V1)
	}
	#
	gps_names <- gsub(".csv", "",raw_data[[i]][[length(raw_data[[i]])]][gps.i])
	#
	HR    <- raw_data[[i]][[hr.i[1]]] %>%
		dplyr::distinct(V1, .keep_all = TRUE)
	if(class(HR[1,1]) == "character"){ HR <- HR[-1,]} 
	HR$V1 <- as.POSIXct(as.numeric(as.character(HR$V1))/1000, origin = "1970-01-01", tz="Europe/London")			
	HR    <- zoo::zoo(x = HR$V2, order.by = HR$V1)
	#
	if(gsr_present=="YES"){	
		GSR   <- raw_data[[i]][[gsr.i[1]]] %>%
			dplyr::distinct(V1, .keep_all = TRUE)
		if(class(GSR[1,1]) == "character"){ GSR <- GSR[-1,]} 
		GSR$V1 <- as.POSIXct(as.numeric(as.character(GSR$V1))/1000, origin = "1970-01-01", tz="Europe/London")
		GSR    <- zoo::zoo(x = GSR$V2, order.by = GSR$V1)
		#
		merged_data[[i]] <- zoo::merge.zoo(gps_list[[1]], gps_list[[2]],
										   gps_list[[3]], gps_list[[4]],
										   gps_list[[5]], gps_list[[6]],
										   HR, GSR)
		colnames(merged_data[[i]]) <- c(gps_names, "heart_rate", "GSR")
	} else {
		merged_data[[i]] <- zoo::merge.zoo(gps_list[[1]], gps_list[[2]],
										   gps_list[[3]], gps_list[[4]],
										   gps_list[[5]], gps_list[[6]],
										   HR)
		colnames(merged_data[[i]]) <- c(gps_names, "heart_rate")	
	}
	#time <- merged_data[[i]]
	#merged_data[[i]] <- cbind(time, merged_data[[i]])
	#colnames(merged_data[[i]])[1] <- "time"
	merged_data[[i]] <- as.data.frame(merged_data[[i]])
	setwd(wrkdir)
	write.csv(merged_data[[i]], paste0("driver",i, "_", dirs[i], ".csv"), row.names=TRUE)
}


############################ ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ############################

############################ CAPTURE HEARTRATE BASELINES ############################
############################ CAPTURE HEARTRATE BASELINES ############################

############################ ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ############################

	session_id <- as.character(session_id)
	pg <- DBI::dbDriver("PostgreSQL") 
	db <- DBI::dbConnect(drv = pg, 
	                  	 user="eaidevmaster",
	                  	 password="eaidevmaster",
	                  	 host="eai-dev-rr.cbxpfybyjgtq.eu-west-1.rds.amazonaws.com",
	                  	 dbname="eai")
	qr <- paste0("SELECT metric, value, EXTRACT(EPOCH FROM time AT TIME ZONE 'UTC') *1000 AS time FROM ts_combined WHERE session_id = '",session_id,"';")
	api_data <- DBI::dbGetQuery(db, qr) # database, SQLquery
	DBI::dbDisconnect(db)


color.gradient <- function(x, colors=c("green","orange","red"), colsteps=500) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

session_id <-  "88f26655-76c9-4cc0-847d-bc8af827694a" # adams baseline
session_id <-  "2f036273-313c-4e82-b4ea-fbe719820e78" # ciarans baseline
session_id <-  "b9a8bcad-ba60-4f8f-a927-80cb470fad09" # Ians baseline
session_id <-  "d993cda1-6d13-45fa-859e-9641dc8b0412" # My Drive

data <- api_data %>%
	dplyr::filter(metric == "gsr")

plot(y=data$value, x=1:length(data$value), type="l", col=color.gradient(data$value), pch=18, cex=2)
points(smooth.spline(data$value, spar=0.3), type="l")
abline(h=60, lty=3, col="grey50")
abline(h=70, lty=3, col="grey50")
abline(h=80, lty=3, col="grey50")
abline(h=90, lty=3, col="grey50")
abline(h=100, lty=3, col="grey50")
############################ ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ############################

############################ ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ############################
############################ ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ############################

############################ ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ############################


require("readr")
require("dplyr")
require("ggplot2")
require("leaflet")
require("tidyr")
require("lubridate")
require("stringi")
require("RColorBrewer")
require("osmdata")
require("magrittr")
require("osmar")
require("geosphere")
require("mapview")

# features: HIGHWAY
# motorway
# trunk
# primary
# secondary
# tertiary
# residential
# unclassified

setwd("/home/dmoore/KTP/data/driver_studyP2/data")

driver01 <- read.delim("driver1_011217_CAVAN_AND_NIAZ.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
colnames(driver01)[1] <- "time"
# Latitude of Drive
driver01_lat <- driver01 %>%
					dplyr::select(time, location_latitude) %>%
					na.omit()
# Longitude of Drive
driver01_lon <- driver01 %>%
					dplyr::select(time, location_longitude) %>%
					na.omit()

# Grab Speed
speed <- driver01 %>%
			dplyr::select(location_speed) %>%
			na.omit()

mlat_driver <- mean(driver01_lat$location_latitude)
mlon_driver <- mean(driver01_lon$location_longitude)

lat_driver <- driver01_lat$location_latitude
lon_driver <- driver01_lon$location_longitude

mat <- matrix(c(driver01_lon$location_longitude, driver01_lat$location_latitude), ncol=2)
# Calculate Distance travelled between GPS coords
route.dist     <- calc_route_dist(mat)
dist.travelled <- sum(route.dist)

# Convert Speed to MPH
# time      <- driver01_lat$time
# time_secs <- time_convert(time, format="hours", start=TRUE)
# time_secs <- abs(time_secs[1:length(time_secs)-1] - time_secs[2:length(time_secs)])
# TO DO
# met_mile = 0.00062137119223733
# route.mile <- route.dist * met_mile
# m_per_h <- round(route.dist * time_secs, 3)
# mph     <- m_per_s * (25/11)
##


setwd("/home/dmoore/KTP/data/driver_studyP2")


left  = min(lon_driver); left = left -0.002
right = max(lon_driver) 

top = max(lat_driver) 
bot = min(lat_driver) 

################################

# To use pre-downloaded file
#src  <- osmsource_file(file = "bel_map.osm")
api  <- osmsource_api()
box  <- corner_bbox(left, bot, right, top)
bel  <- get_osm(box, api)

#################################
par(mfrow=c(4,3), ann=FALSE, xaxt="n", yaxt="n", mar=c(0.0, 0.0, 0.0, 0.0),oma = c(0.0, 0.0, 0.0, 0.0))

roads <- as.character(unique(bel$ways$tags$k))

# plot everything 
for(i in 1:length(roads)){
hw_unc    <- find(bel, way(tags(k == roads[i])))# & v == "service")))
hw_unc_d  <- find_down(bel, way(hw_unc))
hw_unc_s  <- subset(bel, ids = hw_unc_d)
if(i == 1)
	{
		plot_ways(hw_unc_s, col = "grey70", lwd = 0.1)
	} else {
		plot_ways(hw_unc_s, add = T, col = "grey70", lwd = 0.1)
	}
}

hw_res    <- find(bel, way(tags(k == "highway" & v == "residential")))
hw_res_d  <- find_down(bel, way(hw_res))
hw_res_s  <- subset(bel, ids = hw_res_d)
plot_ways(hw_res_s, add = T, col = "grey65", lwd = 0.5, )

hw_pri    <- find(bel, way(tags(k == "highway" & v == "primary")))
hw_pri_d  <- find_down(bel, way(hw_pri))
hw_pri_s  <- subset(bel, ids = hw_pri_d)
plot_ways(hw_pri_s, add = T, col = "#ffecb3", lwd = 2)

hw_sec    <- find(bel, way(tags(k == "highway" & v == "secondary")))
hw_sec_d  <- find_down(bel, way(hw_sec))
hw_sec_s  <- subset(bel, ids = hw_sec_d)
plot_ways(hw_sec_s, add = T, col = "#ffecb3", lwd = 2)

hw_ter    <- find(bel, way(tags(k == "highway" & v == "tertiary")))
hw_ter_d  <- find_down(bel, way(hw_ter))
hw_ter_s  <- subset(bel, ids = hw_ter_d)
plot_ways(hw_ter_s, add = T, col = "#ffecb3", lwd = 2)

hw_tru    <- find(bel, way(tags(k == "highway" & v == "trunk")))
hw_tru_d  <- find_down(bel, way(hw_tru))
hw_tru_s  <- subset(bel, ids = hw_tru_d)
plot_ways(hw_tru_s, add = T, col = "#ffcc66", lwd = 4)

hw_mot    <- find(bel, way(tags(k == "highway" & v == "motorway")))
hw_mot_d  <- find_down(bel, way(hw_mot))
hw_mot_s  <- subset(bel, ids = hw_mot_d)
plot_ways(hw_mot_s, add = T, col = "#ff5050", lwd = 5)

hw_tl    <- find(bel, way(tags(k == "highway" & v == "trunk_link")))
hw_tl_d  <- find_down(bel, way(hw_tl))
hw_tl_s  <- subset(bel, ids = hw_tl_d)
plot_ways(hw_tl_s, add = T, col = "#ff9999", lwd = 1)

hw_ml    <- find(bel, way(tags(k == "highway" & v == "motorway_link")))
hw_ml_d  <- find_down(bel, way(hw_ml))
hw_ml_s  <- subset(bel, ids = hw_ml_d)
plot_ways(hw_ml_s, add = T, col = "#ff9999", lwd = 1)

hw_rab    <- find(bel, way(tags(k == "jrabtion" & v == "roundabout")))
hw_rab_d  <- find_down(bel, way(hw_rab))
hw_rab_s  <- subset(bel, ids = hw_rab_d)
plot_ways(hw_rab_s, add = T, col = "grey40", lwd = 1)

traff_ids <- find(bel, node(tags(k %agrep% "traffic_signals")))
hw_trl_s  <- subset(bel, node_ids = traff_ids)
plot_nodes(hw_trl_s, add = T, col = "darkred", pch="*")

points(x = driver01_lon$location_longitude, 
	   y = driver01_lat$location_latitude, 
	   pch=".", 
	   col="#4d4d4d", 
	   cex=2,
	   type="p") 

### Calculate distance and speed between each lon/lat coords
DriverRoute <- matrix(c(driver01_lon$location_longitude, driver01_lat$location_latitude), ncol=2)
route_dist  <- calc_route_dist(DriverRoute)
distance_travelled <- sum(route_dist)

### Calculate nearest feature ###

mot <- matrix(c(hw_mot_s$nodes$attrs$lon, hw_mot_s$nodes$attrs$lat), ncol=2); colnames(mot) <- c("lon", "lat") 
res <- matrix(c(hw_res_s$nodes$attrs$lon, hw_res_s$nodes$attrs$lat), ncol=2); colnames(res) <- c("lon", "lat") 
tru <- matrix(c(hw_tru_s$nodes$attrs$lon, hw_tru_s$nodes$attrs$lat), ncol=2); colnames(tru) <- c("lon", "lat") 
pri <- matrix(c(hw_pri_s$nodes$attrs$lon, hw_pri_s$nodes$attrs$lat), ncol=2); colnames(pri) <- c("lon", "lat") 
sec <- matrix(c(hw_sec_s$nodes$attrs$lon, hw_sec_s$nodes$attrs$lat), ncol=2); colnames(sec) <- c("lon", "lat") 
ter <- matrix(c(hw_ter_s$nodes$attrs$lon, hw_ter_s$nodes$attrs$lat), ncol=2); colnames(ter) <- c("lon", "lat") 
unc <- matrix(c(hw_unc_s$nodes$attrs$lon, hw_unc_s$nodes$attrs$lat), ncol=2); colnames(unc) <- c("lon", "lat") 
tru_l <- matrix(c(hw_unc_s$nodes$attrs$lon, hw_unc_s$nodes$attrs$lat), ncol=2); colnames(unc) <- c("lon", "lat") 
mot_l <- matrix(c(hw_unc_s$nodes$attrs$lon, hw_unc_s$nodes$attrs$lat), ncol=2); colnames(unc) <- c("lon", "lat") 
rab   <- matrix(c(hw_unc_s$nodes$attrs$lon, hw_unc_s$nodes$attrs$lat), ncol=2); colnames(unc) <- c("lon", "lat") 





#### TERAINE ####
#landuse, leisure


# bg_ids  <- find(bel, way(tags(k == "building")))
# bg_ids  <- find_down(bel, way(bg_ids))
# bg      <- subset(bel, ids = bg_ids)
# bg_poly <- as_sp(bg, "polygons")
# plot(bg_poly, col="grey80", add=TRUE)


# bg_ids  <- find(bel, way(tags(k == "landuse")))
# bg_ids  <- find_down(bel, way(bg_ids))
# bg      <- subset(bel, ids = bg_ids)
# bg_poly <- as_sp(bg, "polygons")
# plot(bg_poly, col="grey90", add=TRUE)

# nat_ids <- find(bel, way(tags(v %in% c("commercial"))))
# nat_ids <- find_down(bel, way(nat_ids))
# nat <- subset(bel, ids = nat_ids)
# nat_poly <- as_sp(nat, "polygons")
# plot(nat_poly, col = "grey90", add=TRUE, border="#ffffff00")

# nat_ids <- find(bel, way(tags(k %in% c("leisure"))))
# nat_ids <- find_down(bel, way(nat_ids))
# nat <- subset(bel, ids = nat_ids)
# nat_poly <- as_sp(nat, "polygons")
# plot(nat_poly, col = "#ccffcc",add=TRUE,border="#66ff66")



setwd("/home/dmoore/KTP/data/driver_studyP2/data")

driver01 <- read.delim("driver1_011217_CAVAN_AND_NIAZ.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
colnames(driver01)[1] <- "time"

######## Plot Gps Trace ######## 

# Plot start point # 
points(x = driver01_lon$location_longitude[1], 
	   y = driver01_lat$location_latitude[1], 
	   pch="x", 
	   col="black", 
	   cex=2) 

points(x = driver01_lon$location_longitude, 
	   y = driver01_lat$location_latitude, 
	   pch=".", 
	   col="#4d4d4d", 
	   cex=2,
	   type="p") 

points(x = driver01_lon$location_longitude[dim(driver01_lon$location_longitude)[1]], 
	   y = driver01_lat$location_latitude[dim(driver01_lat$location_latitude)[1]], 
	   pch=20, 
	   col="red", 
	   cex=15) 


### Calculate distance and speed between each lon/lat coords
DriverRoute <- matrix(c(driver01_lon$location_longitude, driver01_lat$location_latitude), ncol=2)
route_dist  <- calc_route_dist(DriverRoute)
distance_travelled <- sum(route_dist)

### Calculate nearest feature ###


mot <- matrix(c(hw_mot_s$nodes$attrs$lon, hw_mot_s$nodes$attrs$lat), ncol=2); colnames(mot) <- c("lon", "lat") 
res <- matrix(c(hw_res_s$nodes$attrs$lon, hw_res_s$nodes$attrs$lat), ncol=2); colnames(res) <- c("lon", "lat") 
tru <- matrix(c(hw_tru_s$nodes$attrs$lon, hw_tru_s$nodes$attrs$lat), ncol=2); colnames(tru) <- c("lon", "lat") 
pri <- matrix(c(hw_pri_s$nodes$attrs$lon, hw_pri_s$nodes$attrs$lat), ncol=2); colnames(pri) <- c("lon", "lat") 
sec <- matrix(c(hw_sec_s$nodes$attrs$lon, hw_sec_s$nodes$attrs$lat), ncol=2); colnames(sec) <- c("lon", "lat") 
ter <- matrix(c(hw_ter_s$nodes$attrs$lon, hw_ter_s$nodes$attrs$lat), ncol=2); colnames(ter) <- c("lon", "lat") 
unc <- matrix(c(hw_unc_s$nodes$attrs$lon, hw_unc_s$nodes$attrs$lat), ncol=2); colnames(unc) <- c("lon", "lat") 


peak.alt <- subset(hw_res_s$nodes$tags,(k=='ele' ))









##############################################################################################
 ############################################################################################
##############################################################################################
 ############################################################################################
##############################################################################################
 ############################################################################################
##############################################################################################
 ############################################################################################
##############################################################################################
 ############################################################################################
##############################################################################################










bel$nodes$tags$k[traffic_signals]


crossing 
junction 
drive_through
church



hw_res_s
hw_mot_s
hw_tru_s
hw_pri_s
hw_sec_s
hw_ter_s
hw_unc_s






























marks <- c()



features <- matrix(ncol=length(marks))




##### simple function to decide road type #####
motorway_lat <- round(hw_res_s$nodes$attrs$lon,3)
test_lat <- round(driver01_lon$location_longitude, 3)
test_lat%in%motorway_lat
###############################################



































hw_unc    <- find(bel, way(tags(k == "highway" & v == "unclassified")))
hw_unc_d  <- find_down(bel, way(hw_unc))
hw_unc_s  <- subset(bel, ids = hw_unc_d)
plot_ways(hw_unc_s, add = T, col = "grey60", lwd = 1)


calc_dist(DriverRoute)#, GPS=GPS)

gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6378.137 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  d <- d * 1000
  return(d) # Distance in km
}
	
dist  <- vector(length=dim(matty)[1])
disf  <- vector(length=dim(matty)[1])
close <- vector(length=dim(GPS)[1])
for(i in 1:dim(GPS)[1]){
	long1 <- GPS[i,1]
	lat1  <- GPS[i,2]
	for(j in 1:dim(matty)[1]){
		long2 <- matty[j,1]
		lat2  <- matty[j,2]


		#dist[j] <- as.numeric(d)
		dist[j] <- distHaversine(c(long1, lat1), c(long2, lat2))
		disf[j] <- earth.dist(long1, lat1, long2, lat2)*1000

	}
	close[i] <- min(dist)
	print(close[i])
	print(min(disf))
}

distHaversine(c(long1, lat1), c(long2, lat2))




dlon = lon2 - lon1
dlat = lat2 - lat1
a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
c = 2 * atan2( sqrt(a), sqrt(1-a) )
d = R * c (where R is the radius of the Earth) 







distMeeus(c(long1, lat1), c(long2, lat2))
distRhumb(c(long1, lat1), c(long2, lat2))
distVincentyEllipsoid(c(long1, lat1), c(long2, lat2))
distVincentySphere(c(long1, lat1), c(long2, lat2))


long2 =-5.939593 
long1 =-5.939580
lat1 =54.58012
lat2 =54.58012




lat lon


gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}

































function measure(lat1, lon1, lat2, lon2){  // generally used geo measurement function
    var R = 6378.137; // Radius of earth in KM
    var dLat = lat2 * Math.PI / 180 - lat1 * Math.PI / 180;
    var dLon = lon2 * Math.PI / 180 - lon1 * Math.PI / 180;
    var a = Math.sin(dLat/2) * Math.sin(dLat/2) +
    Math.cos(lat1 * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) *
    Math.sin(dLon/2) * Math.sin(dLon/2);
    var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
    var d = R * c;
    return d * 1000; // meters
}



# relate time spent waiting to heart rate.

bel.box <- center_bbox(center_lon = 54.5775, center_lat = -5.9436, width =  2000, height = 2000)

# Download all osm data inside this area
api <- osmsource_api()
bel <- get_osm(bel.box, source = api)

# General plot
plot(bel)

# Find highways
ways <- find(rome, way(tags(k == "highway")))
ways <- find_down(rome, way(ways))
ways <- subset(rome, ids = ways)

# SpatialLinesDataFrame object
hw_lines <- as_sp(ways, "lines")  

# Plot
spplot(hw_lines, zcol = "uid")

# Interactive view
mapview::mapview(hw_lines) 

# Make a random points dataset (like GPS)
gpsPoints <- spsample(x = hw_lines, n = 100, type = "random")

# Plot points
plot(hw_lines, xlab = "Lon", ylab = "Lat")
plot(gpsPoints, add = TRUE, pch = 19, col = "red")
box()

# Distances between Higways and random points
distances <- dist2Line(p = gpsPoints, line = hw_lines)



function measure(lat1, lon1, lat2, lon2){  // generally used geo measurement function
    var R = 6378.137; // Radius of earth in KM
    var dLat = lat2 * Math.PI / 180 - lat1 * Math.PI / 180;
    var dLon = lon2 * Math.PI / 180 - lon1 * Math.PI / 180;
    var a = Math.sin(dLat/2) * Math.sin(dLat/2) +
    Math.cos(lat1 * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) *
    Math.sin(dLon/2) * Math.sin(dLon/2);
    var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
    var d = R * c;
    return d * 1000; // meters
}



54.58014924
-5.93957247



library(ggmap)
gc <- c(54.59197043,	"-6.06024784")
revgeocode(54.59197043,	:/)





# Grab box of location
bb = getbb('Northern Ireland')
q <- opq(bbox = bb) %>%
		add_osm_feature(key = 'highway', value = 'motorway') %>%
		add_osm_feature(key = 'highway', value = 'trunk') %>%
		add_osm_feature(key = 'highway', value = 'primary')	%>%
		add_osm_feature(key = 'highway', value = 'secondary') %>%
		add_osm_feature(key = 'highway', value = 'tertiary') %>%
		add_osm_feature(key = 'highway', value = 'residential') %>%
		add_osm_feature(key = 'name') %>%
    	osmdata_sf()

q2 <- add_osm_feature(q, key = 'bicycle', value = 'designated')

q1 <-  osmdata_sp(q)
sp::plot(q1$osm_lines)


identical(q, opq(bbox = 'Northern Ireland'))


driver <- dbDriver("PostgreSQL")
connection <- dbConnect(driver, 
				 dbname = "OSM",
                 host = "localhost", 
                 port = 5432,
                 user = "dmoore", 
                 password = "OpenStreetMAP99")
dbExistsTable(connection, "lines")

OpenStreetMAP99



##### plotting street types #####

bb = getbb('Belfast, U.K.')
q <- opq(bbox = bb)
q <- opq(bbox = 'Belfast') %>%
    add_osm_feature(key = 'highway', value = 'trunk') %>%
cway_sev <- osmdata_sp(q)
sp::plot(cway_sev$osm_lines, col="orange")

#################################



q1 <- opq('Belfast') %>%
    add_osm_feature(key = 'highway', value = 'trunk')
cway_sev <- osmdata_sp(q)


q2 <- opq('Belfast') %>%
    add_osm_feature(key = 'highway', value = 'motorway')
roads_sev <- osmdata_sp(q2)
sp::plot(cway_sev$osm_lines, col="orange")
sp::plot_ways(roads_sev$osm_lines, col="red")







src <- osmsource_file(file = "map_sensum")
bb <- center_bbox(54.57, -5.93, 1000, 1000)
bel <- get_osm(bb, src)

hw_res    <- find(bel, way(tags(k == "highway" & v == "residential")))
hw_res_d  <- find_down(bel, way(hw_res))
hw_res_s  <- subset(bel, ids = hw_res_d)
plot_ways(hw_res_s, col = "lightblue", lwd = 1)

hw_res    <- find(bel, way(tags(k == "highway" & v == "motorway")))
hw_res_d  <- find_down(bel, way(hw_res))
hw_res_s  <- subset(bel, ids = hw_res_d)
plot_ways(hw_res_s, add = T, col = "red", lwd = 3)

hw_res    <- find(bel, way(tags(k == "highway" & v == "trunk")))
hw_res_d  <- find_down(bel, way(hw_res))
hw_res_s  <- subset(bel, ids = hw_res_d)
plot_ways(hw_res_s, add = T, col = "orange", lwd = 2)

hw_res    <- find(bel, way(tags(k == "highway" & v == "primary")))
hw_res_d  <- find_down(bel, way(hw_res))
hw_res_s  <- subset(bel, ids = hw_res_d)
plot_ways(hw_res_s, add = T, col = "darkolivegreen1", lwd = 2)

hw_res    <- find(bel, way(tags(k == "highway" & v == "secondary")))
hw_res_d  <- find_down(bel, way(hw_res))
hw_res_s  <- subset(bel, ids = hw_res_d)
plot_ways(hw_res_s, add = T, col = "cadetblue1", lwd = 2)

hw_res    <- find(bel, way(tags(k == "highway" & v == "tertiary")))
hw_res_d  <- find_down(bel, way(hw_res))
hw_res_s  <- subset(bel, ids = hw_res_d)
plot_ways(hw_res_s, add = T, col = "beige", lwd = 2)

hw_res    <- find(bel, way(tags(k == "highway" & v == "unclassified")))
hw_res_d  <- find_down(bel, way(hw_res))
hw_res_s  <- subset(bel, ids = hw_res_d)
plot_ways(hw_res_s, add = T, col = "grey", lwd = 2)























		add_osm_feature(key = 'highway', value = 'primary')	%>%
		add_osm_feature(key = 'highway', value = 'secondary') %>%
		add_osm_feature(key = 'highway', value = 'tertiary') %>%
		add_osm_feature(key = 'highway', value = 'residential') %>%
hw_res    <- find(bel, way(tags(k == "highway" & v == "primary")))
hw_res_d  <- find_down(bel, way(hw_res))
hw_res_s  <- subset(bel, ids = hw_res_d)
plot_ways(bikePaths, add = T, col = "lightblue", lwd = 3)

hw_res    <- find(bel, way(tags(k == "highway" & v == "residential")))
hw_res_d  <- find_down(bel, way(hw_res))
hw_res_s  <- subset(bel, ids = hw_res_d)
plot_ways(bikePaths, add = T, col = "lightblue", lwd = 3)



ways <- find(bel, way(tags(k == "highway")))

bel$ways$tags$k == highway // v == residential



ways <- find_up(bel, way(ways))
ways <- subset(bel, ids = hw_res_up)



plot_ways(ways)
plot_ways(hways_data, add=T)
plot_ways(hways_data, add=T)
plot_ways(hways_data, add=T)

hw_lines <- as_sp(ways, "lines")



plot_ways(osmdata, col="gray")
plot_ways(hways_data, add=T)






library('osmar')
library('geosphere')

# Define the spatial extend of the OSM data we want to retrieve
belfast.box <- center_bbox(center_lon = 12.5450, center_lat = 41.8992, width =  2000, height = 2000)

# Download all osm data inside this area
api <- osmsource_api()
belfast <- get_osm(belfast.box, source = api)

# General plot
plot(belfast)

# Find highways
ways <- find(belfast, way(tags(k == "highway"), key=residential))
ways <- find_down(belfast, way(ways))
ways <- subset(belfast, ids = ways)

# SpatialLinesDataFrame object
hw_lines <- as_sp(ways, "lines")  

# Plot
spplot(hw_lines, zcol = "uid")

# Interactive view
mapview::mapview(hw_lines) 

# Make a random points dataset (like GPS)
gpsPoints <- spsample(x = hw_lines, n = 100, type = "random")

# Plot points
plot(hw_lines, xlab = "Lon", ylab = "Lat")
plot(gpsPoints, add = TRUE, pch = 19, col = "red")
box()

# Distances between Higways and random points
distances <- dist2Line(p = gpsPoints, line = hw_lines)





USER: dmoore
PASS: OpenStreetMAP99