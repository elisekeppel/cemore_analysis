#### PRH Location Processing ####
# Author: James Fahlbusch
# Date: 2/16/2019
# Goldbogen Lab, Stanford University
#
# Description: This script imports location data from Wildlife Computers TDR10 (DAP version 3.0)
# or Sirtrack Fastloc Tags, uses a speed/distance/angle filter to 
# remove bad position data. 
#
# Select the .pos for Sirtrack tags or the _locations file for Wildlife Computers Tags
#
# Output: CSV Lat/Lon and UTC timestamp
# Set the WD to the path of source file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
rm(current_path)
#### Load Packages ####
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
# Load Packages
pkgTest("rstudioapi")
pkgTest("tidyverse")
pkgTest("ggplot2")
pkgTest("lubridate")
pkgTest("argosfilter")
pkgTest("ggmap")
pkgTest("maps")
pkgTest("mapdata")
pkgTest("leaflet")
pkgTest("htmlwidgets")
pkgTest("rnaturalearth")
pkgTest("marmap")
pkgTest("metR")
# pkgTest("metR") # note: getNOAA.bathy was not working, but an alternate version on github is used here
# source('../Global Functions/getNOAAbathy.r') not required because function is in marmap::getNOAAbathy.r

#### Variables ####
# Speed threshhold in m/s
vmaxWhale <- 4
# Distance thresholds for sharp turn angles
# (15 degrees and 25 degrees)a
distLimit = c(1000, 2000) # dist in m

##Filter By Deployment time
# if using GMT...
# Specify the start and end time of deployment (NOTE: will be specific to each deployment, use GMT deployment time)
#startTime <- as.POSIXct(strptime("2022-10-18 15:55:00",format="%Y-%m-%d %H:%M:%S"),tz="GMT")
# Always use logger retrieval time (regardless if retrieved before battery died)
#endTime <- as.POSIXct(strptime("2017-07-10 17:44:00",format="%Y-%m-%d %H:%M:%S"),tz="GMT")

# if using local time from tag guide
tzOffset <- "Etc/GMT+7"
startTime <- as.POSIXct(strptime("10/18/22 15:55:00",format="%m/%d/%y %H:%M:%S"),tz=tzOffset)
attr(startTime, "tzone") <- "GMT" # change the timezone 
endTime <- as.POSIXct(strptime("10/18/22 18:47:00",format="%m/%d/%y %H:%M:%S"),tz=tzOffset)
attr(endTime, "tzone") <- "GMT" # change the timezone 

#### Import ####
#  select the location file
# for Sirtrack this will be the pos file
# for TDR10 this is the locations.csv file
filename <- file.choose()
setwd(dirname(filename)) # set the wd so files are saved with the deployment data

#Extract tag type and deployment id
depid <- basename(filename)
depid <- unlist(strsplit(depid,"[.]"))
t_type <- depid[2] # Sirtrack is pos, TDR10 is csv
depid <- depid[[1]][1]
depid
ifelse(t_type == "pos", "Sirtrack Selected", "TDR10 Selected")

# different import and processing by tag type
if(t_type == "pos"){
  #  pos file import
  data <- read.table(filename, header = FALSE, sep = ",", quote = "\"",
                     dec = ".", comment.char = "", fill = TRUE, skip = 5)
  #str(data)
  # select only columns with datetime, lat, long
  data <- data[,c("V1","V2","V3","V4","V5","V6","V9","V10")]
  # change column names to more practical shorter names
  colnames(data)[1:8] <- c("day","month","year", "hour","minute","seconds","Lat","Long")
  # remove empty locations
  data <- data %>% filter(data$Lat != 0 | data$Long != 0)
  # make a UTC Datetime column
  data$dt <- ymd_hms(paste(data$year,data$month,data$day,data$hour,data$minute,data$seconds,sep="-"))
  attr(data$dt, "tzone") <- "GMT" # Set TZ to GMT
  attr(data$dt, "tzone") # check that dt is in GMT time
  #str(data)

  #Simple plot
  plot(data$Long,data$Lat)
  # remove unused columns
  data <-data[,c("dt","Lat","Long")]  
  # change column names to more practical shorter names
  colnames(data)[1:3] <- c("dt","lat","long")
  
}else {
  # data <- read_csv(filename,
  #                  col_types = cols(Date = col_datetime(format = "%H:%M:%OS %d-%m-%Y"),
  #                                   Latitude = col_double(),
  #                                   Longitude = col_double()))
  data <- read.csv(filename, sep=",", header=TRUE)
  
  # select only columns with datetime, lat, long, hit type, and comment
  data <- data[,c("Date","Type","Latitude","Longitude", "Comment")]
  # change column names to more practical shorter names
  colnames(data)[1:5] <- c("dt","type","lat","long","comment")
  # remove any positions labeled 'user'
  data <- data %>% filter(data$type != "User") 
  # remove any clearly bad positions (identified by Wildlife Computers DAP)
  if(length(is.na(data$comment)) != length(data$comment) | dim(data[!str_detect(data$comment, "> 4 deviations from mean"),])[1] != length(data$comment)  ){
    data <- data[!str_detect(data$comment, "> 4 deviations from mean"),]
  }
  #class(data$dt)
  data$dtOrig <- data$dt
  # deal with the reversed datetimes from wildlife computers
  dt <- str_split(as.character(data$dtOrig), " ")
  
  data$date <- substr(as.character(data$dtOrig), 17, 27)
  
  # Dates were in the format 27-Jun-2017...
  # Dates <- format(strptime(sapply(dt, "[", 1), format = "%d-%b-%Y"), "%m/%d/%Y")
  Dates <- format(strptime(sapply(dt, "[", 2), format = "%d-%m-%Y"), "%m/%d/%Y")
  Dates <- strptime(sapply(dt, "[", 2), format = "%d-%m-%YYYY")
  
  
  # Times have strange levels of precision that we drop here (could round)
  Times <- sapply(dt, "[", 2)#, format = "%H:%M:%OS"), "%m/%d/%Y")
  
  data$dt <- as.POSIXct(strptime(paste(Dates, Times, sep=" "),format="%m/%d/%Y %H:%M:%OS"),options(digits.secs = 2) ,tz="GMT")
  # data$dt <- as.POSIXct(strptime(data$dtOrig,format="%m/%d/%Y %H:%M:%OS"),options(digits.secs = 2) ,tz="GMT")
  attr(data$dt, "tzone") # check that dt is in GMT time 

  # make a UTC Datetime column
  #attr(data$dt, "tzone") <- "GMT" # Set TZ to GMT
  rm(Times, Dates, dt)
  #str(data)
  as.character(data$dt[1])
  #Simple plot
  plot(data$long,data$lat) 
  # remove unused columns
  data <-data[,c("dt","lat","long")] 
  
}

plot(data$dt)
#### Filter Bad Locations ####
# Run subset() function to extract data for the selected timerange
if(startTime > min(data$dt) & startTime < max(data$dt)){ # make sure startTime is in the range of values
  data <- subset(data, data$dt >= startTime)
}
if(endTime < max(data$dt) & endTime > min(data$dt)){# make sure endTime is in the range of values
  data <- subset(data, data$dt <= endTime)
}
plot(data$dt)
# str(data)
plot(data$long,data$lat) 
# all locations are GPS quality
data$lc <- 3
# filter data using sdafilter
cfilter <-sdafilter(data$lat,data$long,data$dt, data$lc, vmax = vmaxWhale, ang = c(15, 25), distlim = distLimit)
# filter by speed only
mfilter<- vmask(data$lat, data$long, data$dt, vmax = vmaxWhale)

# plot unfiltered data
plot(data$long,data$lat, col="grey",type="l",
     ylim=c(min(data$lat),max(data$lat)), 
     xlim=c(min(data$long),max(data$long)),
     xlab="Longitude",ylab="Latitude")
lines(data$long[which(mfilter=="not")],
      data$lat[which(mfilter=="not")],col="red")
lines(data$long[which(cfilter=="not")],
      data$lat[which(cfilter=="not")],col="blue")


# check number of locations (by location class) removed by each filter
print("Speed only: ", quote = FALSE)
print(summary(as.factor(mfilter)))
print("SDA Filter: ", quote = FALSE)
print(summary(as.factor(cfilter)))
        
#### Export CSV ####
## Export 3 csv files:
# 1) Original Data, formatted  with results from Speed and SDA
# 2) Speed Filtered Data
# 3) SDA filtered Data

# Column names for export
colnames(data)[1:3] <- c("DateTimeUTC","Lat","Long")

datafull <- cbind(data[1:3],mfilter,cfilter)
colnames(datafull)[4:5] <- c(paste0("SpeedFilter_max", vmaxWhale),
                         paste0("SDAFilter_max", vmaxWhale,"_ad1_",distLimit[1] ,"M_ad2_",distLimit[2], "M"))
# Export the full data
write.csv(datafull, file=paste(depid,"-FullFilter", ".csv",sep=""))
# Export the speed data
dataSpeed <- data[which(mfilter != "removed"),] 
write.csv(dataSpeed, file=paste(depid,"-SpeedFilter", ".csv",sep=""))
# Export the sda data
dataSDA <- data[which(cfilter != "removed"),] 
write.csv(dataSDA, file=paste(depid,"-SDAFilter", ".csv",sep=""))

#### Map Filtered Data ####
world <- ne_countries(scale = "large", returnclass = "sf")
# Calculate Distance to 200m Isobath
b = getNOAA.bathy(lon1 = min(dataSDA$Long-3,na.rm=TRUE), lon2 = max(dataSDA$Long+3,na.rm=TRUE), lat1 = max(dataSDA$Lat+3,na.rm=TRUE), lat2 = min(dataSDA$Lat-3,na.rm=TRUE),resolution = 1, keep=FALSE)
# convert bathymetry to data frame
bf = fortify.bathy(b)

# Adjust this for short deployments in constrained area:
degBuffer <- .25 # Number degree buffer around plot

ggplot(data = world) +
  geom_sf() +
  geom_sf() +
  coord_sf(xlim = c(min(dataSDA$Long,na.rm = TRUE)-degBuffer, max(dataSDA$Long,na.rm = TRUE)+degBuffer), 
           ylim = c(min(dataSDA$Lat,na.rm = TRUE)-degBuffer, max(dataSDA$Lat,na.rm = TRUE)+degBuffer), expand = FALSE) +
  # add 200m contour
  geom_contour(data = bf, 
               aes(x=x, y=y, z=z),
               breaks=c(-100,-200,-300,-400),
               breaks=c(-200),
               size=c(0.4),
               colour="darkgrey", show.legend = FALSE) +
  #geom_text_contour(data = bf, aes(x=x, y=y,z = z),breaks=c(-100,-200,-300,-400), 
  geom_text_contour(data = bf, aes(x=x, y=y,z = z),breaks=c(-200), 
                    show.legend = FALSE, size = 2.2, alpha = .6, nudge_y = -.002) +
  geom_point(data=dataSDA,aes(x=as.numeric(as.character(Long)),
                              y=as.numeric(as.character(Lat))),
             alpha = 0.5, color= 'blue',size = 1) +
  geom_path(data=dataSDA,aes(x=as.numeric(as.character(Long)),
                             y=as.numeric(as.character(Lat))),
            alpha = 0.5, color= 'blue') +
  scale_alpha(guide = 'none') + 
  # annotation_scale(location = "bl", width_hint = 0.5) + 
  ggtitle(paste0(depid," SDA Filter")) +
  xlab("Longitude") + 
  ylab("Latitude") + 
  theme(axis.title = element_text(family="Times",face="bold", size=20),
        axis.text = element_text(family="Times", face="bold", size=18),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = NULL)
 

ggsave(sprintf("%s_SDA_Filter_Map.png",depid), plot = last_plot(), device = "png",
       scale = 2, width = 7, height = 5, units = "in", dpi = 600, limitsize = F)


#### Create Leaflet Map #### 

# initiate the leaflet instance and store it to a variable
m = leaflet() %>%
  # Base groups
  addProviderTiles(providers$Esri.OceanBasemap, group = "Oceans (default)") %>%  
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addTiles(group = "OSM") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>% 
  addPolylines(
    lng = dataSDA$Long, 
    lat = dataSDA$Lat,
    weight = 1.5,
    group = "lines"
  ) %>% 
  addCircleMarkers( 
    lng = dataSDA$Long,  
    lat = dataSDA$Lat,
    popup = dataSDA$DateTimeUTC, 
    radius = 2, 
    color = "black",
    stroke = FALSE, 
    fillOpacity = 0.75,
    #clusterOptions = markerClusterOptions(),
    group = depid
  )  %>%
  # Layers control
  addLayersControl(
    baseGroups = c("Oceans (default)", "Toner", "OSM", "Toner Lite"),
    overlayGroups = c(depid,"lines"),
    options = layersControlOptions(collapsed = F),
    position = "topright"
  ) %>% 
  addMiniMap(
    tiles = providers$Esri.WorldStreetMap,
    toggleDisplay = TRUE) 
# we can "run"/compile the map, by running the printing it
m
# Save it!
saveWidget(m, file=paste0(depid,"-Map.html"))

