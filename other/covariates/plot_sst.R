coast <- sf::st_read(dsn = "shapefiles", layer = "BC_coast_UTM9") %>% st_transform(crs = 4326)

# load nc
nc = nc_open(data_file)
r <- brick(data_file)
names <- r@data@names[468:501] %>% substring(2,11) %>% lubridate::ymd() %>% #%>% length()
  data.frame() %>% mutate(year=year(.), month=month.abb[month(.)], name = paste(month,year, sep=""))
names <- names$name
# filter to desired dates to reduce size for analysis
r@z$Date
r@z$Date[468:501]
r2 <- r[[468:501,drop=T]]
names(r2@layers) <- names
# crop to desired extent to reduce size for plotting
bb <- extent(c(233, 238, 48, 50)) #-127+360, -122+360
r3 <- crop(r2, bb)
plot(r3)

r4 <- r2@layers[[1]] %>% crop(bb) %>% projectRaster(crs = 4326)
r4@data@names
plot(r4)
st_crs(r4) 

r5 <- r4 %>% as("SpatialPixelsDataFrame") %>% 
 as.data.frame() %>% mutate(x=oce::angleRemap(x))

cols <- rev(brewer.pal(11,"RdBu"))

ggplot() +  
  geom_sf(data=coast,
  fill="grey60", color="grey50", linewidth=0.25) +
  geom_raster(data=r5, aes(x=x, y=y, fill=X2020.08.01), alpha=0.8)  + 
  scale_color_manual(values=cols)+
  coords

#---------------------------------------------
#---------------------------------------------
#    ANIMATE
#---------------------------------------------
#---------------------------------------------

# https://stackoverflow.com/questions/47698349/use-animate-with-series-of-levelplots-in-r-raster
library(raster)
library(rasterVis)
library(animation)
library(classInt)

# r <- raster(ncol=40, nrow=20)
# r[] <- rnorm(n=ncell(r))
# s <- stack(x=c(r, r*r, r*r*r, r*r*r*r))
# 
# classes <- classIntervals(values(r), n=5, style="fisher", precision = 3)
# brks <- classes$brks
# brks <- round(brks, 2)
# 
# saveGIF({
#   for(i in c(1:nlayers(s))){
#     l <- levelplot(s[[i]], colorkey=list(at=brks, labels=c(as.character(brks))), margin=FALSE)
#     plot(l)
#   }
# }, interval=0.2, movie.name="animation.gif")

cols <- rev(brewer.pal(11,"RdBu"))[2:9]
col_ramp <- colorRampPalette(cols)
cols <- col_ramp(20)

classes <- classIntervals(values(r3), n=10, style="fisher", precision = 3)
brks <- classes$brks
brks <- round(brks, 2)

r3@data@names[[i]] 
s <- r3
saveGIF({
  for(i in c(1:nlayers(s))){
    date <- r3@data@names[[i]] %>% substring(2,11) %>% lubridate::ymd() %>%
      data.frame() %>% mutate(year=year(.), month=month.abb[month(.)])
    l <- levelplot(s[[i]], margin=FALSE, colorkey=list(at=brks, labels=c(as.character(brks))),
                   col.regions=cols, main=paste("Mean Sea Surface Temperature", date$month, date$year, "(C)",sep=" "))
    plot(l)
  }
}, interval=2, movie.name="sst.gif")
#--------------------------------------

# note the distance b/w lines of lat is aprox 111 km, so b/w 0.25 degrees lat is ~28 km
# distance b/w lines of lon at around 48-50 degrees lat is approx 71-74 km, so @0.25 degrees lon ~ 18 km
