# https://hansenjohnson.org/post/sst-in-r/
# find ncdf in r help here : https://pjbartlein.github.io/REarthSysSci/netCDF.html

# CITATION:
# Reynolds, R.W., N.A. Rayner, T.M. Smith, D.C. Stokes, and W. Wang, 2002: 
#   An improved in situ and satellite SST analysis for climate. J. Climate, 15, 1609-1625.

# Please note: If you acquire NOAA Optimum Interpolation (OI) SST V2 data products from PSL,
# we ask that you acknowledge us in your use of the data. This may be done by including text such as 
# NOAA Optimum Interpolation (OI) SST V2 data provided by the NOAA PSL, Boulder, Colorado, USA, from
# their website at https://psl.noaa.gov in any documents or publications using these data. We would 
# also appreciate receiving a copy of the relevant publications. 

library(oce)
library(ncdf4)
# library(ocedata)
data("coastlineWorld")

# https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/
data_file <- "covariates/noaa/sst.mon.mean.nc"
# Monthly values from 1981/09 to 2023/05
# Spatial Coverage
# 0.25 degree latitude x 0.25 degree longitude global grid (1440x720)
# @ 50 N lat, 0.25 degrees = 71 km/4 ~18 km
# @ 49 N lat, 0.25 degrees = 73 km/4 ~18
# lines of lat are sep'd by ~ 111 km
# so 0.25 ~ 28 km
# 89.875S - 89.875N,0.125E to 359.875E

# open netcdf file and extract variables
nc = nc_open(data_file)

# view netcf metadata
print(nc)
#--------------------------------------
#--------------------------------------
# extract data
#--------------------------------------
#--------------------------------------
# names(nc$var)
# nc$ndims
# names(nc$dim)
# nc$var$sst$dimids
# nc$var$sst$size
lat = ncvar_get(nc, "lat")
lon = ncvar_get(nc, "lon")
time = ncvar_get(nc, "time")
sst = ncvar_get(nc, "sst") # lon 1440, lat 720, time 501

fillvalue <- ncatt_get(nc,"sst","_FillValue")
tunits <- ncatt_get(nc,"time","units")

dim(sst)
dimnames(sst)
nlon <- dim(lon)
nlat <- dim(lat)
nt <- dim(time)
#-------------------------------------------
# trying to get data in workable format
# start with nc array 'sst' with 3 dimensions, lat, lon and time
# filter on date, convert to vector, then matrix, then df, filter on lon/lat, then expand to long df
# can plot as a raster, filter for specific year and month first
#-------------------------------------------
grid <- expand.grid(lon=lon, lat=lat)

library(lattice)
levelplot(sst[,,1] ~ lon * lat, data=grid, 
                   #at=cutpts,
                     cuts=11, pretty=T, 
                   col.regions=(rev(brewer.pal(10,"RdBu"))), main="Mean Temperature (C) example plot")
#-------------------------------------------
# Filter time dimension in nc object
#-------------------------------------------
# dimnames(sst) <- list(c("lon","lat","time"))
# lat_cemore <- lat %>% dplyr::filter(lat %in% c(48:49.5))
# convert timestamp (first convert to seconds as POSIXct reads the time elapsed in seconds, not days)

# try subsetting sst array by dates

time_x = as.POSIXct(time*24*60*60, origin = '1800-01-01 00:00:00', tz = 'UTC')
time_df <- data.frame(time, time_x) # for relating the nc time format to date format for filtering
time_cemore = time[which(time >= 80566)] #(80566 = 2020-08-01)
sst2 <- sst[,,(468:501),drop=T]
dim(sst2) #  1440  720   34
nt <- dim(sst2)[3]

#----------------------------------
#----------------------------------
# Convert the whole array to a data frame, and calculate MTWA, MTCO and the annual mean
# from here : https://pjbartlein.github.io/REarthSysSci/netCDF.html
#----------------------------------
#----------------------------------

sst2[sst2==fillvalue$value] <- NA

# Reshape the whole array
tmp_vec_long <- as.vector(sst2)
length(tmp_vec_long)
tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt) # this was creating a 1036800 * 501 mat before because I hadn't updated 'nt'
dim(tmp_mat)
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df02 <- data.frame(cbind(lonlat,tmp_mat)) # this takes a LOT of memory and must rm() other objects and/or shut down other programs to compute. though much better now that I was able to subset the array by time.
# saveRDS(tmp_df02, "covariates/sst_tmp_df02.rds")
# tmp_df02 <- readRDS("covariates/sst_tmp_df02.rds")

# create date names for the new df
subtime <- time[468:501]
time_df = as.POSIXct(subtime*24*60*60, origin = '1800-01-01 00:00:00') %>% 
  lubridate::date() %>% 
  data.frame() %>% mutate(year=year(.), month_abb=month.abb[month(.)])
time_df$name <- paste(time_df$month_abb, time_df$year, sep="")
names(tmp_df02) <- c("lon","lat", time_df$name)

# convert and filter lon
tmp_df02 %<>% mutate(lon=oce::angleRemap(lon)) %>% filter(between(lon, -127, -122), between(lat, 47, 51)) 
saveRDS(df, "covariates/sst_dataframe.rds")

# create long data
ldf <- df %>% tidyr::pivot_longer(cols=!c(lon,lat), names_to="monthyear",values_to="sst") %>% 
  mutate(year=substring(monthyear,4,7),month_abb=substring(monthyear,1,3))
saveRDS(ldf, "covariates/sst_dataframe_long.rds")

# summarise_by_seasons TODO?? Is this meaningful?

#--------------------------------------------------
#--------------------------------------------------
#              PLOT
#--------------------------------------------------
#--------------------------------------------------
cols <- rev(brewer.pal(11,"RdBu"))[2:9]
col_ramp <- colorRampPalette(cols)
p1 <- df %>% filter
ggplot() +  
  geom_sf(data=coast, fill="grey60", color="grey50", linewidth=0.25) +
  geom_raster(data=r5, aes(x=x, y=y, fill=X2020.08.01), alpha=0.8)  + 
  scale_fill_gradientn(colours = col_ramp(20)) +
  coords


ggplot() +
  geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  labs(fill = "Depth (m)") +
  scale_fill_gradientn(colours = col_ramp(20)) +
  theme(legend.position = "bottom",
        axis.text=element_text(size=12),
        legend.text=element_text(size=12)
  ) +
  guides(fill = guide_colorbar(title.position = "left"))


  scale_fill_manual(rev(brewer.pal(10,"RdBu")))
# 3.4.2 Get the annual mean
# Get the annual mean, mtwa and mtco (mean temperatures of the warmest and coldest montth) values and add them to the second data frame.


#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# TO CREATE AN ARRAY FROM THE DF (ie. if you wish to create a nc file)
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# 4.1.1 Initial set up – create dimension variables
# Use df object which has already been filtered on lat and lon
lon2 <- unique(df$lon)
lat2 <- unique(df$lat)
time2 <- time_cemore
tunits2 <- tunits
nlon2 <- length(lon2)
nlat2 <- length(lat2)
nt2 <- nt

# convert tmp_df02 back into an array
tmp_mat2 <- as.matrix(df[3:(3+nt2-1)])
dim(tmp_mat2)

# memory.limit() 
# memory.limit(size=100000)
tmp_array2 <- array(tmp_mat2, dim=c(nlon2,nlat2,nt2))  # this takes a LOT of memory. I had to actually increase memory to get this to run.
# saveRDS(tmp_array2, "covariates/sst_tmp_array2.rds")
# tmp_array2 <- readRDS("covariates/sst_tmp_array2.rds")
dim(tmp_array2)
grid2 <- expand.grid(lon=lon2, lat=lat2)

levelplot(tmp_array2[,,3] ~ lon2 * lat2, data=grid2, 
          #at=cutpts,
          cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))), main="Mean Temperature example (C)")


# SEE WEBSITE FOR THE REST OF THIS. I LIKELY DON'T NEED IT.

#-------------------------------------------
# Filter lon dimension in nc object
#-------------------------------------------
# 
# lon_df <- data.frame(lon)
# lon_x <- lon_df %>% mutate(lon_x = case_when(
#   lon %in% c(0:180) ~ lon,
#   !lon %in% c(0:180) ~ lon-360
# ))
# 
# lon_cemore = lon[which(lon <237)] 
# lon_cemore = lon[which(lon_cemore >234)] 
# 
# 
# lat_cemore = lat[which(lat <237)] 
# lat_cemore = lat[which(lat_cemore >234)] 


#--------------------------------------------------------------
#--------------------------------------------------------------
#     RESTART
#--------------------------------------------------------------
#--------------------------------------------------------------
# b2 <- brick(sst2) # this doesn't work - loses data dimension for some reason...?

# So have to start with brick and subset within...
b <- brick(data_file, varname = "sst")
st_crs(b)

# TRY TO SUBSET THE LAT AND LON IN EACH RASTER

b2 <- b[[468:501]]
ex <- extent(b2,-126,-123, 48,50)
b3 <- crop(b2, ex)

plot(coastlineWorld)
plot(b3[[1]], add=T)

plot(b3)
plot(coast$geometry)
plot(b3[[1]], add=T)     
# close netcdf
nc_close(nc)


#-------------------------------------
coast <- sf::st_read(dsn = "shapefiles", layer = "BC_coast_UTM9") %>% st_transform(crs = 4326)
coords <- ggplot2::coord_sf(xlim = c(-126, -123), ylim = c(48.15, 50.1), crs = sf::st_crs(4326)) # SoG
# area <- st_read("shapefiles/Full_study_area_UTM9N.shp") %>% st_transform(crs=4326)

df <- data.frame(lat,lon,sst[1])
sst <- st_as_sf(df, coords=c(Y=lon, X=lat), crs=4326)

ggplot() +
  geom_sf(data= coast) +
  geom_sf(data=)
  coords
  
#-------------------
# working with arrays
#-------------------
  vector1 <- c(5, 9, 3)
  vector2 <- c(10, 11, 12, 13, 14, 15)
  
  # Take these vectors as input to the array.
  array1 <- array(c(vector1, vector2), dim = c(3, 3, 2))
  
  print(array1)
  
  vector3 <- c(9, 1, 0)
  vector4 <- c(6, 0, 11, 3, 14, 1, 2, 6, 9)
  array2 <- array(c(vector1, vector2), dim = c(3, 3, 2))
  
  array2
  
  # create matrices from these arrays.
  matrix1 <- array1[,,2]
  matrix2 <- array2[,,2]
  
  # Add the matrices.
  result <- matrix1 + matrix2
  print(result)
  
  
  # Create two vectors of different lengths.
  # vector3 <- c(9, 1, 0)
  # vector4 <- c(6, 0, 11, 3, 14, 1, 2, 6, 9)
  # array2 <- array(c(vector3, vector4), dim = c(3, 3, 2))
  # 
  # print(array2)
  # 
  # # create matrices from these arrays.
  # matrix1 <- array1[,,2]
  # matrix2 <- array2[,,2]
  # 
  # # Add the matrices.
  # result <- matrix1 + matrix2
  # print(result)
  
  ax <- array(1:24, c(2,3,4))
  ay <- array(1:12, c(1,3,4))
  dim(ax)
  #[1] 2 3 4
  dim(ay)
  #[1] 1 3 4
  dim(ax[,1:2,])
  #[1] 2 2 4
  dim(ay[,1:2,])
  #[1] 2 4

#---------------------------------------------
# subsetting nc data
  #---------------------------------------------
  install.packages("ncdf.tools")
  library(ncdf.tools)
  transNcdfSubset <- function(file.input, dim.values = list(latitude = c(), 
                                                longitude = c(), time = c()), values.type = c("range", "indices", 
                                                                                              "values")[2], file.output = sub("[.]nc", "_subs.nc", file.input), 
                  var.name = readNcdfVarName(file.input))
  sstc <- transNcdfSubset(data_file, dim.values = list(latitude = ))
  
  #---------------------------------------
  # another approach
  #---------------------------------------
  library(ncdf)
  ncfile <- nc<-open.ncdf("test.nc")
  varcomid <- get.var.ncdf(ncfile,varid = "COMID")
  vartime <- get.var.ncdf(ncfile,varid = "Time")
  ndims <- ncfile$var[['Qout']]$ndims 
  varsize <- ncfile$var[['Qout']]$varsize
  comid <- which(varcomid == 1439445)
  
  start <- c(comid, 4)
  count <- c(1, varsize[2] - start[2])
  
  dat <- get.var.ncdf(nc = ncfile,varid = "Qout", start = start, count = count)