library(RNetCDF)

x <- open.nc("covariates/noaa/NOAAGlobalTemp_v5.0.0_gridded_s188001_e202205_c20220608T133245.nc")


x <- readOGR("covariates/us contacts/pacunski/All_habmaps3.shp")
sf <- x %>% st_as_sf()
plot(x)
ggplot() + geom_sf(data=sf)
