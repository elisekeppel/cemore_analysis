# PRISMM shapefiles

grid <- readOGR("shapefiles/PRISMM/PRISMM_PredictionGrid_5km_SouthShelf_WithCovariates.shp")
plot(grid)
offshore <- readOGR("shapefiles/PRISMM/PRISMM_PredictionGrid_5km_Offshore_WithCovariatesEdited.shp")
plot(offshore, add=T, fill = "blue")

area <- readOGR("shapefiles/Full_study_area_UTM9N.shp") %>% st_as_sf() %>% sf_transform_xy(target_crs=4326)
ggplot() +geom_sf(data =area) +
  geom_sf(data=area)
plot(coast)
plot(area, add=T)
plot(area)
