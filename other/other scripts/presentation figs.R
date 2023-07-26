# Figures for presentations
devtools::load_all(cemore)

# year <- 2023
# month <- "02"
all_effort <- readRDS(paste0("output_cemore/all_effort/all_effort_to_", year,"_",month, ".rds"))
all_effort_lines <- readRDS(paste0("output_cemore/all_lines/all_effort_lines_to_", year,"_",month, ".rds"))
all_ap_sf <- readRDS(paste0("output_cemore/all_sgt/all_effort_sgt_to_", year,"_",month, ".rds"))

# Seasonal Effort Map
p <- suppressMessages(plot_survey(
  Save = T,
  file_name = paste0("output_maps_cemore/seasonal_effort_to",year,"_",month,".png"),
  data.source="cemore",
  border = T,
  single_survey = F,
  incidentals <-  F,
  hydrophone <- F,
  plot_effort = T, # T/F
  plot_sgt = F,
  effort_data = all_effort_lines,
  season=  T,
  
  N=T,
  km=T,
  depth=F))
ggsave(paste0("output_maps_cemore/seasonal_effort_to",year,"_",month,".png"),p)
# suppressMessages(ggsave("knitr-figs-pdf/seasonal_track.png",p))
# plot_crop("knitr-figs-pdf/seasonal_track.png", quiet=T)

plot_survey(
  # Save=T,
  # file_name = "output_maps_cemore/seasonYear_hw_to_2022_10.png",
  # data=ap_sf2022,
  border=F,
  single_survey=F,
  plot_sgt=T,
  plot_effort = T,
  # effort_data = effort_lines2022,
  # show_transit=F,
  # N=T,
  # km=T,
  # depth=T,
  legend = F,
  species="humpback",
  season=F,
  by_seasonYear = T)

ap_sf2022 <- all_ap_sf %>% filter(year<2023)
effort_lines2022 <- all_effort_lines %>% filter(year<2023)

plot_survey(
  Save=F,
  # file_name = "output_maps_cemore/seasonYear_hw.png",
  data=ap_sf2022,
  border=F,
  single_survey=F,
  plot_sgt=T,
  plot_effort = T,
  effort_data = effort_lines2022,
  # show_transit=F,
  # N=T,
  # km=T,
  # depth=T,
  legend = F,
  species="humpback",
  season=F,
  by_seasonYear = T)
ggsave("output_maps_cemore/sightings_seasonYear_hw_2023apr_black.png")

plot_survey(
  Save=F,
  # file_name = "output_maps_cemore/seasonYear_hw.png",
  data=ap_sf2022,
  border=F,
  single_survey=F,
  effort_data = effort_lines2022,
  # show_transit=F,
  # N=T,
  # km=T,
  # depth=T,
  legend = F,
  species="harbour",
  seasonYear = T)
ggsave("output_maps_cemore/sightings_seasonYear_hp_2023apr_black.png")

x <- all_effort_lines %>% data.frame() %>% 
  group_by(year, month_abb) %>% 
  summarise(transects = length(unique(transect_no)),
  distance_km = round(sum(length_km),digits=0))
write.csv(x, "output_cemore/survey_sum_2023feb.csv", row.names = F)


# plot one full set of transects
shp <- read_sf("C:\\Users\\KeppelE\\Documents\\CeMoRe\\Analysis\\cemore_analysis\\survey_data\\raw_data\\2023\\2023-02\\transects\\t18km_iteration_26.shp")
)
p <- plot_survey(
  Save=F,
  # file_name = "output_maps_cemore/seasonYear_hw.png",
  data=ap_sf2022,
  border=F,
  single_survey=F,
  effort_data = effort_lines2022,
  plot_sgt=F,
  plot_effort=F,
  # show_transit=F,
  # N=T,
  # km=T,
  # depth=T,
  legend = F)
  # species="harbour",
  # seasonYear = T)
coord <- ggplot2::coord_sf(xlim = c(-125.5, -122.9), ylim = c(48.1, 49.44), crs = sf::st_crs(4326))

p+geom_sf(data=shp) + coord

#------------------------------------

all_ap_sf %>% group_by(Species) %>% filter(date<"2022-11-01") %>% 
  summarise(sightings=n(), individuals=sum(Group_Size))
all_ap_sf %>% filter(Species %like% "killer") %>% data.frame() %>% 
  dplyr::select(year, month_abb, Species, Group_Size)

#--------------------------------------
# for tech report seasonYear sgts by species
plot_survey(
  Save=F,
  high_res=T,
  # file_name = "output_maps_cemore/seasonYear_hw.png",
  # data=ap_sf2022,
  border=F,
  single_survey=F,
  plot_sgt=T,
  plot_effort = T,
  # effort_data = effort_lines2022,
  # show_transit=F,
  # N=T,
  # km=T,
  # depth=T,
  legend = T,
  species="humpback",
  season=F,
  by_seasonYear = T,
  leg.pos="right")
ggsave("output_maps_cemore/sightings_seasonYear_hw_2022oct_hi_res.png")

plot_survey(
  Save=F,
  high_res=T,
  # file_name = "output_maps_cemore/seasonYear_hw.png",
  # data=ap_sf2022,
  border=F,
  single_survey=F,
  plot_sgt=T,
  plot_effort = T,
  # effort_data = effort_lines2022,
  # show_transit=F,
  # N=T,
  # km=T,
  # depth=T,
  legend = T,
  species="harbour",
  season=F,
  by_seasonYear = T,
  leg.pos="right")
ggsave("output_maps_cemore/sightings_seasonYear_hp_2022oct_hi_res.png")

plot_survey(
  Save=F,
  high_res=T,
  # file_name = "output_maps_cemore/seasonYear_hw.png",
  # data=ap_sf2022,
  border=F,
  single_survey=F,
  plot_sgt=T,
  plot_effort = T,
  # effort_data = effort_lines2022,
  # show_transit=F,
  # N=T,
  # km=T,
  # depth=T,
  legend = T,
  species="Dall",
  season=F,
  by_seasonYear = T,
  leg.pos="right")
ggsave("output_maps_cemore/sightings_seasonYear_dp_2022oct_hi_res.png")
