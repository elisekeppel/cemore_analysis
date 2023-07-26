# Data request for data from March 2022 for Kitasoo, Central Coast
# Requested through CM Nov 8, 2022

source("R/0 Setup.R")
devtools::load_all(cemore)

month <- '03'
month_abb <- month.abb[as.numeric(month)]
year <- 2022
survey_title <- paste(first_up(month_abb), year)
surveyid = paste0("cemore_", year, tolower(month_abb))

single <- T
# include directory for non-standard cemore survey data. Data may not have been processed yet, in which case raw data need to be processed first.
effort <- load_effort(year, month, single, dir ="C:\\Users\\KeppelE\\Documents\\CeMoRe\\Analysis\\cemore_analysis\\OUTPUT FILES\\GwaiiHaanas_202203\\dataEffort table")
effort_lines <- get_effort_lines(effort)
ap_sf <- load_sightings(year, month, single, dir ="C:\\Users\\KeppelE\\Documents\\CeMoRe\\Analysis\\cemore_analysis\\OUTPUT FILES\\GwaiiHaanas_202203\\dataSightings_True Positions") %>% 
  dplyr::select(c(year, month, Species, Group_Size, season))
# x <- ggplot2::coord_sf(xlim = c(-133, -125), ylim = c(50, 54), crs = sf::st_crs(4326))

bathy <- getNOAA.bathy(-133.5, -128,51.5, 55,res=1, keep=TRUE) %>%
  fortify(bathy)
bathy$z[which(bathy$z >= 0)] <- 0
col <- rev(RColorBrewer::brewer.pal(9L, "Blues")[4:7])
col_ramp <- colorRampPalette(col)
x <- ggplot2::coord_sf(xlim = c(-133, -128.5), ylim = c(52, 54.5), crs = sf::st_crs(4326))

# Plot effort trackline and effort sgt
plot_survey(Save = F,
            single_survey = T,
            badelf = NULL,
            plot_effort=T,
            effort_by_day=F,
            hydrophone = F,
            coord=x,
            bathy=bathy)

# Not showing many sightings on survey effort. Grab off effort bad elf tracks and plot them, along with also incidental sightings:
dir <- "C:\\Users\\KeppelE\\Documents\\CeMoRe\\Analysis\\cemore_analysis\\survey_data\\raw_data\\2022"
files <- list.files(file.path(dir, paste0(year,"-",month), "badelf"), all.files=T)[3:15]

# file_name <- files[2]
# test <- get_track(file_name, year = 2022, month = "03")

list <- purrr::map(files,get_track, year = 2022, month = "03")
line.list <- do.call(rbind, list)
lines <- get_lines(line.list)
# 
# plot_track(lines, coord=x) +
#   geom_sf(data=ap_sf)  + x


# Plot raw trackline and effort sgt
plot_survey(Save = F,
            single_survey = F,
            badelf = lines,
            plot_effort=T,
            effort_by_day=F,
            hydrophone = F,
            coord=x,
            bathy=bathy)

# Track both raw and effort tracklines, and effort sgt
plot_survey(Save = F,
            single_survey = T,
            badelf = lines,
            plot_effort=T,
            effort_by_day=F,
            hydrophone = F,
            coord=x,
            bathy=bathy)

inc <- get_incid(include_hw_porps=T)

# Track both raw and effort tracklines, and both effort and incidental sgt
plot_survey(Save = F,
            single_survey = T,
            badelf = lines,
            plot_effort=T,
            effort_by_day=F,
            hydrophone = F,
            coord=x,
            bathy=bathy,
            incidentals = T,
            incl = T)

# Add in fulcrum photo id/biopsy attempt records
dir <- "C:\\Users\\KeppelE\\Documents\\CeMoRe\\Analysis\\cemore_analysis\\survey_data\\raw_data\\2022/2022-03"
df <- read.csv(file.path(dir,"HW_20220325_Kitasu Bay_DFOCeMoRe_encounter data (1).csv"))
sf <- df %>% mutate(year = 2022, month = 3, Species = tolower(species), Group_Size = group_size,season = "Winter") %>% 
  dplyr::select(c(year, month, Species, Group_Size, season, latitude, longitude)) %>% st_as_sf(coords = c("longitude","latitude"), crs=4326)
ap_sf <- rbind(ap_sf, sf)
# ap_sf <- sf
plot_survey(Save = F,
            single_survey = T,
            badelf = lines,
            plot_effort=T,
            effort_by_day=F,
            hydrophone = F,
            coord=x,
            bathy=bathy,
            incidentals = T,
            incl = T) +
  labs(caption=
  "Cetacean survey March 25-29, 2022. Active survey effort is represented by the black survey 
  effort line while the grey trackline shows transit path between effort segments. Cetacean 
  sightings while surveying as well as incidental sightings while in transit are shown for the 
  area of interest. Point size indicates number of animals in individual sightings.") +
  theme(plot.caption= element_text(size=9,hjust=0.6))
ggsave("data requests/Kitasoo_data_request_202203.png")


