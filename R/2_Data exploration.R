# Data exploration
source("R/0 Setup.R")
devtools::load_all(cemore)

{year <- 2023
month <- "05"
month_abb <- month.abb[as.numeric(month)]
survey_title <- paste(first_up(month_abb), year)
data.source = "cemore"
vessel <- "MB"

# surveyid = paste0("cemore_", year, tolower(month_abb))
surveyid = paste0(data.source, "_", year, tolower(month_abb))
if(!exists("year")) stop("No year has been entered. Enter year of survey. For example, enter year <- 2021")
if(!exists("month")) stop("No month has been entered. Enter month of survey. For example, for October, enter month <- '10'")
}
# iteration <- surveys$iteration[which(surveys$year == year & surveys$month_abb == month_abb & surveys$SurveyID == surveyid)]
# Load monthly data

# ----------------------------------------------------------------------
# ----------------- PLOT BAD ELF TRACK -----------
# ----------------------------------------------------------------------
# transect_dir <- paste0("./survey_data/raw_data/",year,"/",year,"-",month,"/bad elf")
# files <- list.files(transect_dir)
# transects <- purrr::map_df(files, get_track, year = year, month = month)
# plot_track(transects=transects)

# ----------------------------------------------------------------------
# ----------------- LOAD PROCESSED EFFORT and SIGHTINGS DATA -----------
# ----------------------------------------------------------------------
# On effort segments only - from eva's code output
{
  single <- T
  effort <- load_effort(year, month, single, vessel = "MB", data.source = "cemore")
  effort_lines <- get_effort_lines(effort)
  ap_sf <- load_sightings(year, month, single, vessel = "MB", data.source = "cemore")
}

# unique(ap_sf$Species)
# ggplot(effort_lines) + geom_sf()
# ggplot(effort_lines %>% filter(TransectID=="cemore_2022jun2")) + geom_sf()
# PLOT TRACKLINE
coast <- sf::st_read(dsn = "shapefiles", layer = "BC_coast_UTM9") %>% st_transform(crs = 4326)
# coord <- ggplot2::coord_sf(xlim = c(-125.4, -123), ylim = c(48.15, 49.4), crs = sf::st_crs(4326)) # southern Salish Sea
# coord <- ggplot2::coord_sf(xlim = c(-132, -129), ylim = c(52.5, 54.5), crs = sf::st_crs(4326)) #North/central coast
coords <- ggplot2::coord_sf(xlim = c(-126, -123), ylim = c(48.15, 50.1), crs = sf::st_crs(4326)) # SoG

plot_survey(leg.pos = "right")

ggplot() + geom_sf(data = coast, fill = "light yellow") +
  geom_sf(data = effort_lines, size = 0.5, aes(colour = as.factor(day))) +
  # geom_sf(data = effort_lines, size = 0.5, aes(linetype = status)) +
  # scale_linetype_manual(values=c("On Effort"="solid", "In Transit" = "dashed"))+
  # geom_sf(data = effort_lines, size = 0.5, aes(colour = as.factor(Beaufort))) +
  labs(colour="Day")+
  coords +
  ggtitle(paste0(data.source," survey trackline ", month_abb, " ",year)) +
  # scale_color_manual(name = "Day") + #values = c(cols),
  # guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(axis.text.x = element_text(angle=75,vjust=0.5))+
  theme(legend.position = "right") 

# RUN SUMMARY.R FOR SURVEY SIGHTINGS SUMMARY FOR REPORT
summary <- survey_summary(data.source = "cemore", vessel="MB")
# summary <- survey_summary(data.source = "mmcp", vessel=vessel)
summary[[1]] # sgts
summary[[2]] # species
summary[[3]] # effort
summary[[4]][c(1,2,3,4,10)] # survey
View(summary[[3]]) # effort

# source(file.path(cemore, "developing/summary.R"))
cat(paste0("Number of transects completed in ", month_abb, " ", year, ": ", length(unique(effort$TransectID))))
cat(paste("Total km surveyed in",month_abb, year,"survey :", round(sum(st_length(effort_lines))/1000,0) %>% units::set_units(NULL)))

x <- rbind(summary[[2]], summary[[1]])
x$Species <- as.character(x$Species)
x$Species[7] <- "ALL SPECIES"
x
y <- full_join(summary[[3]][nrow(summary[[3]]),], summary[[4]]) %>% ungroup() %>%
  # y <- full_join(summary[[3]], summary[[4]]) %>% ungroup() %>% 
  dplyr::select(-c(start_day, end_day,firstday,lastday,start_month,end_month,date,TransectID,distance_km))
View(y)

# CHECK RAW DATA FOR INCIDENTAL SIGHTINGS OF INTEREST

                  
incid <- get_incid(single=T, include_hw_porps = T)
all_incid <- get_all_raw_sgt(single=F)

#--------------------------------------
#         Plot survey
#--------------------------------------
# bathymetry for north coast survey Franklin 2023 January
bathy <- getNOAA.bathy(-132.5, -127.5,55.5, 51.5,res=1, keep=TRUE) %>%
  fortify(b)
# coord <- coord_sf(xlim = c(-132, -128), ylim = c(52, 55), crs = sf::st_crs(4326)) # North Coast

# plot_survey(Save = F,
#             plot_sgt = F,
#             # species = "harbour porpoise",
#                  single_survey = T,
#             # season=T,
#                  # incidentals =  T,
#             effort_by_vis = T,
#             bathy = bathy,
#             coord = coord,
#                  hydrophone = F,
#             leg.pos = "right") 
xmin=min(effort$Longitude)-1
xmax=max(effort$Longitude)+1
ymin=max(effort$Latitude)-1
ymax=min(effort$Latitude)+1
xmin= -123 
xmax=  -125.5
ymin=  49
ymax=  50.5
coords <- ggplot2::coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), crs = sf::st_crs(4326)) # SoG
plot_survey(Save = F,
            border = F,
            effort_data = effort_lines,
            
            xmin= -125.5,
            xmax=  -123 ,
            ymin=  49,
            ymax=  50.5,
            # xmin=min(effort$Longitude)-0.2,
            # xmax=max(effort$Longitude)+0.2,
            # ymin=max(effort$Latitude)-0.5,
            # ymax=min(effort$Latitude)+0.5,
            # species = "harbour porpoise",
            single_survey = T,
            # incidentals =  T,
            # incl = T,
            # effort_by_vis = F,
            # show_transit = T,
            # bathy = bathy,
            coord = coords,
            # hydrophone = F,
            leg.pos = "right")

# Plot all surveys
plot_survey(Save = F,
                 border = T,
                 single_survey = T,
                 incidentals =  T,
                 hydrophone = F)
g
# For North coast (Franklin Jan 2023 survey)
# coord <- coord_sf(xlim = c(-126.5, -123.5), ylim = c(46, 50), crs = sf::st_crs(4326))

#-------------------------------------------------------------------
# ----------------------- ALL SURVEYS --------------------
#-------------------------------------------------------------------
# these data are prepared in 1 Data processing.R

# Load previously saved data
# all_effort <- load_effort(year = year, month=month, vessel="MB",single_survey = F)
# all_effort_lines <- get_effort_lines(all_effort)
# all_ap_sf <- load_sightings(year=year, month=month, single_survey=F, vessel="MB")
# saveRDS(all_effort, paste0("output_cemore/all_effort/all_effort_to_", year,"_",month, ".rds"))
# saveRDS(all_effort_lines, paste0("output_cemore/all_lines/all_effort_lines_to_", year,"_",month, ".rds"))
# saveRDS(all_ap_sf, paste0("output_cemore/all_sgt/all_effort_sgt_to_", year,"_",month, ".rds"))
all_effort <- readRDS(paste0("output_cemore/all_effort/all_effort_to_", year,"_",month, ".rds"))
all_effort_lines <- readRDS(paste0("output_cemore/all_lines/all_effort_lines_to_", year,"_",month, ".rds"))
all_ap_sf <- readRDS(paste0("output_cemore/all_sgt/all_effort_sgt_to_", year,"_",month, ".rds"))
# filter for species of interest
# all_ap_sf %<>% filter(Species %like% "Humpback")
# length(unique(all_effort_lines$ONSEQ_ID))
sum <- survey_summary(single=F)
# all_effort %>% filter(date<lubridate::make_date(year=year, match(month_abb,month.abb))+1)
View(sum[[1]]) # sgts
sum[[2]] %>% filter(Species %like% "harbour") %>% group_by(Species,SurveyID) %>% summarise(n_sgt = sum(number_sightings), n_indiv=sum(number_individuals))# species
sum[[2]] %>% group_by(Species) %>% summarise(n_sgt = sum(number_sightings), n_indiv=sum(number_individuals))# species
sum[[3]] # effort
sum[[4]] # survey
View(summary[[3]]) # effort

#-------------------------------------------
#--------- plot effort  --------
#-------------------------------------------
plot_survey(single_survey=F)
plot_survey(single_survey=F, plot_effort = F, effort_data=all_effort_lines,plot_sgt = T, species = "humpback whale",season = T)
plot_survey(single_survey=F, plot_effort = T, plot_sgt=F, monthly = T)
#-------------------------------------------
#--------- Species counts by survey --------
#-------------------------------------------
effort <- all_effort %>% group_by(SurveyID) %>% 
  summarise(n()) # to maintain all surveys in dataframe (include surveys with 0 sgts)
df <- all_ap_sf %>% data.frame()
hw <- df %>% filter(Species %like% "humpback") %>% 
  right_join(effort)
# ggplot(hw) + # just counts sightings
#   geom_bar(aes(x=SurveyID)) +
#   theme(axis.text.x = element_text(angle=75,vjust=0.5))
ggplot(hw, aes(x = SurveyID, y = Group_Size)) +
  geom_col(aes(colour=season,fill=season)) + # (same as geom_bar, stat="identity")
  theme(axis.text.x = element_text(angle=75,vjust=1.5,hjust=1.5)) +
  ggtitle("Humpbacks")
ggsave("summaries/hw_sightings_by_survey.png")

hp <- df %>% filter(Species %like% "harbour") %>% 
  right_join(effort)
ggplot(hp, aes(x = SurveyID, y = Group_Size)) +
  geom_col() + # (same as geom_bar, stat="identity")
  theme(axis.text.x = element_text(angle=75,vjust=1.5,hjust=1.5)) +
  ggtitle("Harbour porpoise")
ggsave("summaries/hp_sightings_by_survey.png")

dp <- df %>% filter(Species %like% "Dall") %>% 
  right_join(effort)
ggplot(dp, aes(x = SurveyID, y = Group_Size)) +
  geom_col() + # (same as geom_bar, stat="identity")
  theme(axis.text.x = element_text(angle=75,vjust=1.5,hjust=1.5)) +
  ggtitle("Dall's porpoise")
ggsave("summaries/dp_sightings_by_survey.png")

tkw <- df %>% filter(Species %like% "Bigg") %>% 
  right_join(effort)
ggplot(tkw, aes(x = SurveyID, y = Group_Size)) +
  geom_col() + # (same as geom_bar, stat="identity")
  theme(axis.text.x = element_text(angle=75,vjust=1.5,hjust=1.5)) +
  ggtitle("Bigg's")
ggsave("summaries/tkw_sightings_by_survey.png")

srkw <- df %>% filter(Species %like% "southern") %>% 
  right_join(effort)
ggplot(srkw, aes(x = SurveyID, y = Group_Size)) +
  geom_col() + # (same as geom_bar, stat="identity")
  theme(axis.text.x = element_text(angle=75,vjust=1.5,hjust=1.5)) +
  ggtitle("Southern residents")
ggsave("summaries/srkw_sightings_by_survey.png")

kw <- df %>% filter(Species %like% "killer") %>% 
  right_join(effort) %>% mutate(Group_Size = case_when(
    is.na(Species) ~ 0,
    !is.na(Species) ~ Group_Size
    )
)
ggplot(kw, aes(x = SurveyID, y = Group_Size, fill=Species)) +
  geom_col(position="dodge") + # (same as geom_bar, stat="identity")
  theme(axis.text.x = element_text(angle=75,vjust=1.5,hjust=1.5)) +
  ggtitle("Killer whales")
ggsave("summaries/all_kw_sightings_by_survey.png")

length(unique(all_effort_lines$TransectID))
len <- st_length(all_effort_lines) %>% sum()#/1000 
units(len) <- "km"
len
x <- all_effort_lines %>% mutate(length_m=st_length(geometry)) %>% 
  as.data.frame() %>% dplyr::select(-geometry) %>% 
  group_by(SurveyID) %>% summarise(survey_length_m=round(sum(length_m),digits=0))
x$survey_length_km <- x$survey_length_m
units(x$survey_length_km) <- "km"

View(x)

summary_all <- survey_summary(single=F)
View(summary_all[[1]]) # sighting counts
View(summary_all[[2]]) # species counts
View(summary_all[[3]]) # distance/transects
View(summary_all[[4]]) # dates
all_effort_lines %>% group_by(TransectID) %>% summarise(length=st_length(geometry))

# Plot all surveys
plot_survey(Save = F,
            border = T,
            single_survey = F,
            incidentals =  F,
            hydrophone = F,
            effort_only = F)

# total number of survey days and total count cetacean sightings; manually check any NA values
all_ap_sf %>% as.data.frame() %>% dplyr::group_by(year, month_abb, Species) %>%
  dplyr::summarise(field_days = (max(day(time_index)) - min(day(time_index)) + 1), number_sightings = n(), number_indivduals = sum(Group_Size))


df <- as.data.frame(all_ap_sf) 
df %<>% arrange(year, month) 
df %<>%
  mutate(Survey = paste(month_abb, year, " "),
         Count = Group_Size)
lev <- unique(df$Survey)
df$Survey  <- factor(df$Survey, levels = lev) 

# all non-target species in all surveys (including incidentals)
x <- get_all_raw_sgt()# %>% filter(Species=="Harbour Porpoise")
all <- x %>% 
  filter(!Species %like% c("Humpback") & !Species %like% "Porpoise") %>% 
  mutate(Best.Cnt=ifelse(is.na(Best.Cnt), 1,Best.Cnt)) %>% 
  group_by(Species) %>% summarise(sgt=length(Species), indiv=sum(Best.Cnt))
 # unique(x$Species)
# Look at how many indicental sightings there were of non-target species
on_eff  <- all_ap_sf %>% as.data.frame() %>% 
   filter(!Species %like% c("Humpback") & !Species %like% "Porpoise") %>% 
   group_by(Species) %>% summarise(on_eff_sgt=length(Species), on_eff_indiv=sum(Group_Size))
full_join(all, on_eff) %>% mutate(incid_sgt=sgt-on_eff_sgt, incid_indiv=indiv-on_eff_indiv)

ggplot(df, aes(x=Survey, y=Count)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Individual porpoises sighted by survey")
theme(axis.text.x = element_text(angle = 90))

summary %<>% cbind(year, season)
# counts of sightings by each survey
all_ap_sf %>%  as.data.frame() %>% dplyr::group_by(year, month) %>%
  dplyr::summarise(number_sightings = n(), number_indivduals = sum(Group_Size))

  # counts by species in each survey
summary <- all_ap_sf %>%  as.data.frame() %>% dplyr::group_by(year, month, Species) %>%
  dplyr::summarise(number_sightings = n(), number_indivduals = sum(Group_Size))
View(summary)

# counts by species in all surveys
all_ap_sf %>%  as.data.frame() %>% dplyr::group_by(Species) %>%
  dplyr::summarise(number_sightings = n(), number_indivduals = sum(Group_Size), avg_grp_sz = mean(Group_Size))

# counts by species in all surveys by beaufort
bf <- all_ap_sf %>% as.data.frame() %>% filter(vessel == "MB") %>% dplyr::group_by(Species, beauf) %>%
  dplyr::summarise(number_sightings = n(), number_indivduals = sum(Group_Size), avg_grp_sz = mean(Group_Size))
View(bf)

x <- all_ap_sf %>% group_by(Species, year, month) %>% dplyr::summarise(n =sum(Group_Size))
View(x)
# days on effort per survey
y <- all_effort %>% filter(Status == "ON") %>%
  dplyr::group_by(Year = year(GpsT), Month = factor(month.abb[month(GpsT)], levels = month.abb[1:12]), SurveyID) %>% 
  dplyr::summarise("# survey days" = n_distinct(day(GpsT)), "# transects" = (n_distinct(TransectID)))
sum(y$`# transects`)


x <- all_effort_lines %>% mutate(length_km=st_length(geometry)/1000) %>%
  as.data.frame() %>% dplyr::select(-geometry) %>% 
  mutate(Clumped_Beaufort = case_when(
    Beaufort <2 ~ "0-1",
    Beaufort == 2 ~ "2",
    Beaufort > 2 ~ "3+"))
v <- x %>% group_by(season,SurveyID,Clumped_Beaufort)%>%
  summarise(survey_length_km=round(sum(length_km),digits=0))
o <- x %>% group_by(Port.Obs,Clumped_Beaufort)%>%
  summarise(survey_length_km=round(sum(length_km),digits=0))
p <- x %>% group_by(Stbd.Obs,Clumped_Beaufort)%>%
  summarise(survey_length_km=round(sum(length_km),digits=0))


w <- x %>% group_by(season) %>%
  summarise(survey_length_km=round(sum(length_km),digits=0))
y <- x %>% group_by(season,Clumped_Beaufort) %>%
  summarise(length_km=round(sum(length_km),digits=0)) 
units(y$length_km) <- NULL
units(w$survey_length_km) <- NULL

z <- full_join(y,w) %>% 
  mutate(percentage=round(length_km/survey_length_km*100,digits=1))

ggplot(z)+geom_col(aes(x=season,y=percentage,fill=as.factor(Clumped_Beaufort)))

z <- y %>% tidyr::pivot_wider(names_from=Clumped_Beaufort, values_from=length_km)
View(z)
write.csv(summary, paste0("output/sightings_summary/cemore_seasonal_sightings_summary_to_", year, tolower(month_abb), ".csv"), row.names = FALSE)
write.csv(bf, paste0("output/sightings_summary/cemore_sgts_summ_by_bf_to_", year, tolower(month_abb), ".csv"), row.names = FALSE)

# {Save = F
#   single_survey <- F
#   incidentals <-  F
#   hydrophone <- F
#   sightings_only <- F
#   # source(file.path(cemore,"developing/plot_survey.R"))
#   source(file.path("R/plot_survey.R"))
# }

files()
gpx <- plotKML::readGPX("E:/SEAiq_Tracks_20220427_162914.gpx")
# plot lowrance gpx trails
gpx <- plotKML::readGPX("survey_data\\tracklines\\transects\\gpx\\2022-04seaiq\\SEAiq_Tracks_20220427_162914.gpx")

gpx <- plotKML::readGPX("SEAiq_Tracks_20220427_162914.gpx")

x <- list.files("C:\\Users\\KeppelE\\Documents\\CeMoRe\\Analysis\\cemore_analysis\\survey_data\\raw_data\\2022-04")
gpx <- plotKML::readGPX("SEAiq_Tracks_20220427_162914.gpx")[[4]][[1]][[1]]

route = readOGR("SEAiq_Tracks_20220427_162914.gpx","tracks")
route %<>% sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  dplyr::mutate(date = date) %>%
  dplyr::select(time, date, type, geometry, hour) %>%
  dplyr::group_by(date, hour) %>%
  dplyr::summarize(do_union=FALSE) %>%
  sf::st_cast("LINESTRING")
sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  %>% 
  sf::st_cast("LINESTRING")
coordinates(route)[[1]][[1]]

OGR data source with driver: GPX 
Source: "Testing.gpx", layer: "tracks"
with 1 features
It has 12 fields
> plot(route)
                        