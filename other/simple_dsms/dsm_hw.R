#--------------------------------------
#--------------------------------------
# Humpback simple density/abundance
#--------------------------------------
#--------------------------------------
source("R/0 Setup.R")
# single = F
# all_effort <- load_effort(2022, 02, single,vessel=="MB")
# all_effort_lines <- get_effort_lines(all_effort)
# all_ap_sf <- load_sightings(year, as.numeric(month), single,,vessel=="MB")

#-----------------------------------------
#-----------------------------------------
# Load previously saved data
#-----------------------------------------
#-----------------------------------------
# Effort data
all_effort_lines <- readRDS(paste0("output/all_effort_lines_to ", survey_title, ".rds")) 
# ggplot()+geom_sf(data=all_effort_lines)
l <- sum(st_length(all_effort_lines)) %>% as.numeric()/1000
w = 2
a <- l * w * 2

# Sightings data
sp = "Humpback Whale"
# sp = "Harbour Porpoise"
all_ap_sf <- readRDS(paste0("output/all_effort_sgt_to ", survey_title, ".rds")) %>% 
  data.frame() %>% filter(Species == sp) 


#-----------------------------------------
#-----------------------------------------
#  SET UP DATA FRAMES
#-----------------------------------------
#-----------------------------------------

#-----------------------------------------
# Effort data = sample.table
#-----------------------------------------
effort <- all_effort_lines %>% 
  data.frame() %>% 
  # unique segment ID's and length for each
  transmute(Sample.Label=TransectID,
            # Region.Label = SurveyID,
            Region.Label = season,
            Effort = st_length(geometry),
            Area=B) %>% 
  # dplyr::select(-c(day,TransectID,CloudCover)) %>% 
  group_by(Region.Label,Area,Sample.Label) %>% 
  summarise(Effort=as.numeric(sum(Effort))/1000) %>% ungroup()
# units(effort$Effort) <- "km"

#-----------------------------------------
# Region.table
#-----------------------------------------
# ---Areas from cemore_design.Rproj@create_monthly_survey_design---
# sum(Full_study_area@area) 
# A <- 5039.467 # km^2 (total survey area - should this be Cdn only?)
# units(A) <- units::as_units("km^2")
# ------- OR --------
# study_area_can <- st_intersection(Full_study_area@region, canada_shp)
# st_area(study_area_can) %>% sum()
B <- 2937.946 # %>% units::set_units(km^2)#km ^2

# reg <- data.frame(Region.Label="Cdn_survey_area",Area=B)
reg <- data.frame(unique(effort$Region.Label), Area=B)
# reg <- data.frame(season=unique(effort$season), Area=B)

#-----------------------------------------
# Sightings data = obs.table
#-----------------------------------------
sightings <- all_ap_sf %>%
  rename(Observer=SightedBy,
         size=Group_Size) %>%
  dplyr::mutate(Survey=SurveyID,
                Beaufort=as.numeric(as.character(Beaufort)),
                GroupSize = factor(
                  dplyr::case_when(
                    size==1 ~ "1",
                    size==2 ~ "2",
                    size==3 ~ "3",
                    size>3 ~ "4+"
                  ), levels=  c("1","2","3","4+")),
                swell=factor(case_when(
                  swell == "Big >2" ~ "Mod-Big",
                  swell == "Moderate 1-2 m" ~ "Mod-Big",
                  swell == "Low <1 m" ~ "Low",
                  swell == "No swell" ~ "None"),
                  levels= c("None", "Low", "Mod-Big")),
                Clumped_Beaufort = case_when(
                  Beaufort <2 ~   "0-1",
                  Beaufort == 2 ~ "2",
                  Beaufort > 2 ~  "3+"),
                Clumped_Vis = case_when(
                  Visibility == "G&E"~ "G/E",
                  !Visibility == "G&E"~ "M/P"),
                Observer = case_when(
                  Observer == "CMcMillan" ~"a",
                  Observer == "EKeppel"~"b",
                  Observer == "LSpaven"~ "c")) %>% 
  mutate(
    # Region.Label= Survey,
    Region.Label= season,
    Sample.Label=TransectID,
         object=Sgt_ID) %>% 
  dplyr::select(Region.Label,
                # Survey=SurveyID,
                Sample.Label,
                # SightingTime= time_index,
                # year,
                # season,
                # month,
                # month_abb,
                # object,
                distance,
                # Species,
                size,
                Clumped_Vis
                # Beaufort
                )
n <-  nrow(sightings)
#-----------------------------------------


# find transects with no sightings and create NA distance rows
obs <- full_join(sightings, effort) #%>% #left_join(reg) %>% 
  # dplyr::select(-size)
  # mutate(size=case_when(
  #  is.na(size) ~ 0,
  #  !is.na(size) ~ size
  # ))
# CHECK: calculate number of rows sightings df should have 
# total number of transects in effort - number of transects with sightings = missing transects
etransects <- unique(effort$Sample.Label)
# stransects <- unique(df$Sample.Label)
# add number of missing transects to number of sightings for number of rows we should have
# if(length(etransects) - length(stransects) + nrow(df) == nrow(obs)) print("obs df correct length")
# CHECK: expected number of unique transect id's are the same as in effort df
if(length(unique(obs$Sample.Label)) == nrow(effort)) print("correct number of unique transect id's in obs")

# load detection function

#--------------------------------
#**Calculating the Horvitz-Thompson estimate**
A=B
a=sum(st_length(all_effort_lines)) %>% as.numeric()/1000 *w*2 #km

# N=A/a*sum((group_sizes/pi))

#--------without size-----------------
# obs2 <- obs %>% dplyr::select(-size)
# hr.v2 <- ds(data=obs2, truncation=2, key="hr", formula=~Clumped_Vis)
# summary(hr.v2)

# Note to mention availability bias given the short amount of time to sight HW if they only surface onece in the time we can sight them along the transect

#--------with size-----------------

hr.v2b <- ds(data=obs, truncation=2, key="hr", formula=~Clumped_Vis)
summary(hr.v2b)

group_sizes2 <- hr.v2b$ddf$data$size %>% as.numeric()
group_sizes2 <- group_sizes2[hr.v2b$ddf$data$distance <= w]

probs2 <- predict(hr.v2b$ddf)$fitted %>% as.numeric()
N2 <- B/a*sum(group_sizes2/probs2)

check <- data.frame(dist = hr.v2b$ddf$data$distance, prob = predict(hr.v2b$ddf)$fitted)

# try separating by season then running models for each
# then use this script as a template for the other species :D

#-------------------------------------------------
#------------------- DSM -------------------------
#-------------------------------------------------

#filter dataset for truncation distance
obs <- obs[obs$distance <= hw.hr2$ddf$meta.data$width, ]

#------------------- DSM -------------------------
#filter dataset for truncation distance
obs <- obs[obs$distance <= hr.v2$ddf$meta.data$width, ]
library(dsm)
dsm_x_tw <- dsm(count~s(geometry), ddf.obj=hr.v2,
                segment.data=segs, observation.data=obs,
                family=tw())
