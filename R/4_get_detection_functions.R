# July 7, 2021
# Begin detection functions!
# library(purrr)
# library(lubridate)
# library(dplyr)
# library(ggnewscale)
# library(ggplot2)
# library(lubridate)
# library(magrittr)
# library(purrr)
# library(rgdal)
# library(sf)
# library(sp)
# source("C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/R/functions.R")
# source("C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis/R/utils.R")
cemore <- "C:/Users/keppele/Documents/GitHub/cemore/cemore"
load_all(cemore)
# get data - code originally from 'plot_survey.R'

# ----------------------------------------------------------------------
# ----------- LOAD ALL PROCESSED EFFORT DATA FOR ALL SURVEYS -----------
# ----------------------------------------------------------------------
# UP TO :
# year <- 2021
# month <- 10

coord <- ggplot2::coord_sf(xlim = c(-125.5, -123), ylim = c(48.1, 49.5), crs = sf::st_crs(4326)) 
# pal <- RColorBrewer::brewer.pal(12, "Paired")[c(2:10, 12)]

#-------------------------------------------
# Load previously saved data
all_effort <- readRDS(paste0("output/all_effort_to ", survey_title, ".rds"))
all_effort_lines <- readRDS(paste0("output/all_effort_lines_to ", survey_title, ".rds"))
# all_ap_sf <- readRDS(paste0("output/all_effort_sgt_to ", survey_title, ".rds"))

#----------------------------------------------
#         CREATE SEGMENTS WITH segmentation.R THEN RETURN HERE
#----------------------------------------------

l <- sum(st_length(all_effort_lines)) %>% units::set_units("km")

l

#-------------------------------------

# ----------------------------------------------------------------------
# -------------- IMPORT ALL SIGHTINGS DATA TO DATE---------
#---------------------------------------------------------------
###############################
all_ap_sf <- readRDS(paste0("output/all_effort_sgt_to ", survey_title, ".rds"))

all_ap_sf <- all_ap_sf %>% 
  filter(!is.na(PSD_nm)) %>% 
  mutate(distance = PSD_nm * 1.852,
         beauf = as.numeric(Beaufort),
         glare2 = Glare)
  
#######################

{setwd("OUTPUT FILES/dataSightings_True Positions")
files <- list.files(pattern = "\\.shp$")
AP <- purrr::map(files, rgdal::readOGR)
setwd("../../")
AP <- do.call(rbind, AP)}
# survey_title <- paste0("All CeMoRe surveys from Sep 2020 - ", survey_title)
#---------------------------------------------------------------
ap_sf <- AP %>% 
  st_as_sf() %>%
  filter(vessel == "MB") %>% 
  dplyr::filter(!is.na(PSD_nm), !SightedBy=="SHrushowy") %>%
  dplyr::mutate( 
    year = factor(year(time_index), c(2020,2021,2022)), 
    month = month(time_index), 
    month_abb = factor(month.abb[month], month.abb[]),
                   SightingTime = time_index,
                   distance = PSD_nm * 1852,
                   object = row_number(),
                   Sample.Label = paste(SurveyID, onseq_id, sep = "_"),
                   size = BestNumber,
                   SightedBy,
                   Beaufort = beauf,
                   long = final_lon,
                   lat = final_lat) %>% 
  dplyr::mutate(season = as.factor(case_when(
    month %in% c(6:8) ~ "Summer",
    month %in% c(9:11)  ~ "Fall",
    month %in% c(12, 1,2) ~ "Winter",
    month %in% c(3,4,5) ~ "Spring"
  )),
  # observer = case_when(
  #   observer == "CMcMillan" ~ 1,
  #   observer == "LSpaven"  ~ 2,
  #   observer == "EKeppel" ~ 3,
  #   observer== "SHrushowy" ~ 4
  # )
  ) %>% 
  st_transform(crs = st_crs(4326))
ap_sf$Species %<>% factor(c('Humpback Whale', 
                            'Harbour Porpoise', 
                            'Dalls Porpoise',
                            'Unknown Porpoise',
                            'Killer Whale - Northern Resident',
                            'Killer Whale - Southern Resident',
                            'Killer Whale - Transient',
                            'Killer Whale - Unknown ecotype',  # added for special account in Apr 2021
                            
                            'Minke Whale',
                            'Fin Whale',
                            "Grey Whale"))




ap_sf$month_abb[which(ap_sf$month_abb == "Aug" & ap_sf$year == 2020)] <- "Sep"
ap_sf$month_abb[which(ap_sf$month_abb == "May" & ap_sf$year == 2022)] <- "Apr"

lev <- unique(as.data.frame(ap_sf)[c("month_abb", "year")]) %>%  arrange(year, month_abb) %>%
  transmute(month_year = paste0(month_abb, " ", year)) %>%
  unlist()

ap_sf %<>%
  mutate(month_year = factor(paste0(month_abb, " ", year),
                             levels = lev))

#-------------------------------------------------------------------------------
# ------------------- Try for HW data-------------------------------------------
#-------------------------------------------------------------------------------
all_ap_sf <- all_ap_sf %>% 
  filter(!is.na(PSD_nm)) %>% 
  mutate(distance = PSD_nm * 1.852)

x <- all_ap_sf %>% filter(Species %like% "humpback") %>% 
  mutate(beauf = as.numeric(Beaufort))


par(mfrow=c(1,2))
hist(x$beauf, breaks=seq(0, 5, by=1),
     main="HW Observations per Beaufort level",
     xlab="Beaufort")
hist(all_effort_lines$Beaufort, breaks=seq(0, 5, by=1),
     main="HW Transects per Beaufort level",
     xlab="Beaufort")

par(mfrow=c(1,2))
hist(x$PSD_nm, xlab="Distance (nm)",breaks=seq(0,max(x$PSD_nm),len=30),
     main="Dist freq (nm) HW to Oct 2021")

hist(x$distance, xlab="Distance (km)",breaks=seq(0,max(x$distance),len=30),
     freq=FALSE, main="Dist freq (km) HW to Oct 2021", ylab="Probability of detection")

hist(x$distance, main="", xlab="Distance",breaks=seq(0,max(x$distance),len=7), 
     freq=FALSE, ylim=c(0, 85), axes=FALSE, ylab="Probability of detection")


l <- sum(st_length(all_effort_lines))
w = 2 # metres
a <- l * w * 2
n = nrow(x)


ggplot() +
  geom_sf(data = coast)+
  geom_sf(data = x, aes(colour = SurveyID)) +
  coord +
  ggtitle("Humpbacks, all surveys Sept 2020 - current")
#---------------------------------------------------------------
ggplot() + 
  geom_histogram(data = x, aes(distance, fill = beauf)) +
  ggtitle("Humpbacks, all surveys Sept 2020 - current")

df_hn <- ds(data.frame(x), truncation = 4)
summary(df_hn)
# calculate effective strip width

mu_hn <- predict(df_hn, esw=TRUE)$fitted[1]
#562
saveRDS(df_hn, "cemore_HW_ddf_hn_2021_oct.rds")

# Model : Half-normal key function 
# AIC   : 1313.127 
# 
# Detection function parameters
# Scale coefficient(s):  
#   estimate         se
# (Intercept) 6.103085 0.06760098
# 
# Estimate          SE         CV
# Average p             0.2801448  0.01893817 0.06760138
# N in covered region 342.6799827 37.64560437 0.10985644
plot(df_hn)
# savepdf("cemore_HW_ddf_hn_2021_jul.pdf")
# savePlot("cemore_HW_ddf_hn_2021_jul", type = "pdf")
df_hr <- ds(data.frame(x), key = "hr", truncation = 4)
summary(df_hr)

# Model : Hazard-rate key function 
# AIC   : 1314.428 
# 
# Detection function parameters
# Scale coefficient(s):  
#   estimate       se
# (Intercept) 6.182397 0.123572
# 
# Shape coefficient(s):  
#   estimate        se
# (Intercept) 1.296273 0.1977732
# 
# Estimate          SE         CV
# Average p             0.30244  0.02659071 0.08792062
# N in covered region 317.41834 38.87083339 0.12245932
plot(df_hr)



mu_hr <- predict(df_hr, esw=TRUE)$fitted[1] #576
phat_hr <- mu_hr/w #0.288
a <- 2*w*l # covered area

# Estimated Density
# n/phat/a
D_km2 <- (n/phat_hr)/a * 1000000 #0.0413/km2

# Nhat.lt <- (A/covered)* (n/p)

mu_hn <- predict(df_hn, esw=TRUE)$fitted[1] #519
phat_hn <- mu_hn/w #0.26
a <- 2*w*l
D_km2 <- (n/phat_hn)/a * 1000000 # 0.0458/km2


#-------------------------------------------------------------------------------
df_un <- ds(x, key = "unif", truncation = 2000)
summary(df_un)
plot(df_un)

#-------------------------------------------------------------------------------
# ------------------- Try for HP data-------------------------------------------
#-------------------------------------------------------------------------------
x <- ap_sf %>% filter(Species == "Harbour Porpoise") %>% 
  filter(!is.na(distance)) %>% 
  mutate(beauf = as.factor(beauf))
ggplot() +
  geom_sf(data = coast)+
  geom_sf(data = x, aes(colour = x$SurveyID)) +
  coord
#---------------------------------------------------------------
ggplot() + 
  geom_histogram(data = x, aes(distance, fill = beauf))

df_hn <- ds(data.frame(x), truncation = 550)
summary(df_hn)

# Summary for distance analysis 
# Number of observations :  482 
# Distance range         :  0  -  550 
# 
# Model : Half-normal key function with cosine adjustment term of order 2 
# 
# Strict monotonicity constraints were enforced.
# AIC   : 5750.149 
# 
# Detection function parameters
# Scale coefficient(s):  
#   estimate         se
# (Intercept) 5.292108 0.03922016
# 
# Adjustment term coefficient(s):  
#   estimate         se
# cos, order 2 0.1155652 0.07052024
# 
# Estimate          SE         CV
# Average p              0.4070949  0.02198946 0.05401557
# N in covered region 1183.9992009 76.25336710 0.06440323

plot(df_hn)

df_hr <- ds(data.frame(x), key = "hr", truncation = 550)
summary(df_hr)

# Summary for distance analysis 
# Number of observations :  482 
# Distance range         :  0  -  550 
# 
# Model : Hazard-rate key function with cosine adjustment term of order 2 
# 
# Strict monotonicity constraints were enforced.
# AIC   : 5749.312 
# 
# Detection function parameters
# Scale coefficient(s):  
#   estimate         se
# (Intercept) 5.659653 0.07699011
# 
# Shape coefficient(s):  
#   estimate        se
# (Intercept) 1.679825 0.1962855
# 
# Adjustment term coefficient(s):  
#   estimate         se
# cos, order 2 0.3936251 0.09285786
# 
# Estimate          SE         CV
# Average p              0.4119268  0.02244257 0.05448194
# N in covered region 1170.1108546 75.72660992 0.06471747
plot(df_hr)


