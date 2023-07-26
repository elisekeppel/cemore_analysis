# # Load raw data, tidy and format it for running Eva's code, run Eva's code
# 
# TO DO: CORRECT HOE FOR RB IN HOE.TXT
# source("R/0 Setup.R")
# iterations <- data.frame(cbind(
#   year = c(rep(2020,3),rep(2021, 11),rep(2022,3)),
#   month_abb =c("Sep", "Oct", "Nov", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Oct", "Nov", "Dec","Jan","Feb","Apr"),
#   iteration = c(1,2,3,6,7,8,9,10,11,12,13,14,15,16,18,19,20)))
# saveRDS(iterations, "C:/users/keppele/documents/github/cemore/tech_report/data/iterations.rds")
# ----------------------------------------------------------------------
# ----------------- SET SURVEY DATE --------------------------------
# ----------------------------------------------------------------------
# {year <- 2021
# month <- "02"
# month_abb <- month.abb[as.numeric(month)]
# survey_title <- paste(first_up(month_abb), year)
# folder = paste0("survey_data/raw_data/",year, "-", month)
# surveyid = paste0("cemore_", year, tolower(month_abb))
# iterations <- data.frame(cbind(
#   year = c(2020, 2020,2020,2021,2021,2021,2021,2021,2021,2021, 2021, 2021),
#   month_abb =c("Sep", "Oct", "Nov", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Oct"),
#   iteration = c(1,2,3,6,7,8,9,10,11,12,13,14)))
# iteration <- iterations$iteration[which(iterations$year == year & iterations$month_abb == month_abb)]
# }
# ----------------------------------------------------------------------
# ------------- IMPORT AND COLLATE MYSTI DATA --------------------------
# ----------------------------------------------------------------------
# import data from all mysti files and combine files of same type into objects in a list
#
# if there are any instances where there are 2 'final' mysti versions or tracklines,
# run combine_mysti() or combine_tracklines prior to this step
# combine_mysti_obs(2023,"01",12)
# combine_mysti_tracks(2023,"01",12)
# 
data <- get_obs_data(year=year,month=month, data.source = "cemore", vessel = vessel)
# data <- readRDS(paste0("survey_data/raw_data/collated_rds/cemore_survey_raw_data_", year, month, ".rds"))

# check comments fields
data$effort$QA.QC_Comments[which(!data$effort$QA.QC_Comments=="")]
data$effort$Comments[which(!data$effort$Comments=="")]

data$sightings$Comments[which(!data$sightings$Comments=="")]
# data$sightings[which(!is.na(data$sightings$C=omments))]

#create surveyID .txt file required for processing code
if(data.source=="cemore")data_path <- file.path(main.dir,"tidy_data", year, tolower(month_abb))
if(data.source=="mmcp") data_path <- file.path(main.dir,"tidy_data", year, tolower(month_abb),vessel)
if(!file.exists(data_path)) dir.create(data_path)

{
  survey <- read.table(paste0("survey_data/tidy_data/2022/oct/cemore_2022oct_dataSurveyID.txt"))
survey$SurveyID <- surveyid
survey$Date_Start_GMT <- paste0(substring(min(data$effort$time_index),1,10), substring(min(data$effort$time_index),12,19))
survey$Date_End_GMT <- paste0(substring(max(data$effort$time_index),1,10), substring(max(data$effort$time_index),12,19))
survey$Total_Days <- max(date(data$effort$time_index))-min(date(data$effort$time_index))+1
survey$Vessel_code <- "MB"
if(year==2020 & month==11) vessel <- "RB"
if(year==2021 & month==12) vessel <- "VE"
if(year==2022 & month_abb=="Mar") vessel <- "TA"
if(year==2023 & month_abb=="Jan") vessel <- "FR"
survey$Vessel_code <- vessel
if(data.source=="cemore") write.table(survey,file.path(data_path,paste0(data.source,"_",year,tolower(month_abb),"_dataSurveyID.txt")))
# if(data.source=="mmcp") write.table(survey,file.path(data_path,paste0(data.source,"_",year,tolower(month_abb),"_",vessel,"_dataSurveyID.txt")))
}

# 
# if(data.source=="cemore") survey <- read.table(file.path(data_path,paste0(data.source,"_",year,tolower(month_abb),"_dataSurveyID.txt")))
# # if(data.source=="mmcp") survey <- read.table(file.path(data_path,paste0(data.source,"_",year,tolower(month_abb),"_",vessel,"_dataSurveyID.txt")))
# # iteration <- read.csv(file.path(main.dir, "surveys.csv"))
# # process data for any corrections and create 'tidy data'
# 
# source(paste0(cemore, "/data_processing_scripts/data_corrections.R"))
# # source(paste0(cemore, "/data_processing_scripts/data_corrections_mmcp.R"))
# 
# #---------------------------------------------------------------
# # Special cases, check data for incidental sightings of interest
# #---------------------------------------------------------------
# # create temp data frame for any relevant incidental sightings
# s <- data$sightings %>% filter(Incidental.Sighting == T)# & !Species %like% c("Humpback") & !Species %like% "Porpoise")
# 
# # ########################################################################
# ###----------- Run CEMORE_DATA_PROCESSING.R -----------###
# ########################################################################
# # 1. CREATE SURVEY INFO FILE IN TIDY DATA FOLDER, move trackline data to trackline > transects > csv folder
# #    Note: if error in slow speed, check that all tracklines are in folder
# #    Note: if multiple mysti trackline files for the same day, combine using combine_tracks
# 
# # 2. RUN Eva's adapted code
# 

{
  # rm(list = ls(all=TRUE))
  # cemore <- "C:/Users/keppele/Documents/GitHub/cemore/cemore"
  # devtools::load_all(cemore)
  # data.source = "cemore"
  # vessel = "MB"
  
  # year = 2023
  # month = "05"
  month_abb <- month.abb[as.numeric(month)]
  if(data.source == "cemore"){
    surveyid = paste0(data.source,"_", year, tolower(month_abb))
    # surveyid = paste0(data.source,"_", year, tolower(month_abb),"_",vessel)
    main.dir = "survey_data"
    data_path <- file.path(main.dir,"tidy_data", year, tolower(month_abb))
  }
  if(data.source == "mmcp"){
    surveyid = paste0(data.source,"_", year, tolower(month_abb), "_",vessel)
    main.dir = "mmcp_data"
    data_path <- file.path(main.dir,"tidy_data", year, tolower(month_abb), vessel)
  }
  options(java.parameters="-Xmx12000m")
  path<-getwd() #dirname(rstudioapi::getActiveDocumentContext()$path)
  
  if(grep("/",path)){
    u <- "/"
  } else {
  }
  system.time(source(paste(cemore,u,"data_processing_scripts",u,"CEMORE_DATA_PROCESSING.R",sep="")))
}

#-------------------------------------------------------------------
# ----------------------- SAVE INDIV SURVEYS --------------------
#-------------------------------------------------------------------

single <- T
effort <- load_effort(year, month, single)
effort_lines <- get_effort_lines(effort)
ap_sf <- load_sightings(year, month, single)



# saveRDS(effort, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/effort_", year, "_",month, ".rds"))
saveRDS(effort, paste0("output_",data.source,"/effort/effort_ ", year, "_",month, ".rds"))

# saveRDS(effort_lines, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/effort_lines_ ", year, "_",month, ".rds"))
saveRDS(effort_lines, paste0("output_",data.source,"/effort_lines/effort_lines_ ", year, "_",month, ".rds"))

# saveRDS(ap_sf, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/effort_sgt_ ", year, "_",month, ".rds"))
saveRDS(ap_sf, paste0("output_",data.source,"/sgt/effort_sgt_ ", year, "_",month, ".rds"))

# #-------------------------------------------------------------------
# # ----------------------- SAVE ALL SURVEYS --------------------
# #-------------------------------------------------------------------
survey_title <- paste(first_up(month_abb), year)

all_effort <- load_effort(year, month, single=F, vessel = "MB")
all_effort_lines <- get_effort_lines(all_effort)
all_ap_sf <- load_sightings(year, month, single=F, vessel = "MB")

saveRDS(all_effort, paste0("C:/users/keppele/documents/github/cemore/tech_report/output_cemore/all_effort_to_", year, "_",month, ".rds"))
saveRDS(all_effort, paste0("output_",data.source,"/all_effort/all_effort_to_", year, "_",month, ".rds"))

saveRDS(all_effort_lines, paste0("C:/users/keppele/documents/github/cemore/tech_report/output_cemore/all_effort_lines_to_", year, "_",month, ".rds"))
saveRDS(all_effort_lines, paste0("output_",data.source,"/all_lines/all_effort_lines_to_", year, "_",month, ".rds"))

saveRDS(all_ap_sf, paste0("C:/users/keppele/documents/github/cemore/tech_report/output_cemore/all_effort_sgt_to_", year, "_",month, ".rds"))
saveRDS(all_ap_sf, paste0("output_",data.source,"/all_sgt//all_effort_sgt_to_", year, "_",month, ".rds"))

# # all_ap_sf %>% data.frame() %>% group_by(SurveyID) %>% summarise(diff=sum(!(!is.na(port_visib)==!is.na(stbd_visib))))
# # unique(all_ap_sf$Species)
# 
