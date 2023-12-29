# Set up working environment
{ 
  rm(list=ls(all=T))
  options(java.parameters="-Xmx12000m")
  # setwd("C:/Users/keppele/Documents/CeMoRe/Analysis/cemore_analysis")
  library(Rcpp)
  library(data.table)
  library(Distance)
  library(dplyr)
  library(gdalUtils)
  library(ggnewscale)
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(gtable)
  library(lubridate)
  library(magrittr)
  library(maps)
  library(marmap)
  library(purrr)
  library(raster)
  library(rasterly)
  library(RColorBrewer)
  library(rgdal)
  library(sf)
  library(sp)
  library(stringr)
  library(plotKML)
  cemore <- "C:/Users/keppele/Documents/GitHub/cemore/cemore"
  devtools::load_all(cemore)
  suppressMessages(sf::sf_use_s2(FALSE))
  dir <- "C:/users/keppele/documents/cemore/analysis/cemore_analysis"
}

{ 
  year <- 2023
  month <- "11"
  vessel <- "MB"
  iteration <- "30"
  # iteration <- "2023b"
  month_abb <- month.abb[as.numeric(month)]
  survey_title <- paste(first_up(month_abb), year)
  data.source <- "cemore"
  # data.source <- "mmcp"
  surveyid = paste0(data.source,"_", year, tolower(month_abb))
  # surveyid = paste0(data.source,"_", year, tolower(month_abb),"_",vessel)
  main.dir <- "survey_data"
  # main.dir <- "mmcp_data"
  rds <- file.path(dir, main.dir, "surveys.rds")
   
}

## set up surveys dataframe (used to be 'iterations' dataframe)
# {
#   years = c(rep(2020,5),rep(2021, 11),rep(2022,9))
#   month_abbs =c("Jul","Aug","Sep", "Oct", "Nov", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Oct", "Nov", "Dec","Jan","Feb","Apr","Jun","Jul","Aug","Sep","Oct","Dec")
#   
#   surveys <- data.frame(cbind(
#     year = years,
#     month_abb=month_abbs,
#     iteration = c(99,99,1,2,3,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,24,25),
#     SurveyID = paste0("cemore_", years, tolower(month_abbs)),
#     objectives =c("Testing survey protocols; staff familiarization with research methods, protocols, and equipment",
#                   "Staff familiarization with research equipment and research vessel; first line-transect survey from primary vessel",
#                   "Line-transect surveys; deployment and retrieval of data-logging tags on humpback whales",
#                   "Line-transect surveys",
#                   "Line-transect surveys",
#                   # "Photo-identification and genetic sampling of humpback whales",
#                   "Line-transect surveys",
#                   "Line-transect surveys",
#                   "Line-transect surveys",
#                   "Line-transect surveys",
#                   "Line-transect surveys",
#                   "Line-transect surveys; deployment and recovery of data-logging tags on humpback whales",
#                   "Line-transect surveys",
#                   "Line-transect surveys; deployment of acoustic recorders; deployment and recovery of data-logging tags on humpback whales; photo-identification and genetic sampling of humpback whales",
#                   "Line-transect surveys; deployment of acoustic recorders",
#                   "Line-transect surveys; recovery of acoustic recorders",
#                   "Line-transect surveys",
#                   "Line-transect surveys",
#                   "Line-transect surveys; deployment of acoustic recorders",
#                   "Line-transect surveys",
#                   "Line-transect surveys; deployment of acoustic recorders",
#                   "Line-transect surveys; deployment and recovery of acoustic recorders; deployment and recovery of data-logging tags on humpback whales; photo-identification and genetic sampling of humpback whales",
#                   "Line-transect surveys; deployment and recovery of acoustic recorders; photo-identification and genetic sampling of humpback whales",
#                   "Line-transect surveys; deployment and recovery of acoustic recorders",
#                   "Line-transect surveys; deployment and recovery of acoustic recorders; deployment and recovery of data-logging tags on humpback whales",
#                   "Line-transect surveys; deployment and recovery of acoustic recorders"
#     )
#   ))
#   
#   surveys$iteration <- as.numeric(surveys$iteration)
#   lev <- unique(surveys$SurveyID)
#   surveys$SurveyID %<>% factor(levels=lev)
#   surveys$month_abb <- factor(surveys$month_abb, levels=month.abb)
# 
#   saveRDS(surveys, rds)
#   csv <- file.path(dir, "survey_data/surveys.csv")
#   write.csv(surveys, csv, row.names=F)
#   }

surveys <- readRDS(rds)
# surveys <- surveys[1:nrow(surveys)-1,]
# surveys$SurveyID <- paste0(surveys$SurveyID, "_MB")
# iteration <- surveys$iteration[which(surveys$year == year & surveys$month_abb == month_abb & surveys$SurveyID == surveyid)]

if(!iteration %in% surveys$iteration){
  objectives <- readline(prompt = cat(paste("Please type survey objectives [click here & type ie. Line-transect surveys & hit Enter]    \n\n"), sep = " "))
  
  i <- nrow(surveys)+1
  levels(surveys$SurveyID) <- c(unique(surveys$SurveyID), surveyid)
  surveys[i,] <- c(year,month_abb,iteration,paste0(surveyid),"Line-transect surveys")
  # surveys[i,] <- c(year,month_abb,iteration,paste0(surveyid),objectives, vessel)
  
  rds <- file.path(dir, main.dir, "surveys.rds")
  saveRDS(surveys, rds)
  # saveRDS(surveys,
  #         file.path("C:/users/keppele/documents/github/cemore/tech_report/data", "surveys.rds"))
  csv <- file.path(dir,main.dir, "surveys.csv")
  write.csv(surveys, csv, row.names=F)
}

# fix surveyid col, and add vessel column
# surveys$SurveyID <- gsub("_VE", "", surveys$SurveyID)
# surveys$vessel <- substr(surveys$SurveyID, 16, 17)
# surveys[which(surveys$vessel==""),]$vessel <- "MB"

# Add other vessel surveys
# surveyid = paste0(data.source,"_", 2022, "mar")#,"_",vessel)
# mar2022 <- c(2022,"Mar","TA2022-03",paste0(surveyid),"Line-transect surveys", "TA")
# surveyid = paste0(data.source,"_", 2023, "jun")#,"_",vessel)
# jun2023 <- c(2023,"Jun","TI2023-06",paste0(surveyid),"Line-transect surveys; photo-identification and genetic sampling of humpback whales", "TI")
# surveyid = paste0(data.source,"_", 20223, "oct")#,"_",vessel)
# oct2023 <- c(2023,"Oct","TI2023-10",paste0(surveyid),"Line-transect surveys; deployment and recovery of acoustic recorders; photo-identification and genetic sampling of humpback whales", "TI")
# 
# surveys <- rbind(surveys[1:18,], mar2022, surveys[19:28,], jun2023, surveys[29:30,], oct2023, surveys[31,])
# 
# lev<- c(unique(surveys$SurveyID))
# surveys$SurveyID <- factor(surveys$SurveyID, levels=lev)



## Remove any unwanted lines (ie. from testing)
# surveys <- surveys[1:28,]
# saveRDS(surveys, rds)
# csv <- file.path(dir, "survey_data/surveys.csv")
# write.csv(surveys, csv, row.names=F)
