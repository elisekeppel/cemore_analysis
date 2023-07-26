
# TRY THIS (ADD SURVEYS BEYOND FEB 2022 AS NECESSARY)
source("R/0 Setup.R")
# comment out source("R/0 Setup.R") in '1 Data processing.R'
# comment out year/month in '1 Data processing.R' & 0 Setup.R

# NOTE : there will be pop-ups that need to be responded to with 'no'

year=2020
for(m in (c("Sep","Oct","Nov"))){
    month_abb = m
    month <- match(m,month.abb)
    if(nchar(month)==1) month <- paste0("0",month)
    surveyid = paste0("cemore_", year, tolower(month_abb))
    survey_title <- paste(first_up(month_abb), year)
    iteration <- surveys$iteration[which(surveys$year == year & surveys$month_abb == month_abb)]
    source("R/1 Data processing.R")
    #-------------------------------------------------------------------
    # ----------------------- SAVE INDIV SURVEYS --------------------
    #-------------------------------------------------------------------
    
    # single <- T
    # effort <- load_effort(year, month, single)
    # effort_lines <- get_effort_lines(effort)
    # ap_sf <- load_sightings(year, month, single)
    # 
    # saveRDS(effort, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/effort_", year, "_",month, ".rds"))
    # saveRDS(effort, paste0("output/effort/effort_", year, "_",month, ".rds"))
    # 
    # saveRDS(effort_lines, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/effort_lines_", year, "_",month, ".rds"))
    # saveRDS(effort_lines, paste0("output/effort_lines/effort_lines_", year, "_",month, ".rds"))
    # 
    # saveRDS(ap_sf, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/effort_sgt_", year, "_",month, ".rds"))
    # saveRDS(ap_sf, paste0("output/sgt/effort_sgt_", year, "_",month, ".rds"))
    
    #-------------------------------------------------------------------
    # ----------------------- SAVE ALL SURVEYS --------------------
    #-------------------------------------------------------------------
    
    # all_effort <- load_effort(year, month, single=F, vessel = "MB")
    # all_effort_lines <- get_effort_lines(all_effort)
    # all_ap_sf <- load_sightings(year, month, single=F, vessel = "MB")
    # 
    # saveRDS(all_effort, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/all_effort_to_", year, "_",month, ".rds"))
    # saveRDS(all_effort, paste0("output/all_effort/all_effort_to_", year, "_",month, ".rds"))
    # 
    # saveRDS(all_effort_lines, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/all_effort_lines_to_", year, "_",month, ".rds"))
    # saveRDS(all_effort_lines, paste0("output/all_lines/all_effort_lines_to_", year, "_",month, ".rds"))
    # 
    # saveRDS(all_ap_sf, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/all_effort_sgt_to_", year, "_",month, ".rds"))
    # saveRDS(all_ap_sf, paste0("output/all_sgt//all_effort_sgt_to_", year, "_",month, ".rds"))
    # 
  }

year=2021
for(m in (c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Oct","Nov","Dec"))){
  month_abb = m
  month <- match(m,month.abb)
  if(nchar(month)==1) month <- paste0("0",month)
  surveyid = paste0("cemore_", year, tolower(month_abb))
  survey_title <- paste(first_up(month_abb), year)
  iteration <- surveys$iteration[which(surveys$year == year & surveys$month_abb == month_abb)]
  source("R/1 Data processing.R")
  
  #-------------------------------------------------------------------
  # ----------------------- SAVE INDIV SURVEYS --------------------
  #-------------------------------------------------------------------
  # single <- T
  # effort <- load_effort(year, month, single)
  # effort_lines <- get_effort_lines(effort)
  # ap_sf <- load_sightings(year, month, single)
  # 
  # saveRDS(effort, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/effort_", year, "_",month, ".rds"))
  # saveRDS(effort, paste0("output/effort/effort_", year, "_",month, ".rds"))
  # 
  # saveRDS(effort_lines, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/effort_lines_", year, "_",month, ".rds"))
  # saveRDS(effort_lines, paste0("output/effort_lines/effort_lines_", year, "_",month, ".rds"))
  # 
  # saveRDS(ap_sf, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/effort_sgt_", year, "_",month, ".rds"))
  # saveRDS(ap_sf, paste0("output/sgt/effort_sgt_", year, "_",month, ".rds"))
  # 
  
}

year=2022
for(m in (c("Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Dec"))){
  month_abb = m
  month <- match(m,month.abb)
  if(nchar(month)==1) month <- paste0("0",month)
  surveyid = paste0("cemore_", year, tolower(month_abb))
  iteration <- surveys$iteration[which(surveys$year == year & surveys$month_abb == month_abb)]
  source("R/1 Data processing.R")
  #-------------------------------------------------------------------
  # ----------------------- SAVE INDIV SURVEYS --------------------
  #-------------------------------------------------------------------
  
  # single <- T
  # effort <- load_effort(year, month, single)
  # effort_lines <- get_effort_lines(effort)
  # ap_sf <- load_sightings(year, month, single)
  # 
  # saveRDS(effort, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/effort_", year, "_",month, ".rds"))
  # saveRDS(effort, paste0("output/effort/effort_", year, "_",month, ".rds"))
  # 
  # saveRDS(effort_lines, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/effort_lines_", year, "_",month, ".rds"))
  # saveRDS(effort_lines, paste0("output/effort_lines/effort_lines_", year, "_",month, ".rds"))
  # 
  # saveRDS(ap_sf, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/effort_sgt_", year, "_",month, ".rds"))
  # saveRDS(ap_sf, paste0("output/sgt/effort_sgt_", year, "_",month, ".rds"))
  
}

year=2023
for(m in (c("Jan","Feb","May"))){
  month_abb = m
  month <- match(m,month.abb)
  if(nchar(month)==1) month <- paste0("0",month)
  surveyid = paste0("cemore_", year, tolower(month_abb))
  iteration <- surveys$iteration[which(surveys$year == year & surveys$month_abb == month_abb)]
  source("R/1 Data processing.R")
  #-------------------------------------------------------------------
  # ----------------------- SAVE INDIV SURVEYS --------------------
  #-------------------------------------------------------------------
  
  # single <- T
  # effort <- load_effort(year, month, single)
  # effort_lines <- get_effort_lines(effort)
  # ap_sf <- load_sightings(year, month, single)
  # 
  # saveRDS(effort, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/effort_", year, "_",month, ".rds"))
  # saveRDS(effort, paste0("output/effort/effort_", year, "_",month, ".rds"))
  # 
  # saveRDS(effort_lines, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/effort_lines_", year, "_",month, ".rds"))
  # saveRDS(effort_lines, paste0("output/effort_lines/effort_lines_", year, "_",month, ".rds"))
  # 
  # saveRDS(ap_sf, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/effort_sgt_", year, "_",month, ".rds"))
  # saveRDS(ap_sf, paste0("output/sgt/effort_sgt_", year, "_",month, ".rds"))
  
}

#-------------------------------------------------------------------
# ----------------------- SAVE ALL SURVEYS --------------------
#-------------------------------------------------------------------
#-------------------------------------------------------------------
# ----------------------- SAVE ALL SURVEYS --------------------
#-------------------------------------------------------------------

all_effort <- load_effort(year, month, single=F, vessel = "MB")
all_effort_lines <- get_effort_lines(all_effort)
all_ap_sf <- load_sightings(year, month, single=F, vessel = "MB")

# saveRDS(all_effort, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/all_effort_to_", year, "_",month, ".rds"))
saveRDS(all_effort, paste0("output_",data.source,"/all_effort/all_effort_to_", year, "_",month, ".rds"))

# saveRDS(all_effort_lines, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/all_effort_lines_to_", year, "_",month, ".rds"))
saveRDS(all_effort_lines, paste0("output_",data.source,"/all_lines/all_effort_lines_to_", year, "_",month, ".rds"))

# saveRDS(all_ap_sf, paste0("C:/users/keppele/documents/github/cemore/tech_report/output/all_effort_sgt_to_", year, "_",month, ".rds"))
saveRDS(all_ap_sf, paste0("output_",data.source,"/all_sgt//all_effort_sgt_to_", year, "_",month, ".rds"))



#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------
# 2022 Jan
#-----------------------------------------------------------------------------------------------
{
  source("R/0 Setup.R")
  
  year = 2022
  month = "01"
  month_abb <- month.abb[as.numeric(month)]
  surveyid = paste0("cemore_", year, tolower(month_abb))
  
  options(java.parameters="-Xmx12000m")
  path<-getwd()#dirname(rstudioapi::getActiveDocumentContext()$path)
  data_path <- file.path("survey_data","tidy_data", year, tolower(month_abb))
  if(grep("/",path)){
    u <- "/"
  } else {
  }
  system.time(source(paste(cemore,u,"data_processing_scripts",u,"CEMORE_DATA_PROCESSING.R",sep="")))
}
#-----------------------------------------------------------------------------------------------
# 2021 Dec
#-----------------------------------------------------------------------------------------------
{
  source("R/0 Setup.R")
  year = 2021
  month = "12"
  month_abb <- month.abb[as.numeric(month)]
  surveyid = paste0("cemore_", year, tolower(month_abb))
  
  options(java.parameters="-Xmx12000m")
  path<-getwd()#dirname(rstudioapi::getActiveDocumentContext()$path)
  data_path <- file.path("survey_data","tidy_data", year, tolower(month_abb))
  if(grep("/",path)){
    u <- "/"
  } else {
  }
  system.time(source(paste(cemore,u,"data_processing_scripts",u,"CEMORE_DATA_PROCESSING.R",sep="")))
}
#-----------------------------------------------------------------------------------------------
# 2021 Nov
#-----------------------------------------------------------------------------------------------
{
  source("R/0 Setup.R")
  
  year = 2021
  month = "11"
  month_abb <- month.abb[as.numeric(month)]
  surveyid = paste0("cemore_", year, tolower(month_abb))
  
  options(java.parameters="-Xmx12000m")
  path<-getwd()#dirname(rstudioapi::getActiveDocumentContext()$path)
  data_path <- file.path("survey_data","tidy_data", year, tolower(month_abb))
  if(grep("/",path)){
    u <- "/"
  } else {
  }
  system.time(source(paste(cemore,u,"data_processing_scripts",u,"CEMORE_DATA_PROCESSING.R",sep="")))
}
#-----------------------------------------------------------------------------------------------
# 2021 Oct
#-----------------------------------------------------------------------------------------------
{
  source("R/0 Setup.R")
  
year = 2021
month = "10"
month_abb <- month.abb[as.numeric(month)]
surveyid = paste0("cemore_", year, tolower(month_abb))

options(java.parameters="-Xmx12000m")
path<-getwd()#dirname(rstudioapi::getActiveDocumentContext()$path)
data_path <- file.path("survey_data","tidy_data", year, tolower(month_abb))
if(grep("/",path)){
  u <- "/"
} else {
}
system.time(source(paste(cemore,u,"data_processing_scripts",u,"CEMORE_DATA_PROCESSING.R",sep="")))
}
  #-----------------------------------------------------------------------------------------------
  # 2021 Aug
  #-----------------------------------------------------------------------------------------------

{
  source("R/0 Setup.R")
  
  year = 2021
  month = "08"
  month_abb <- month.abb[as.numeric(month)]
  surveyid = paste0("cemore_", year, tolower(month_abb))

  options(java.parameters="-Xmx12000m")
  path<-getwd()#dirname(rstudioapi::getActiveDocumentContext()$path)
  data_path <- file.path("survey_data","tidy_data", year, tolower(month_abb))
  if(grep("/",path)){
    u <- "/"
  } else {
  }
  system.time(source(paste(cemore,u,"data_processing_scripts",u,"CEMORE_DATA_PROCESSING.R",sep="")))
}
#-----------------------------------------------------------------------------------------------
# 2021 July
#-----------------------------------------------------------------------------------------------
{
  source("R/0 Setup.R")
  year = 2021
  month = "07"
  month_abb <- month.abb[as.numeric(month)]
  surveyid = paste0("cemore_", year, tolower(month_abb))
  
  options(java.parameters="-Xmx12000m")
  path<-getwd()#dirname(rstudioapi::getActiveDocumentContext()$path)
  data_path <- file.path("survey_data","tidy_data", year, tolower(month_abb))
  if(grep("/",path)){
    u <- "/"
  } else {
  }
  system.time(source(paste(cemore,u,"data_processing_scripts",u,"CEMORE_DATA_PROCESSING.R",sep="")))
}
#-----------------------------------------------------------------------------------------------
# 2021 June
#-----------------------------------------------------------------------------------------------

{
  source("R/0 Setup.R")
  
  year = 2021
  month = "06" # TO DO : FIX ERROR
  month_abb <- month.abb[as.numeric(month)]
  surveyid = paste0("cemore_", year, tolower(month_abb))
  
  options(java.parameters="-Xmx12000m")
  path<-getwd()#dirname(rstudioapi::getActiveDocumentContext()$path)
  data_path <- file.path("survey_data","tidy_data", year, tolower(month_abb))
  if(grep("/",path)){
    u <- "/"
  } else {
  }
  # Note: there are comments that suggest a high density count, but Just watch 
  # the code output notes and follow instructions that it doesn't apply to our data.
  # Type n, enter
  system.time(source(paste(cemore,u,"data_processing_scripts",u,"CEMORE_DATA_PROCESSING.R",sep="")))
}
#-----------------------------------------------------------------------------------------------
# 2021 May
#-----------------------------------------------------------------------------------------------

{
  source("R/0 Setup.R")
  
  year = 2021
  month = "05" # TO DO : FIX ERROR
  month_abb <- month.abb[as.numeric(month)]
  surveyid = paste0("cemore_", year, tolower(month_abb))
  
  options(java.parameters="-Xmx12000m")
  path<-getwd()#dirname(rstudioapi::getActiveDocumentContext()$path)
  data_path <- file.path("survey_data","tidy_data", year, tolower(month_abb))
  if(grep("/",path)){
    u <- "/"
  } else {
  }
  system.time(source(paste(cemore,u,"data_processing_scripts",u,"CEMORE_DATA_PROCESSING.R",sep="")))
}
#-----------------------------------------------------------------------------------------------
# 2021 Apr
#-----------------------------------------------------------------------------------------------
{
  source("R/0 Setup.R")
  
  year = 2021
  month = "04"
  month_abb <- month.abb[as.numeric(month)]
  surveyid = paste0("cemore_", year, tolower(month_abb))
  
  options(java.parameters="-Xmx12000m")
  path<-getwd()#dirname(rstudioapi::getActiveDocumentContext()$path)
  data_path <- file.path("survey_data","tidy_data", year, tolower(month_abb))
  if(grep("/",path)){
    u <- "/"
  } else {
  }
  system.time(source(paste(cemore,u,"data_processing_scripts",u,"CEMORE_DATA_PROCESSING.R",sep="")))
}
#-----------------------------------------------------------------------------------------------
# 2021 Mar
#-----------------------------------------------------------------------------------------------
{
  source("R/0 Setup.R")
  year = 2021
  month = "03"
  month_abb <- month.abb[as.numeric(month)]
  surveyid = paste0("cemore_", year, tolower(month_abb))
  
  options(java.parameters="-Xmx12000m")
  path<-getwd()#dirname(rstudioapi::getActiveDocumentContext()$path)
  data_path <- file.path("survey_data","tidy_data", year, tolower(month_abb))
  if(grep("/",path)){
    u <- "/"
  } else {
  }
  system.time(source(paste(cemore,u,"data_processing_scripts",u,"CEMORE_DATA_PROCESSING.R",sep="")))
}
#-----------------------------------------------------------------------------------------------
# 2021 Feb
#-----------------------------------------------------------------------------------------------
{
  source("R/0 Setup.R")
  
  year = 2021
  month = "02"
  month_abb <- month.abb[as.numeric(month)]
  surveyid = paste0("cemore_", year, tolower(month_abb))
  
  options(java.parameters="-Xmx12000m")
  path<-getwd()#dirname(rstudioapi::getActiveDocumentContext()$path)
  data_path <- file.path("survey_data","tidy_data", year, tolower(month_abb))
  if(grep("/",path)){
    u <- "/"
  } else {
  }
  system.time(source(paste(cemore,u,"data_processing_scripts",u,"CEMORE_DATA_PROCESSING.R",sep="")))
}
#-----------------------------------------------------------------------------------------------
# 2021 Jan
#-----------------------------------------------------------------------------------------------
{
  source("R/0 Setup.R")
  
  year = 2021
  month = "01"
  month_abb <- month.abb[as.numeric(month)]
  surveyid = paste0("cemore_", year, tolower(month_abb))
  
  options(java.parameters="-Xmx12000m")
  path<-getwd()#dirname(rstudioapi::getActiveDocumentContext()$path)
  data_path <- file.path("survey_data","tidy_data", year, tolower(month_abb))
  if(grep("/",path)){
    u <- "/"
  } else {
  }
  system.time(source(paste(cemore,u,"data_processing_scripts",u,"CEMORE_DATA_PROCESSING.R",sep="")))
}
#-----------------------------------------------------------------------------------------------
# 2020 Nov
#-----------------------------------------------------------------------------------------------
{
  source("R/0 Setup.R")
  
  year = 2020
  month = "11"
  month_abb <- month.abb[as.numeric(month)]
  surveyid = paste0("cemore_", year, tolower(month_abb))
  survey_title <- paste(first_up(month_abb), year)
  
  options(java.parameters="-Xmx12000m")
  path<-getwd()#dirname(rstudioapi::getActiveDocumentContext()$path)
  data_path <- file.path("survey_data","tidy_data", year, tolower(month_abb))
  if(grep("/",path)){
    u <- "/"
  } else {
  }
  system.time(source(paste(cemore,u,"data_processing_scripts",u,"CEMORE_DATA_PROCESSING.R",sep="")))
}
#-----------------------------------------------------------------------------------------------
# 2020 Oct
#-----------------------------------------------------------------------------------------------
{
  source("R/0 Setup.R")
  
  year = 2020
  month = "10"
  month_abb <- month.abb[as.numeric(month)]
  surveyid = paste0("cemore_", year, tolower(month_abb))
  
  options(java.parameters="-Xmx12000m")
  path<-getwd()#dirname(rstudioapi::getActiveDocumentContext()$path)
  data_path <- file.path("survey_data","tidy_data", year, tolower(month_abb))
  if(grep("/",path)){
    u <- "/"
  } else {
  }
  system.time(source(paste(cemore,u,"data_processing_scripts",u,"CEMORE_DATA_PROCESSING.R",sep="")))
  }
#-----------------------------------------------------------------------------------------------
# 2020 Sept
#-----------------------------------------------------------------------------------------------
{
  source("R/0 Setup.R")
  year = 2020
  month = "09"
  month_abb <- month.abb[as.numeric(month)]
  surveyid = paste0("cemore_", year, tolower(month_abb))
  
  options(java.parameters="-Xmx12000m")
  path<-getwd()#dirname(rstudioapi::getActiveDocumentContext()$path)
  data_path <- file.path("survey_data","tidy_data", year, tolower(month_abb))
  if(grep("/",path)){
    u <- "/"
  } else {
  }
  system.time(source(paste(cemore,u,"data_processing_scripts",u,"CEMORE_DATA_PROCESSING.R",sep="")))
}

