# rerrdap
install.packages("rerddap")
library(rerddap)
install.packages("rerddapXtracto")
library(rerddapXtracto)
help(rerddapXtracto)

k <- 0
survey_data <- list()
iterations %<>% filter(!iteration==99)
for(i in unique(iterations$year)){
  # year <- iterations[[i]]$year
  for(j in unique(iterations %>% filter(year==i))$month){
    # month_abb <- j
    k <- k+1
    survey_data[[k]] <- read.table(file.path("survey_data/tidy_data",i,tolower(j),paste0("cemore_",i,tolower(j),"_dataSurveyID.txt")))
  }
}
y <- suppressMessages(survey_data %>% reduce(full_join))
