source("R/0 Setup.R")
{ year <- 2022
  month <- "10"
  iteration <- "24"
  month_abb <- month.abb[as.numeric(month)]
  survey_title <- paste(first_up(month_abb), year)
  surveyid = paste0("cemore_", year, tolower(month_abb))
  # rds <- file.path(dir, "survey_data/surveys.rds")
}

all_ap_sf <- readRDS(paste0("output/all_sgt/all_effort_sgt_to_", year, "_", month, ".rds")) %>% 
  data.frame() #%>% filter(date<date("2022-03-01") )

sightings <- all_ap_sf %>% 
  rename(size=Group_Size,Glare90=glare) %>%
  mutate(Beaufort = as.numeric(as.character(Beaufort)),
         cBeaufort = case_when(
           Beaufort <2 ~ "0-1",
           Beaufort == 2 ~ "2",
           Beaufort > 2 ~ "3+"),
         cBeaufort2 = case_when(
           Beaufort == 0 ~ "0",
           Beaufort == 1 ~ "1",
           Beaufort == 2 ~ "2",
           Beaufort > 2 ~ "3+"),
         cGrSz2 = factor(
           dplyr::case_when(
             size==1 ~ "1",
             size==2 ~ "2",
             size==3 ~ "3",
             size>3 ~ "4+"
           ), levels=  c("1","2","3","4+")),
         Observer = case_when(
           Observer == "CMcMillan" ~ "a",
           Observer == "EKeppel" ~"b",
           Observer == "LSpaven" ~"c"),
         cGrSz = factor(
           dplyr::case_when(
             size==1 ~ "1",
             size==2 ~ "2",
             size>2 ~ "3+"
           ), levels=  c("1","2","3+")),
         swell=factor(case_when(
           swell %in% c("Big >2", "Moderate 1-2 m", "Low <1 m") ~ "Swell",
           swell == "No swell" ~ "No swell"),
           levels= c("No swell", "Swell")),
         Visibility = case_when(
           Visibility == "G&E"~ "G&E",
           Visibility %in% c("Moderate", "F") ~ "Moderate",
           Visibility == "P" ~"P"),
         cVis = case_when(
           Visibility == "G&E"~ "G/E",
           !Visibility == "G&E"~ "M/P"),
         l_glare = ifelse(Glare90 == "None", NA, l_glare) %>% as.numeric(),
         r_glare = ifelse(Glare90 == "None", NA, r_glare) %>% as.numeric(),
         Glare90y = ifelse(!Glare90=="None","y","n"),
         Glare45 = ifelse(l_glare %in% -45:45 | r_glare %in% -45:45, Glare90, "None"),
         Glare45y = ifelse(Glare45=="None","n","y")
  ) %>% filter(Vessel == "MB") %>% 
  # full_join(t) %>% 
  mutate(
    Region.Label= season,
    Sample.Label=TransectID,
    object=Sgt_ID) %>% 
  dplyr::select(Region.Label,
                Sample.Label,
                date,
                month_abb,
                Bearing_R,
                Reticle,
                distance,
                Species,
                size,
                cGrSz,
                cGrSz2,
                Visibility,
                cVis,
                Beaufort,
                cBeaufort,
                cBeaufort2,
                Glare45,
                Glare90,
                Glare45y,
                Glare90y,
                swell,
                Observer
  )

srkw <- sightings %>% filter(Species %like% "southern resident")
tkw <- sightings %>% filter(Species %like% "Bigg")
sp <- "killer"
kw <- sightings %>% filter(Species %like% sp)
kw[which(kw$Reticle == 138.90),]$Reticle <- 0.5

terra::hist(srkw$distance, xlab="Distance (km)")
terra::hist(srkw$distance, xlab="Distance (km)", breaks = seq(0, max(srkw$distance),len=30))
terra::hist(tkw$distance, xlab="Distance (km)", breaks = seq(0, max(tkw$distance),len=30))

kw2.6un     <-ds(kw, key = "un", truncation = 2.6)
kw2.6hn     <-ds(kw, key = "hn", truncation = 2.6)
kw2.6hr     <-ds(kw, key = "hr", truncation = 2.6)

kw2.6.hn.bfc  <-ds(kw, 2.6, key="hn", formula=~cBeaufort)  
kw2.6.hn.sz   <-ds(kw, 2.6, key="hn", formula=~size)
kw2.6.hn.szc  <-ds(kw, 2.6, key="hn", formula=~cGrSz)
kw2.6.hn.szc2 <-ds(kw, 2.6, key="hn", formula=~cGrSz2)
kw2.6.hn.sw   <-ds(kw, 2.6, key="hn", formula=~swell)              
kw2.6.hn.ob   <-ds(kw, 2.6, key="hn", formula=~Observer)    

kw2.6.hr.bfc  <-ds(kw, 2.6, key="hr", formula=~cBeaufort)  
kw2.6.hr.sz   <-ds(kw, 2.6, key="hr", formula=~size)
kw2.6.hr.szc  <-ds(kw, 2.6, key="hr", formula=~cGrSz)
# kw2.6.hr.szc2 <-ds(kw, 2.6, key="hr", formula=~cGrSz2)
kw2.6.hr.sw   <-ds(kw, 2.6, key="hr", formula=~swell)              
kw2.6.hr.ob   <-ds(kw, 2.6, key="hr", formula=~Observer)  

m <- summarize_ds_models(
  kw2.6un ,
  kw2.6hn ,
  kw2.6hr ,
  kw2.6.hn.bfc ,
  kw2.6.hn.szc ,
  kw2.6.hn.szc ,
  kw2.6.hn.szc2,
  kw2.6.hn.sw  ,
  kw2.6.hn.ob  ,
  kw2.6.hr.bfc ,
  kw2.6.hr.sz ,
  kw2.6.hr.szc ,
  # kw2.6.hr.szc2,
  kw2.6.hr.sw  ,
  kw2.6.hr.ob  ,
                    output="plain") 
m

plot_df(sp, kw2.6un, bins = 15)
plot_df(sp, kw2.6hn, bins = 15)
plot_df(sp, kw2.6hr, bins = 15)
