
source("R/0 Setup.R")
# cemore <- "C:/Users/keppele/Documents/GitHub/cemore/cemore"
# devtools::load_all(cemore)

# load sightings data
all_ap_sf <- load_sightings(single=F, month=10,year=2022)
all_ap_sf %<>% filter(date<"2022-11-30")
# add groupings/modifications to covariates
all_ap_sf <- all_ap_sf %>% 
  rename(GroupSize=Group_Size,Glare90=glare) %>%
  mutate(Beaufort = as.numeric(as.character(Beaufort)),
         Clumped_Beaufort = case_when(
           Beaufort <2 ~ "0-1",
           Beaufort == 2 ~ "2",
           Beaufort > 2 ~ "3+"),
         Clumped_Beaufort2 = case_when(
           Beaufort <2 ~ "0-1",
           Beaufort > 1 ~ "2+"),
         Clumped_Group_Size = factor(
           dplyr::case_when(
             GroupSize==1 ~ "1",
             GroupSize==2 ~ "2",
             GroupSize==3 ~ "3",
             GroupSize>3 ~ "4+"
           ), levels=  c("1","2","3","4+")),
         Clumped_Group_Size2 = factor(
           dplyr::case_when(
             GroupSize==1 ~ "1",
             GroupSize==2 ~ "2",
             GroupSize>2 ~ "3+"
           ), levels=  c("1","2","3+")),
         # swell=factor(case_when(
         #   swell == "Big >2" ~ "Mod-Big",
         #   swell == "Moderate 1-2 m" ~ "Mod-Big",
         #   swell == "Low <1 m" ~ "Low",
         #   swell == "No swell" ~ "None"),
         #   levels= c("None", "Low", "Mod-Big")),
         swell=factor(case_when(
           swell %in% c("Big >2", "Moderate 1-2 m", "Low <1 m") ~ "Swell",
           swell == "No swell" ~ "No swell"),
           levels= c("No swell", "Swell")),
         Visibility = case_when(
           Visibility == "G&E"~ "G&E",
           Visibility %in% c("Moderate", "F") ~ "Moderate",
           Visibility == "P" ~"P"),
         Clumped_Vis = case_when(
           Visibility == "G&E"~ "G/E",
           !Visibility == "G&E"~ "M/P"),
         l_glare = ifelse(Glare90 == "None", NA, l_glare) %>% as.numeric(),
         r_glare = ifelse(Glare90 == "None", NA, r_glare) %>% as.numeric(),
         Glare90y = ifelse(!Glare90=="None","y","n"),
         Glare45 = ifelse(l_glare %in% -45:45 | r_glare %in% -45:45, Glare90, "None"),
         Glare45y = ifelse(Glare45=="None","n","y")
  ) %>% filter(Reticle<7.1, Vessel == "MB")

sp = "Dall's porpoise"
df <- all_ap_sf %>% filter(Species %like% sp) %>% data.frame()
df <- dp # from dp_oct_feb.Rmd

dp.75 <- dp %>% filter(distance<=0.75)
dp.8  <- dp %>% filter(distance<=0.8)
dp.85 <- dp %>% filter(distance<=0.85)
# par(mfrow=c(1,2))
terra::hist(df$distance, xlab="Distance (km)", breaks=seq(0,max(df$distance),len=25),
            main=paste0(sp, " to ", survey_title, "-30bins"))#, xlim = range(c(0,4)))  
terra::hist(df$distance, xlab="Distance (km)",breaks=seq(0,max(df$distance),len=60),
            main=paste0(sp, " to ", survey_title, "-60bins"))#, xlim = range(c(0,4)))

# No apparent evasive behaviour (fewer detections around 0 distance) or heaping. 

# Examine reticle vs. eyeballed sighting distances to check for effects of eyeballing distance (eg. heaping/rounding)
ret <- df %>% filter(!is.na(Reticle))
nret <- nrow(ret)
# par(mfrow=c(1,2))
hist(ret$distance, xlab="Distance (km)",breaks=seq(0,max(ret$distance),len=30),
     main=paste0("To ", survey_title, " - 30bins - ret only"))
# Will leave non-reticle sightings in.
 

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Detection Functions
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Key only models

dp.75.hn       <-ds(df, key = "hn", truncation = 0.75)  
dp.75.hr       <-ds(df, key = "hr", truncation = 0.75)  
dp.75.un       <-ds(df, key = "un", truncation = 0.75)  

dp.8.hn       <-ds(df, key = "hn", truncation = 0.8)  
dp.8.hr       <-ds(df, key = "hr", truncation = 0.8)  
dp.8.un       <-ds(df, key = "un", truncation = 0.8)  

dp.85.hn       <-ds(df, key = "hn", truncation = 0.85)  
dp.85.hr       <-ds(df, key = "hr", truncation = 0.85)  
dp.85.un       <-ds(df, key = "un", truncation = 0.85)  

# Create summary of single covariate detection function models at 1.5 km truncation.
summarize_ds_models(dp.75.hn     ,
                    dp.75.hr     ,
                    dp.75.un ,
                    output="plain") 
plot_df(sp, dp.75.un)

summarize_ds_models(dp.8.hn     ,
                    dp.8.hr     ,
                    dp.8.un ,
                    output="plain") 
plot_df(sp, dp.8.un)

summarize_ds_models(dp.85.hn     ,
                    dp.85.hr     ,
                    dp.85.un ,
                    output="plain") 
plot_df(sp, dp.85.un)

table(dp.8$Beaufort)
table(dp.8$cBeaufort)
table(dp.8$size)
table(dp.8$cGrSz)
table(dp.8$cGrSz2)
table(dp.8$Glare90)
table(dp.8$Glare90y)
table(dp.8$Glare45)
table(dp.8$Glare45y)
table(dp.8$swell)
table(dp.8$Observer)
table(dp.8$Visibility)
table(dp.8$cVis)


# Test single-covariates with half-normal - "#" = not enough samples in each category
dp0.75.hn.bfc  <-ds(df, 0.75, key="hn", formula=~cBeaufort)  
dp0.75.hn.szc  <-ds(df, 0.75, key="hn", formula=~cGrSz)
dp0.75.hn.szc2 <-ds(df, 0.75, key="hn", formula=~cGrSz2)
dp0.75.hn.g90  <-ds(df, 0.75, key="hn", formula=~Glare90)            
dp0.75.hn.g90y <-ds(df, 0.75, key="hn", formula=~Glare90y)                
dp0.75.hn.g45y <-ds(df, 0.75, key="hn", formula=~Glare45y)           
dp0.75.hn.sw   <-ds(df, 0.75, key="hn", formula=~swell)              
dp0.75.hn.ob   <-ds(df, 0.75, key="hn", formula=~Observer)       

# Test single-covariates with hazard rate             
dp0.75.hr.bfc  <-ds(df, 0.75, key="hr", formula=~cBeaufort)  
dp0.75.hr.szc  <-ds(df, 0.75, key="hr", formula=~cGrSz)
dp0.75.hr.szc2 <-ds(df, 0.75, key="hr", formula=~cGrSz2)
dp0.75.hr.g90  <-ds(df, 0.75, key="hr", formula=~Glare90)            
dp0.75.hr.g90y <-ds(df, 0.75, key="hr", formula=~Glare90y)                
dp0.75.hr.g45y <-ds(df, 0.75, key="hr", formula=~Glare45y)           
dp0.75.hr.sw   <-ds(df, 0.75, key="hr", formula=~swell)              
dp0.75.hr.ob   <-ds(df, 0.75, key="hr", formula=~Observer)     

# Create summary of single covariate detection function models at 1.5 km truncation.
summarize_ds_models(dp0.75.hn.bfc ,
                    dp0.75.hn.szc ,
                    dp0.75.hn.szc2,
                    dp0.75.hn.g90 ,
                    dp0.75.hn.g90y,
                    dp0.75.hn.g45y,
                    dp0.75.hn.sw  ,
                    dp0.75.hn.ob  ,
                    
                    dp0.75.hr.bfc ,
                    dp0.75.hr.szc ,
                    dp0.75.hr.szc2,
                    dp0.75.hr.g90 ,
                    dp0.75.hr.g90y,
                    dp0.75.hr.g45y,
                    dp0.75.hr.sw  ,
                    dp0.75.hr.ob  ,
                    
                    dp0.75.un,
                    dp0.75.hn     ,
                    dp0.75.hr     ,
                    output="plain")
 
 #------- 0.8 -----------
 # Test single-covariates with half-normal
dp0.8.hn.bfc  <-ds(df, 0.8, key="hn", formula=~cBeaufort)  
dp0.8.hn.szc  <-ds(df, 0.8, key="hn", formula=~cGrSz)
dp0.8.hn.szc2 <-ds(df, 0.8, key="hn", formula=~cGrSz2)
dp0.8.hn.g90  <-ds(df, 0.8, key="hn", formula=~Glare90)            
dp0.8.hn.g90y <-ds(df, 0.8, key="hn", formula=~Glare90y)                
dp0.8.hn.g45y <-ds(df, 0.8, key="hn", formula=~Glare45y)           
dp0.8.hn.sw   <-ds(df, 0.8, key="hn", formula=~swell)              
dp0.8.hn.ob   <-ds(df, 0.8, key="hn", formula=~Observer)       

# Test single-covariates with hazard rate             
dp0.8.hr.bfc  <-ds(df, 0.8, key="hr", formula=~cBeaufort)  
dp0.8.hr.szc  <-ds(df, 0.8, key="hr", formula=~cGrSz)
dp0.8.hr.szc2 <-ds(df, 0.8, key="hr", formula=~cGrSz2)
dp0.8.hr.g90  <-ds(df, 0.8, key="hr", formula=~Glare90)            
dp0.8.hr.g90y <-ds(df, 0.8, key="hr", formula=~Glare90y)                
dp0.8.hr.g45y <-ds(df, 0.8, key="hr", formula=~Glare45y)           
dp0.8.hr.sw   <-ds(df, 0.8, key="hr", formula=~swell)              
dp0.8.hr.ob   <-ds(df, 0.8, key="hr", formula=~Observer)     

 
 # Create summary of single covariate detection function models at 1.5 km truncation.
 summarize_ds_models(dp0.8.hn.bfc ,
                     dp0.8.hn.szc ,
                     dp0.8.hn.szc2,
                     dp0.8.hn.g90 ,
                     dp0.8.hn.g90y,
                     dp0.8.hn.g45y,
                     dp0.8.hn.sw  ,
                     dp0.8.hn.ob  ,
                     
                     # dp0.8.hr.bfc ,
                     # dp0.8.hr.szc ,
                     # dp0.8.hr.szc2,
                     # dp0.8.hr.g90 ,
                     # dp0.8.hr.g90y,
                     # dp0.8.hr.g45y,
                     # dp0.8.hr.sw  ,
                     # dp0.8.hr.ob  ,
                     # 
                     dp0.8.un,
                     dp0.8.hn     ,
                     dp0.8.hr     ,
                     output="plain") 
 
 #--------- 0.85 --------------
 # Test single-covariates with half-normal
 dp0.85.hn.bfc  <-ds(df, 0.85, key="hn", formula=~cBeaufort)  
 dp0.85.hn.szc  <-ds(df, 0.85, key="hn", formula=~cGrSz)
 dp0.85.hn.szc2 <-ds(df, 0.85, key="hn", formula=~cGrSz2)
 dp0.85.hn.g90  <-ds(df, 0.85, key="hn", formula=~Glare90)            
 dp0.85.hn.g90y <-ds(df, 0.85, key="hn", formula=~Glare90y)                
 dp0.85.hn.g45y <-ds(df, 0.85, key="hn", formula=~Glare45y)           
 dp0.85.hn.sw   <-ds(df, 0.85, key="hn", formula=~swell)              
 dp0.85.hn.ob   <-ds(df, 0.85, key="hn", formula=~Observer)       
 
 # Test single-covariates with hazard rate             
 dp0.85.hr.bfc  <-ds(df, 0.85, key="hr", formula=~cBeaufort)  
 dp0.85.hr.szc  <-ds(df, 0.85, key="hr", formula=~cGrSz)
 dp0.85.hr.szc2 <-ds(df, 0.85, key="hr", formula=~cGrSz2)
 dp0.85.hr.g90  <-ds(df, 0.85, key="hr", formula=~Glare90)            
 dp0.85.hr.g90y <-ds(df, 0.85, key="hr", formula=~Glare90y)                
 dp0.85.hr.g45y <-ds(df, 0.85, key="hr", formula=~Glare45y)           
 dp0.85.hr.sw   <-ds(df, 0.85, key="hr", formula=~swell)              
 dp0.85.hr.ob   <-ds(df, 0.85, key="hr", formula=~Observer)     
 
 # Create summary of single covariate detection function models at 1.5 km truncation.
 summarize_ds_models(dp0.85.hn.bfc ,
                     dp0.85.hn.szc ,
                     dp0.85.hn.szc2,
                     dp0.85.hn.g90 ,
                     dp0.85.hn.g90y,
                     dp0.85.hn.g45y,
                     dp0.85.hn.sw  ,
                     dp0.85.hn.ob  ,
                     
                     dp0.85.hr.bfc ,
                     dp0.85.hr.szc ,
                     dp0.85.hr.szc2,
                     dp0.85.hr.g90 ,
                     dp0.85.hr.g90y,
                     dp0.85.hr.g45y,
                     dp0.85.hr.sw  ,
                     dp0.85.hr.ob  ,
                     
                     dp0.85.un,
                     dp0.85.hn     ,
                     dp0.85.hr     ,
                     output="plain")
 
 plot_df(sp, dp0.75.hn.sw)
 plot_df(sp,  dp0.8.hn.sw)
 plot_df(sp, dp0.85.hn.sw)
 
 plot_df(sp, dp0.75.un)
 plot_df(sp,  dp0.8.un)
 plot_df(sp, dp0.85.un)
 
#---------------------------------------------------------------------
# Multiple covariates - forward selection
#---------------------------------------------------------------------
 
dp0.8.hn.sw <- ds(df, 0.8, key="hn", formula=~swell)  # -81.964

# dp0.8.hn.sw.g90y       <- ds(df, 0.8, key="hn", formula=~swell + Glare90y)              # -84.069
dp0.8.hn.sw.g90       <- ds(df, 0.8, key="hn", formula=~swell + Glare90)                 # -85.068 **
# dp0.8.hn.sw.g45y       <- ds(df, 0.8, key="hn", formula=~swell + Glare45y)              # -81.837
dp0.8.hn.sw.bfc         <- ds(df, 0.8, key="hn", formula=~swell + cBeaufort)               # -78.142
dp0.8.hn.sw.szc        <- ds(df, 0.8, key="hn", formula=~swell + cGrSz)                   # -78.256


dp0.8.hn.sw.g90.szc     <- ds(df, 0.8, key="hn", formula=~swell + Glare90 + cGrSz)       #  -81.19
dp0.8.hn.sw.g90.bfc    <- ds(df, 0.8, key="hn", formula=~swell + Glare90 + cBeaufort )    #  -81.786


summarize_ds_models(dp0.8.un,
                    dp0.8.hn     ,
                    dp0.8.hn.sw,
                    dp0.8.hn.sw.g90   ,
                    dp0.8.hn.sw.bfc    ,
                    dp0.8.hn.sw.szc    ,
                    dp0.8.hn.sw.g90.szc,
                    dp0.8.hn.sw.g90.bfc,
                    output="plain")

 #---------------------------------------------------------------------
 # Multiple covariates
 #---------------------------------------------------------------------

# Fit global half-normal model:
dp.hr.global<-ds(df, truncation=0.75, key="hr", formula=~Glare45+Clumped_Group_Size+Beaufort+swell+Clumped_Vis) # AIC = -94.96 best yet
plot(hw.hr.global)
# Try dropping terms from global model:
# Drop Clumped_Vis:
dp.hr.1<-ds(df, truncation=0.75, key="hr", formula=~Glare45+Clumped_Group_Size+Beaufort+swell)                  # -96.628 better
# Drop swell                  
dp.hr.2<-ds(df, truncation=0.75, key="hr", formula=~Glare45+Clumped_Group_Size+Beaufort)                        # -99.812 better
# Drop Beaufort                         
dp.hr.3<-ds(df, truncation=0.75, key="hr", formula=~Glare45+Clumped_Group_Size)                                 # -101.808 *****BEST***** 
# Drop Clumped_Group_Size                   
dp.hr.4<-ds(df, truncation=0.75, key="hr", formula=~Glare45)                                                    # -97.041 (worse)

# # Create summary of single covariate detection function models
model.table0.75 <- summarize_ds_models(dp.hr.global,
                                       dp.hr.1,
                                       dp.hr.2,
                                       dp.hr.3,
                                       dp.hr.4,
                                       output="plain")
# Model Key function                                                        Formula C-vM $p$-value Average detectability se(Average detectability) Delta AIC
# 4      dp.hr.3  Hazard-rate                                  ~Glare45 + Clumped_Group_Size      0.8645883             0.1172817                 0.2162970  0.000000
# 3      dp.hr.2  Hazard-rate                       ~Glare45 + Clumped_Group_Size + Beaufort      0.8664919             0.1168715                 0.2258262  1.996162
# 5      dp.hr.4  Hazard-rate                                                       ~Glare45      0.5121923             0.1730263                 0.1244270  4.766560
# 2      dp.hr.1  Hazard-rate               ~Glare45 + Clumped_Group_Size + Beaufort + swell      0.8838464             0.1162779                 0.2145622  5.179652
# 1 dp.hr.global  Hazard-rate ~Glare45 + Clumped_Group_Size + Beaufort + swell + Clumped_Vis      0.8940064             0.1206259                 0.2109310  6.847617

# CONCLUSION: At 0.75 km truncation, the uniform model with cos(1) adjustment is the best key model fit.
# However, addition of covariates led to best performing model being hr with glare to 45 degrees and clumped group size. 

