
source("R/0 Setup.R")
cemore <- "C:/Users/keppele/Documents/GitHub/cemore/cemore"
devtools::load_all(cemore)

# load sightings data
all_ap_sf <- load_sightings(single=F, month=10,year=2022)

# add groupings/modifications to covariates
all_ap_sf <- all_ap_sf %>% 
  rename(GroupSize=Group_Size,Glare90=glare) %>%
  mutate(Beaufort = as.numeric(as.character(Beaufort)),
         Clumped_Beaufort = case_when(
           Beaufort <2 ~ "0-1",
           Beaufort == 2 ~ "2",
           Beaufort > 2 ~ "3+"),
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
         swell=factor(case_when(
           swell == "Big >2" ~ "Mod-Big",
           swell == "Moderate 1-2 m" ~ "Mod-Big",
           swell == "Low <1 m" ~ "Low",
           swell == "No swell" ~ "None"),
           levels= c("None", "Low", "Mod-Big")),
         Clumped_Vis = case_when(
           Visibility == "G&E"~ "G/E",
           !Visibility == "G&E"~ "M/P"),
         l_glare = ifelse(Glare90 == "None", NA, l_glare) %>% as.numeric(),
         r_glare = ifelse(Glare90 == "None", NA, r_glare) %>% as.numeric(),
         Glare90y = ifelse(!Glare90=="None","y","n"),
         Glare90c = ifelse(Glare90=="Severe","Severe","Mild/None"),
         Glare45 = ifelse(l_glare %in% -45:45 | r_glare %in% -45:45, Glare90, "None"),
         Glare45y = ifelse(Glare45=="None","n","y"),
         Glare45c = ifelse(Glare45=="Severe","Severe","Mild/None")
  ) %>% filter(Reticle<7.1)

sp = "harbour porpoise"
df <- all_ap_sf %>% filter(Species == sp) %>% data.frame()

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
# Models with no truncation.

# Truncation at 0.65 appears to line up well with 0.15 x probability of detection (though truncation at 2 km may show a better goodness of fit)
hp.65.hn       <-ds(df, key = "hn", truncation = 0.65, adjustment = NULL)    # -940.24  * best
hp.65.hn.cos   <-ds(df, key = "hn", truncation = 0.65, adjustment = "cos")   # -938.426
hp.65.hr       <-ds(df, key = "hr", truncation = 0.65, adjustment = NULL)    # -929.773
hp.65.hr.herm  <-ds(df, key = "hr", truncation = 0.65, adjustment = "herm")  # -935.683 *
hp.65.un.cos   <-ds(df, key = "un", truncation = 0.65, adjustment = "cos")   # -938.138 *
hp.65.un.poly  <-ds(df, key = "un", truncation = 0.65, adjustment = "poly")  # -937.723

table(df$Beaufort)
table(df$Clumped_Beaufort)
table(df$GroupSize)
table(df$Clumped_Group_Size)
table(df$Glare90)
table(df$Glare90y)
table(df$Glare45)
table(df$Glare45y)
table(df$swell)
table(df$Observer)
table(df$Visibility)
table(df$Clumped_Vis)


# Test single-covariate hazard-rate models with hr
hp.65.hr.bf   <-ds(df, .65, key="hr", formula=~Beaufort)              # AIC = -935.341 *
hp.65.hr.bfc  <-ds(df, .65, key="hr", formula=~cBeaufort)      # AIC = -935.262
hp.65.hr.sz   <-ds(df, .65, key="hr", formula=~GroupSize)             # AIC = -929.12  **
hp.65.hr.szc  <-ds(df, .65, key="hr", formula=~cGrSz)    # AIC = -924.699
hp.65.hr.szc2 <-ds(df, .65, key="hr", formula=~cGrSz2)   # AIC = -926.265
hp.65.hr.g90  <-ds(df, .65, key="hr", formula=~Glare90)               # AIC = -928.807 **
hp.65.hr.g90y <-ds(df, .65, key="hr", formula=~Glare90y)              # AIC = -928.068
hp.65.hr.g45  <-ds(df, .65, key="hr", formula=~Glare45)               # AIC = -928.194
hp.65.hr.g45y <-ds(df, .65, key="hr", formula=~Glare45y)              # AIC = -928.023
hp.65.hr.sw   <-ds(df, .65, key="hr", formula=~swell)                 # AIC = -926.07
hp.65.hr.ob   <-ds(df, .65, key="hr", formula=~Observer)              # AIC = -934.766
hp.65.hr.vis  <-ds(df, .65, key="hr", formula=~Visibility)            # AIC = -927.043
hp.65.hr.visc <-ds(df, .65, key="hr", formula=~cVis)           # AIC = -927.879 **


# Test single-covariate half-normal models
hp.65.hn.bf   <-ds(df, .65, key="hn", formula=~Beaufort)              # AIC = -952.328
hp.65.hn.bfc  <-ds(df, .65, key="hn", formula=~Clumped_Beaufort)      # AIC = -952.799 *
hp.65.hn.sz   <-ds(df, .65, key="hn", formula=~GroupSize)             # AIC = -939.814 **
hp.65.hn.szc  <-ds(df, .65, key="hn", formula=~Clumped_Group_Size)    # AIC = -935.944
hp.65.hn.szc2 <-ds(df, .65, key="hn", formula=~Clumped_Group_Size2)   # AIC = -937.669
hp.65.hn.g90  <-ds(df, .65, key="hn", formula=~Glare90)               # AIC = -938.633 **
hp.65.hn.g90y <-ds(df, .65, key="hn", formula=~Glare90y)              # AIC = -938.627
hp.65.hn.g45  <-ds(df, .65, key="hn", formula=~Glare45)               # AIC = -937.492
hp.65.hn.g45y <-ds(df, .65, key="hn", formula=~Glare45y)              # AIC = -938.314
hp.65.hn.sw   <-ds(df, .65, key="hn", formula=~swell)                 # AIC = -936.344
hp.65.hn.ob   <-ds(df, .65, key="hn", formula=~Observer)              # AIC = -942.738
hp.65.hn.vis  <-ds(df, .65, key="hn", formula=~Visibility)            # AIC = -937.851
hp.65.hn.visc <-ds(df, .65, key="hn", formula=~Clumped_Vis)           # AIC = -938.867 **

# Create summary of single covariate detection function models at 1.5 km truncation.
model.table0.65hn <- summarize_ds_models(hp.65.hn,
                                        hp.65.un.cos,
                                        hp.65.hr.herm ,
                                        hp.65.hn.bfc,
                                        hp.65.hn.ob,
                                        hp.65.hn.visc,
                                        hp.65.hn.g90,
                                        hp.65.hn.sz,
                                        hp.65.hn.sw,
                                      output="plain") 
# Model                                                   Key function           Formula C-vM $p$-value Average detectability se(Average detectability) Delta AIC
# 4  hp.65.hn.bfc                                                    Half-normal ~Clumped_Beaufort      0.1413329             0.5742212                0.01812704   0.00000
# 5   hp.65.hn.ob                                                    Half-normal         ~Observer      0.2057038             0.5792268                0.01715855  10.06171
# 1      hp.65.hn                                                    Half-normal                ~1      0.1644523             0.5815733                0.01723228  12.55958
# 8   hp.65.hn.sz                                                    Half-normal        ~GroupSize      0.1598149             0.5808631                0.01724605  12.98528
# 6 hp.65.hn.visc                                                    Half-normal      ~Clumped_Vis      0.1596366             0.5813295                0.01737686  13.93229
# 7  hp.65.hn.g90                                                    Half-normal          ~Glare90      0.1722476             0.5806561                0.01721899  14.16603
# 2  hp.65.un.cos                 Uniform with cosine adjustment term of order 1              <NA>      0.1491990             0.5769284                0.01283462  14.66137
# 9   hp.65.hn.sw                                                    Half-normal            ~swell      0.1628589             0.5815350                0.01724853  16.45491
# 3 hp.65.hr.herm Hazard-rate with Hermite polynomial adjustment term of order 4                ~1      0.1548149             0.5882001                0.03270327  17.11666

#---------------------------------------------------------------------
# Multiple covariates
#---------------------------------------------------------------------

# Fit global half-normal model:
hp.hn.global<-ds(df, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer+GroupSize+Clumped_Vis+Glare90+swell) # AIC = -947.129 (worse)
plot(hw.hn.global)
# Try dropping terms from global model:
# Drop swell:
hp.hn.1<-ds(df, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer+GroupSize+Clumped_Vis+Glare90)            # -950.877 better, but still worse than clumped bf
# Drop Beaufort         
hp.hn.2<-ds(df, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer+GroupSize+Clumped_Vis)                    # -953.087 better
# Drop GroupSize                
hp.hn.3<-ds(df, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer+GroupSize)                                # -954.983 better
# Keep GroupSize, drop                
hp.hn.4<-ds(df, truncation=0.65, key="hn", formula=~Clumped_Beaufort+Observer)                                          # -955.725 *****BEST*****
# Drop visibility                                         
hp.hn.5<-ds(df, truncation=0.65, key="hn", formula=~Clumped_Beaufort)                                                   # -952.799 (worse)


# Dropping terms did not improve the model, conclude that global model is the best
# # Create summary of single covariate detection function models
model.table0.65 <- summarize_ds_models(hp.hn.global,
                                       hp.hn.1,
                                       hp.hn.2,
                                       hp.hn.3,
                                       hp.hn.4,
                                       hp.hn.5,
                                       output="plain")
# Model Key function                                                                  Formula C-vM $p$-value Average detectability se(Average detectability) Delta AIC
# 5      hp.hn.4  Half-normal                                             ~Clumped_Beaufort + Observer      0.1644933             0.5714232                0.01817483 0.0000000
# 4      hp.hn.3  Half-normal                                 ~Clumped_Beaufort + Observer + GroupSize      0.1592268             0.5708095                0.01817078 0.7421995
# 3      hp.hn.2  Half-normal                   ~Clumped_Beaufort + Observer + GroupSize + Clumped_Vis      0.1578506             0.5707186                0.01813749 2.6377449
# 6      hp.hn.5  Half-normal                                                        ~Clumped_Beaufort      0.1413329             0.5742212                0.01812704 2.9258554
# 2      hp.hn.1  Half-normal         ~Clumped_Beaufort + Observer + GroupSize + Clumped_Vis + Glare90      0.1525790             0.5699886                0.01826320 4.8477331
# 1 hp.hn.global  Half-normal ~Clumped_Beaufort + Observer + GroupSize + Clumped_Vis + Glare90 + swell      0.1519497             0.5698801                0.01826170 8.5958701

# CONCLUSION: At 0.65 km truncation, the half normal model with no adjustment is the best key model fit.
# However, addition of covariates led to best performing model being hn with clumped bf and observer. 

