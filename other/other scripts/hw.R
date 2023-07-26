
source("R/0 Setup.R")
# cemore <- "C:/Users/keppele/Documents/GitHub/cemore/cemore"
# devtools::load_all(cemore)
{ year <- 2022
  month <- "02"
  # iteration <- "24"
  month_abb <- month.abb[as.numeric(month)]
  survey_title <- paste(first_up(month_abb), year)
  surveyid = paste0("cemore_", year, tolower(month_abb))
  rds <- file.path(dir, "survey_data/surveys.rds")
  iteration <- surveys[which(surveys$SurveyID==surveyid),]$iteration
}
# load sightings data
all_ap_sf <- load_sightings(single=F, month=10,year=2022) # N = 1698 - includes ALL sgt, doesn't end in Oct2022
all_ap_sf %<>% filter(date(time_index) < date("2022-11-01")) # N = 1505
all_ap_sf %<>% filter(date(time_index) < date("2022-03-01")) # N = 1057
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
         clumped_precip = case_when(
           precip == "Clear" ~ "Clear",
           precip %in% c("Mist", "Light Rain") ~ "Mist/Rain",
           precip %in% c("Fog", "Haze", "Smoke") ~ "Fog/Haze/Smoke"
         ),
         l_glare = ifelse(Glare90 == "None", NA, l_glare) %>% as.numeric(),
         r_glare = ifelse(Glare90 == "None", NA, r_glare) %>% as.numeric(),
         Glare90y = ifelse(!Glare90=="None","y","n"),
         Glare45 = ifelse(l_glare %in% -45:45 | r_glare %in% -45:45, Glare90, "None"),
         Glare45y = ifelse(Glare45=="None","n","y")
  ) %>% filter(Vessel == "MB") # N = 1385 (readRDS), N = 1385 (load_sightings) 

sp = "humpback whale"
df <- all_ap_sf %>% filter(Species == sp) %>% data.frame()
nrow(df)  #372 Oct, 193 Feb
# par(mfrow=c(1,2))
terra::hist(df$distance, xlab="Distance (km)", breaks=seq(0,max(df$distance),len=30),
            main=paste0(sp, " to ", survey_title, "-30bins"))#, xlim = range(c(0,4)))  
terra::hist(df$distance, xlab="Distance (km)",breaks=seq(0,max(df$distance),len=60),
            main=paste0(sp, " to ", survey_title, "-60bins"))#, xlim = range(c(0,4)))

# No apparent evasive behaviour (fewer detections around 0 distance) or heaping. 

# Examine reticle vs. eyeballed sighting distances to check for effects of eyeballing distance (eg. heaping/rounding)
ret <- df %>% filter(!is.na(Reticle))
nret <- nrow(ret) #366
# par(mfrow=c(1,2))
hist(ret$distance, xlab="Distance (km)",breaks=seq(0,max(ret$distance),len=30),
     main=paste0("To ", survey_title, " - 30bins - ret only"))
# Will remove non-reticle sightings.
# df <- ret 
df1.5 <- df %>% filter(distance<=1.5)

#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Detection Functions
#---------------------------------------------------------------------
#---------------------------------------------------------------------
# Models with no truncation. Left column AIC = all sgt, left = reticle only sgt

# Truncation at 1.5 appears to line up well with 0.15 x probability of detection (though truncation at 2 km may show a better goodness of fit)
hw1.5.hn       <-ds(df, key = "hn", truncation = 1.5, adjustment = NULL)    # 177.565 ret 177.731
hw1.5.hn.cos   <-ds(df, key = "hn", truncation = 1.5, adjustment = "cos")   # 178.341 ret 178.719
hw1.5.hr       <-ds(df, key = "hr", truncation = 1.5, adjustment = NULL)    # 179.375 ret 180.032
hw1.5.hr.herm  <-ds(df, key = "hr", truncation = 1.5, adjustment = "herm")  # 180.472 ret 180.89
hw1.5.un.cos   <-ds(df, key = "un", truncation = 1.5, adjustment = "cos")   # 177.197 ret 177.423 *****BEST***** order 1
hw1.5.un.poly  <-ds(df, key = "un", truncation = 1.5, adjustment = "poly")  # 179.659 ret 180.114 (2,4)  

model.table1.5 <- summarize_ds_models(hw1.5.hn     ,
                                     hw1.5.hn.cos ,
                                     hw1.5.hr     ,
                                     hw1.5.hr.herm,
                                     hw1.5.un.cos ,
                                     hw1.5.un.poly,
                                     output="plain") 
# uniform with cos(1) adj best model for both with and without reticles. 
#Should we look removing/looking separately at zero's? Any benefit? Who has tried/lloked at this?

#  cov distribution tables - second row is truncated to 1.5 km
table(df1.5$precip)
# Clear        Fog       Haze Light Rain       Mist      Smoke 
# 243         37          2          9          3         78 
# 198         28          2          9          3         75 
table(df1.5$clumped_precip)
# Clear Fog/Haze/Smoke      Mist/Rain 
# 198            105             12 
table(df1.5$Beaufort)
# 0   1   2   3   4 
# 4 154 167  29  18 
# 4 133 137  24  17 
table(df1.5$Clumped_Beaufort)
#  0-1   2  3+ 
#  158 167  47 
# 137 137  41 
table(df1.5$GroupSize)
#   1   2   3   4   5   6   7   8 
# 241  92  20  10   5   1   2   1 
# 211  71  18   8   4       2   1
table(df1.5$Clumped_Group_Size)
#   1   2   3  4+ 
# 241  92  20  19 
# 211  71  18  15 
table(df1.5$Glare90)
# Mild   None Severe 
# 23    289     60 
# 19    243     53 
table(df1.5$Glare90y)
# n   y 
# 289  83
# 243  72
table(df1.5$Glare45)
# Mild   None Severe 
# 16    298     58 
# 13    251     51 
table(df1.5$Glare45y)
# n   y 
# 298  74 
# 251  64 
table(df1.5$swell)
# None   Low Mod-Big 
# 76     156     140 
# 66     136     113 
table(df1.5$Observer)
# a          b        c 
# 160       112       100 
# 124       104        87
table(df1.5$Visibility)
# G&E Moderate        P 
# 326       35       11
# 282       23       10 
table(df1.5$Clumped_Vis)
# G/E M/P 
# 326  46  
# 282  33 

# Test single-covariate hazard-rate models with hr
hw1.5.hr.bf    <-ds(df, 1.5, key="hr", formula=~Beaufort)               # AIC = 180.171
hw1.5.hr.bfc   <-ds(df, 1.5, key="hr", formula=~Clumped_Beaufort)      # AIC = 179.942 *
hw1.5.hr.sz    <-ds(df, 1.5, key="hr", formula=~GroupSize)              # AIC = 180.989 **
hw1.5.hr.szc   <-ds(df, 1.5, key="hr", formula=~Clumped_Group_Size)    # AIC = 185.139
hw1.5.hr.szc2  <-ds(df, 1.5, key="hr", formula=~Clumped_Group_Size2)  # AIC = 183.318
hw1.5.hr.g90   <-ds(df, 1.5, key="hr", formula=~Glare90)                # AIC = 178.877 *  
hw1.5.hr.g90y  <-ds(df, 1.5, key="hr", formula=~Glare90y)              # AIC = 181.034
hw1.5.hr.g45   <-ds(df, 1.5, key="hr", formula=~Glare45)                # AIC = 180.18
hw1.5.hr.g45y  <-ds(df, 1.5, key="hr", formula=~Glare45y)              # AIC = 181.138
hw1.5.hr.sw    <-ds(df, 1.5, key="hr", formula=~swell)                  # AIC = 182.998
hw1.5.hr.ob    <-ds(df, 1.5, key="hr", formula=~Observer)               # AIC = 180.829   
hw1.5.hr.vis   <-ds(df, 1.5, key="hr", formula=~Visibility)            # AIC = 180.664  
hw1.5.hr.visc  <-ds(df, 1.5, key="hr", formula=~Clumped_Vis)          # AIC = 180.537 *
hw1.5.hr.prec  <-ds(df, 1.5, key="hr", formula=~precip)               # AIC = 188.277
hw1.5.hr.precc <-ds(df, 1.5, key="hr", formula=~clumped_precip)      # AIC = 183.313
hw1.5.hr.cc    <-ds(df, 1.5, key="hr", formula=~CloudCover)      # AIC = 183.313


# Test single-covariate half-normal models
hw1.5.hn.bf   <-ds(df, 1.5, key="hn", formula=~Beaufort)              # AIC = 178.861 ret 178.895 * (second column is reticle sgt only)
hw1.5.hn.bfc  <-ds(df, 1.5, key="hn", formula=~Clumped_Beaufort)      # AIC = 179.352 ret 179.457
hw1.5.hn.sz   <-ds(df, 1.5, key="hn", formula=~GroupSize)             # AIC = 179.331 ret 179.564 **
hw1.5.hn.szc  <-ds(df, 1.5, key="hn", formula=~Clumped_Group_Size)    # AIC = 182.832 ret 183.022
hw1.5.hn.szc2  <-ds(df, 1.5, key="hn", formula=~Clumped_Group_Size2)  # AIC = 180.833 ret 181.023
hw1.5.hn.g90  <-ds(df, 1.5, key="hn", formula=~Glare90)               # AIC = 178.916 ret 179.411
hw1.5.hn.g90y <-ds(df, 1.5, key="hn", formula=~Glare90y)              # AIC = 177.692 ret 178.025 *   close but not better
hw1.5.hn.g45  <-ds(df, 1.5, key="hn", formula=~Glare45)               # AIC = 179.783 ret 180.239
hw1.5.hn.g45y <-ds(df, 1.5, key="hn", formula=~Glare45y)              # AIC = 177.876 ret 178.332
hw1.5.hn.sw   <-ds(df, 1.5, key="hn", formula=~swell)                 # AIC = 181.352 ret 181.678
hw1.5.hn.ob   <-ds(df, 1.5, key="hn", formula=~Observer)              # AIC = 181.56  ret 181.705
hw1.5.hn.vis  <-ds(df, 1.5, key="hn", formula=~Visibility)            # AIC = 175.893 ret 178.263 *****BEST*****
hw1.5.hn.visc <-ds(df, 1.5, key="hn", formula=~Clumped_Vis)           # AIC = 179.113 ret 179.633
hw1.5.hn.prec  <-ds(df, 1.5, key="hn", formula=~precip)               # AIC = 184.525
hw1.5.hn.sw.bf <- ds(df, 1.5, key="hn", formula=~Clumped_Beaufort*swell) # 187.367
hw1.5.hn.sw.ob <- ds(df, 1.5, key="hn", formula=~Observer*swell)          # 189.161
hw1.5.hn.ob.bf <- ds(df, 1.5, key="hn", formula=~Clumped_Beaufort*Observer) # 182.848
# summary(hw1.5.hn.ob.bf)

ggplot(data = df1.5, aes(Glare45, distance)) +
  geom_violin() +
  geom_boxplot(width=0.1)+
   ylab("Perpendicular sighting distance (km)")+
  xlab("")

# +
#   theme(axis.text=element_text(size=fig_axis_size),
#         axis.title=element_text(size=fig_axis_title_size))

# Create summary of single covariate detection function models at 1.5 km truncation. 
model.table1.5 <- summarize_ds_models(hw1.5.un.cos,
                                      hw1.5.hn,
                                      hw1.5.hr,
                                      hw1.5.hn.bf   ,
                                      hw1.5.hn.bfc  ,
                                      hw1.5.hn.sz   ,
                                      hw1.5.hn.szc  ,
                                      hw1.5.hn.szc2 ,
                                      hw1.5.hn.g90  ,
                                      hw1.5.hn.g90y ,
                                      hw1.5.hn.g45  ,
                                      hw1.5.hn.g45y ,
                                      hw1.5.hn.sw   ,
                                      hw1.5.hn.ob   ,
                                      hw1.5.hn.vis  ,
                                      hw1.5.hn.visc ,
                                      hw1.5.hn.prec ,
                                      hw1.5.hn.sw.bf,
                                      hw1.5.hn.sw.ob,
                                      hw1.5.hn.ob.bf,
                                      hw1.5.hr.bf   ,
                                      hw1.5.hr.bfc  ,
                                      hw1.5.hr.sz   ,
                                      hw1.5.hr.szc  ,
                                      hw1.5.hr.szc2 ,
                                      hw1.5.hr.g90  ,
                                      hw1.5.hr.g90y ,
                                      hw1.5.hr.g45  ,
                                      hw1.5.hr.g45y ,
                                      hw1.5.hr.sw   ,
                                      hw1.5.hr.ob   ,
                                      hw1.5.hr.vis  ,
                                      # hw1.5.hr.visc ,
                                      hw1.5.hr.prec ,
                                      hw1.5.hr.precc,
                                      output="plain") 


# Model                                              Key function   Formula     C-vM $p$-value    Average detectability se(Average detectability) Delta AIC
# 5  hw1.5.hn.vis                                    Half-normal ~Visibility      0.7535238             0.5905040                0.02886512  0.000000
# 1  hw1.5.un.cos Uniform with cosine adjustment term of order 1        <NA>      0.7283786             0.5955247                0.02214056  1.304966
# 3 hw1.5.hn.g90y                                    Half-normal   ~Glare90y      0.7356914             0.5967190                0.02796036  1.799131
# 4 hw1.5.hn.g45y                                    Half-normal   ~Glare45y      0.7202525             0.5968935                0.02795668  1.983720
# 6   hw1.5.hn.bf                                    Half-normal   ~Beaufort      0.6825414             0.5977245                0.02782700  2.968699
# 2  hw1.5.hr.g90                                    Hazard-rate    ~Glare90      0.9306447             0.5755128                0.04584773  2.984200
#---------------------------------------------------------------------
# Multiple covariates - backward selection
#---------------------------------------------------------------------

# Fit global half-normal model:
hw.hn.global<-ds(df, truncation=1.5, key="hn", formula=~Glare90y+Visibility+Beaufort+GroupSize+swell+Observer) # AIC = 185.712 (worse)
plot(hw.hn.global)
# Try dropping terms from global model:
hw.hn.1<-ds(df, truncation=1.5, key="hn", formula=~Visibility+Glare90y+Beaufort+GroupSize+swell) # AIC = 182.149 (b) 
hw.hn.2<-ds(df, truncation=1.5, key="hn", formula=~Visibility+Glare90y+Beaufort+GroupSize+Observer) # AIC = 181.727 (b) - sw
hw.hn.3<-ds(df, truncation=1.5, key="hn", formula=~Glare90y+Visibility+Beaufort+swell+Observer) # AIC = 185.82 (worse)
hw.hn.4<-ds(df, truncation=1.5, key="hn", formula=~Glare90y+Visibility+GroupSize+swell+Observer) # AIC = 183.898 (b)
hw.hn.5<-ds(df, truncation=1.5, key="hn", formula=~Glare90y+Beaufort+GroupSize+swell+Observer) # AIC = 188.96 (worse)
hw.hn.6<-ds(df, truncation=1.5, key="hn", formula=~Visibility+Beaufort+GroupSize+swell+Observer) # AIC = 185.037 (b)

hw.hn.7<-ds(df, truncation=1.5, key="hn", formula=~Visibility+Glare90y+Beaufort+GroupSize) # AIC = 178.186 -obs
hw.hn.8<-ds(df, truncation=1.5, key="hn", formula=~Visibility+Glare90y+Beaufort+Observer) # AIC = 181.83
hw.hn.9<-ds(df, truncation=1.5, key="hn", formula=~Visibility+Glare90y+GroupSize+Observer) # AIC = 179.9
hw.hn.10<-ds(df, truncation=1.5, key="hn", formula=~Visibility+Beaufort+GroupSize+Observer) # AIC = 181.727
hw.hn.11<-ds(df, truncation=1.5, key="hn", formula=~Glare90y+Beaufort+GroupSize+Observer) # AIC = 184.996

hw.hn.12<-ds(df, truncation=1.5, key="hn", formula=~Visibility+Glare90y+Beaufort) # AIC = 177.857
hw.hn.13<-ds(df, truncation=1.5, key="hn", formula=~Visibility+Glare90y+GroupSize) # AIC = 176.31 - bf
hw.hn.14<-ds(df, truncation=1.5, key="hn", formula=~Visibility+Beaufort+GroupSize) # AIC = 177.528
hw.hn.15<-ds(df, truncation=1.5, key="hn", formula=~Glare90y+Beaufort+GroupSize) # AIC = 181.397

hw.hn.16<-ds(df, truncation=1.5, key="hn", formula=~Visibility+GroupSize) # AIC = 176.432
hw.hn.17<-ds(df, truncation=1.5, key="hn", formula=~Visibility+Glare90y) # AIC = 176.009 - gpsz
hw.hn.18<-ds(df, truncation=1.5, key="hn", formula=~Glare90y+GroupSize) # AIC = 179.473

hw1.5.hn.vis<-ds(df, truncation=1.5, key="hn", formula=~Visibility) # AIC = 175.893 **BEST**
hw1.5.hn.g90y<-ds(df, truncation=1.5, key="hn", formula=~Glare90y) # AIC = 177.692

hw1.5.hn <-ds(df, truncation=1.5, key="hn") # AIC = 177.565 (w)

# Fit global hazard-rate model:
hw.hr.global<-ds(df, truncation=1.5, key="hr", formula=~Glare90+Visibility+Beaufort+GroupSize+swell+Observer) # AIC = 190.041 worse
plot(hw.hr.global)
# Try dropping terms from global model:
hw.hr.1<-ds(df, truncation=1.5, key="hr", formula=~Glare90+Visibility+Beaufort+GroupSize+swell) # AIC = 188.188 worse
hw.hr.2<-ds(df, truncation=1.5, key="hr", formula=~Glare90+Visibility+Beaufort+GroupSize+Observer) # AIC = 187.214 worse - sw
hw.hr.3<-ds(df, truncation=1.5, key="hr", formula=~Glare90+Visibility+Beaufort+swell+Observer) # AIC = 189.4 worse
hw.hr.4<-ds(df, truncation=1.5, key="hr", formula=~Glare90+Visibility+GroupSize+swell+Observer) # AIC = 190.168 worse - bf
hw.hr.5<-ds(df, truncation=1.5, key="hr", formula=~Visibility+Beaufort+GroupSize+swell+Observer) # AIC = 189.768 worse
hw.hr.6<-ds(df, truncation=1.5, key="hr", formula=~Glare90+Beaufort+GroupSize+swell+Observer) # AIC = 188.205 worse

hw.hr.7<-ds(df, truncation=1.5, key="hr", formula=~Glare90+Visibility+Beaufort+GroupSize) # AIC = 184.276 worse - obs
hw.hr.8<-ds(df, truncation=1.5, key="hr", formula=~Glare90+Visibility+Beaufort+Observer) # AIC = 185.436 worse 
hw.hr.9<-ds(df, truncation=1.5, key="hr", formula=~Glare90+Beaufort+GroupSize+Observer) # AIC = 184.351 worse
hw.hr.10<-ds(df, truncation=1.5, key="hr", formula=~Visibility+Beaufort+GroupSize+Observer) # AIC = 185.824 worse 
hw.hr.11<-ds(df, truncation=1.5, key="hr", formula=~Glare90+Visibility+GroupSize+Observer) # AIC = 186.243 worse 

hw.hr.12<-ds(df, truncation=1.5, key="hr", formula=~Glare90+Visibility+Beaufort) # AIC = 182.422
hw.hr.13<-ds(df, truncation=1.5, key="hr", formula=~Glare90+Beaufort+GroupSize) # AIC = 182.116 - vis
hw.hr.14<-ds(df, truncation=1.5, key="hr", formula=~Visibility+Beaufort+GroupSize) # AIC = 183.145
hw.hr.15<-ds(df, truncation=1.5, key="hr", formula=~Glare90+Visibility+GroupSize) # AIC = 182.961

hw.hr.16<-ds(df, truncation=1.5, key="hr", formula=~Glare90+Beaufort) # AIC = 180.27 - gpsz
hw.hr.17<-ds(df, truncation=1.5, key="hr", formula=~Beaufort+GroupSize) # AIC = 181.767
hw.hr.18<-ds(df, truncation=1.5, key="hr", formula=~Glare90+GroupSize) # AIC = 180.747

hw.hr.19<-ds(df, truncation=1.5, key="hr", formula=~Beaufort) # AIC = 180.171
hw.hr.20<-ds(df, truncation=1.5, key="hr", formula=~Glare90) # AIC = 178.877


model.table1.5 <- summarize_ds_models(hw1.5.un.cos ,
                                      # hw1.5.hr.g90, # top single covariate hr model (same as hw.hr.5)
                                      hw1.5.hn.g90y ,
                                      hw1.5.hn.vis,
                                      # hw.hr.global,
                                      hw.hn.2,      
                                      hw.hn.global,
                                      output="plain")

# Model                                   Key function                                                         Formula               C-vM $p$-value       Average detectability           se    Delta AIC
# 4  hw1.5.hn.vis                                    Half-normal                                                      ~Visibility      0.7535238             0.5905040                0.02886512  0.000000
# 1  hw1.5.un.cos Uniform with cosine adjustment term of order 1                                                             <NA>      0.7283786             0.5955247                0.02214056  1.304966
# 3 hw1.5.hn.g90y                                    Half-normal                                                        ~Glare90y      0.7356914             0.5967190                0.02796036  1.799131
# 2  hw1.5.hr.g90                                    Hazard-rate                                                         ~Glare90      0.9306447             0.5755128                0.04584773  2.984200
# 6       hw.hn.2                                    Half-normal         ~Visibility + Glare90y + Beaufort + GroupSize + Observer      0.7372168             0.5863498                0.03294230  5.834143
# 7  hw.hn.global                                    Half-normal ~Glare90y + Visibility + Beaufort + GroupSize + swell + Observer      0.7344997             0.5863628                0.03325550  9.819259
# 5  hw.hr.global                                    Hazard-rate               ~Glare90 + Beaufort + GroupSize + swell + Observer      0.9635759             0.5649929                0.04662392 12.312557

par(mfrow=c(1,2))
plot_df(sp, hw1.5.un.cos)
plot_df(sp, hw1.5.hn.g90y)

par(mfrow=c(1,2))
gof_ds(hw1.5.un.cos, main="hw1.5.un.cos")
gof_ds(hw1.5.hn.g90y, main="hw1.5.hn.g90y")
# CONCLUSION: At 1.5 km truncation, the uniform model with adjustment cos(1) is the best key model fit with lowest delta AIC and best fit residual line.
# Half normal with Glare to 90 degrees (y/n) is a better fit than any other single term MCDS detection functions.
# Second and third performing are half normal with glare to 90 clumped to yes/no, followed by half normal with glare to 90 (not clumped).


# #-------------------------------------------------------------------------------------------
# Forward selection
# #-------------------------------------------------------------------------------------------
model.table1.5 <- summarize_ds_models(hw1.5.hn,
                                      hw1.5.hn.bf,
                                      hw1.5.hn.g90y ,
                                      hw1.5.hn.ob,
                                      hw1.5.hn.sw,
                                      hw1.5.hn.sz,
                                      hw1.5.hn.vis,
                                      hw.hn.global,
                                      output="plain")
model.table1.5

# vis as cov results in lowest AIC. Model with vis and each of the other cov's

hw1.5.hn.v.g90y    <-ds(df, key = "hn", truncation = 1.5, formula=~Visibility+Glare90y)
hw1.5.hn.v.sz      <-ds(df, key = "hn", truncation = 1.5, formula=~Visibility+size)
hw1.5.hn.v.g90y.sz <-ds(df, key = "hn", truncation = 1.5, formula=~Visibility+Glare90y+size)
hw1.5.hn.v.bf      <-ds(df, key = "hn", truncation = 1.5, formula=~Visibility+Beaufort)
hw1.5.hn.v.sw      <-ds(df, key = "hn", truncation = 1.5, formula=~Visibility+swell)
hw1.5.hn.v.ob      <-ds(df, key = "hn", truncation = 1.5, formula=~Visibility+Observer)

model.table1.5v <- summarize_ds_models(
  hw1.5.hn.vis,
  hw1.5.hn.v.g90y,
  hw1.5.hn.v.sz  ,
  hw1.5.hn.v.g90y.sz,
  hw1.5.hn.v.bf  ,
  hw1.5.hn,
  hw1.5.hn.v.ob  ,
  hw1.5.hn.v.sw  ,
  # hw.hn.global,
  output="plain")

model.table1.5v

# No model resulted in a lower AIC, so the df with vis as the only covariate is the best fit.

# This forward selection process gives the same resulting model with vis as the backward selection,
# but with fewer steps - will try this method moving forward for efficiency.

# #-------------------------------------------------------------------------------------------
# #-------------------------------------------------------------------------------------------
# # Truncation at 4
# hw4.hn       <-ds(df, key = "hn", truncation = 4, adjustment = NULL)    # 663.993
# hw4.hn.cos   <-ds(df, key = "hn", truncation = 4, adjustment = "cos")   # 595.553, Best key model at 4 km truncation
# hw4.hr       <-ds(df, key = "hr", truncation = 4, adjustment = NULL)    # 599.076
# hw4.hr.herm  <-ds(df, key = "hr", truncation = 4, adjustment = "herm")  # na
# hw4.un.cos   <-ds(df, key = "un", truncation = 4, adjustment = "cos")   # 599.274
# hw4.un.poly  <-ds(df, key = "un", truncation = 4, adjustment = "poly")  # 663.993
# 
# # Test single-covariate hazard-rate models with hr
# hw4.hr.bf   <-ds(df, 4, key="hr", formula=~Beaufort)            # = 597.413 *
# hw4.hr.bfc  <-ds(df, 4, key="hr", formula=~Clumped_Beaufort)    # = 598.184
# hw4.hr.sz   <-ds(df, 4, key="hr", formula=~GroupSize)           # = 600.355 *
# hw4.hr.szc  <-ds(df, 4, key="hr", formula=~Clumped_Group_Size)  # = 604.053
# hw4.hr.szc2 <-ds(df, 4, key="hr", formula=~Clumped_Group_Size2) # = 602.21
# hw4.hr.g90  <-ds(df, 4, key="hr", formula=~Glare90)             # = 598.462 *
# hw4.hr.g90y <-ds(df, 4, key="hr", formula=~Glare90y)            # = 598.502
# hw4.hr.g45  <-ds(df, 4, key="hr", formula=~Glare45)             # = 601.407
# hw4.hr.g45y <-ds(df, 4, key="hr", formula=~Glare45y)            # = 600.311
# hw4.hr.sw   <-ds(df, 4, key="hr", formula=~swell)               # = 601.803
# hw4.hr.ob   <-ds(df, 4, key="hr", formula=~Observer)            # = 594.387   # Best
# hw4.hr.vis  <-ds(df, 4, key="hr", formula=~Visibility)          # = 596.514 *
# hw4.hr.visc <-ds(df, 4, key="hr", formula=~Clumped_Vis)         # = 600.783
# 
# # Test single-covariate half-normal models 
# hw4.hn.bf   <-ds(df, 4, key="hn", formula=~Beaufort)            # = 658.528 *
# hw4.hr.bfc  <-ds(df, 4, key="hn", formula=~Clumped_Beaufort)    # = 660.014
# hw4.hr.sz   <-ds(df, 4, key="hn", formula=~GroupSize)           # = 656.737
# hw4.hr.szc  <-ds(df, 4, key="hn", formula=~Clumped_Group_Size)  # = 647.225
# hw4.hr.szc2 <-ds(df, 4, key="hn", formula=~Clumped_Group_Size2) # = 645.226 *
# hw4.hr.g90  <-ds(df, 4, key="hn", formula=~Glare90)             # = 667.47
# hw4.hr.g90y <-ds(df, 4, key="hn", formula=~Glare90y)            # = 665.472
# hw4.hr.g45  <-ds(df, 4, key="hn", formula=~Glare45)             # = 666.058
# hw4.hr.g45y <-ds(df, 4, key="hn", formula=~Glare45y)            # = 664.058 *
# hw4.hr.sw   <-ds(df, 4, key="hn", formula=~swell)               # = 660.643
# hw4.hr.ob   <-ds(df, 4, key="hn", formula=~Observer)            # = 621.925
# hw4.hr.vis  <-ds(df, 4, key="hn", formula=~Visibility)          # = 644.043 *
# hw4.hr.visc <-ds(df, 4, key="hn", formula=~Clumped_Vis)         # = 654.707   # none better
# 
# # Create summary of single covariate detection function models
# model.table4 <- summarize_ds_models(hw4.hr.ob,
#                                       hw4.hn.cos,
#                                     hw4.hr.bf,
#                                     hw4.hr.vis,
#                                         output="plain") 
# Model                                       Key function     Formula C-vM $p$-value Average detectability se(Average detectability) Delta AIC
# 1  hw4.hr.ob                                        Hazard-rate   ~Observer   2.716745e-01             0.2898758                0.52967288  0.000000
# 2 hw4.hn.cos Half-normal with cosine adjustment term of order 2          ~1   2.755741e-01             0.2811525                0.01050965  1.166064
# 3  hw4.hr.bf                                        Hazard-rate   ~Beaufort   6.888378e-01             0.2714791                0.01804140  3.026029
# 4 hw4.hr.vis                                        Half-normal ~Visibility   1.044924e-06             0.3521536                0.01135481 49.656421

# AT 4 km truncation, best key only model is half normal with cos(2) adjustment. Best single covariate model is hazard rate with observer
# as a covariate, followed by the key only hn, then hazard rate bf and hr vis. Strangely, the last two switch orders when comparing 
# direct resulting AIC from each model run individually compared to running the summarize function...?

