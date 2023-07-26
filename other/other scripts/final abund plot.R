# for final abundance estimate plot
all_ap_sf %>% 
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
         cGrSz = factor(
           dplyr::case_when(
             size==1 ~ "1",
             size==2 ~ "2",
             size>2 ~ "3+"
           ), levels=  c("1","2","3+")),
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
         swell=factor(case_when(
           swell == "Big >2" ~ "Mod-Big",
           swell == "Moderate 1-2 m" ~ "Mod-Big",
           swell == "Low <1 m" ~ "Low",
           swell == "No swell" ~ "None"),
           levels= c("None", "Low", "Mod-Big")),
         cVis = case_when(
           Visibility == "G&E"~ "G/E",
           !Visibility == "G&E"~ "M/P"),
         l_glare = ifelse(Glare90 == "None", NA, l_glare) %>% as.numeric(),
         r_glare = ifelse(Glare90 == "None", NA, r_glare) %>% as.numeric(),
         Glare90y = ifelse(!Glare90=="None","y","n"),
         Glare90c = ifelse(Glare90=="Severe","Severe","Mild/None"),
         Glare45 = ifelse(l_glare %in% -45:45 | r_glare %in% -45:45, Glare90, "None"),
         Glare45y = ifelse(Glare45=="None","n","y"),
         Glare45c = ifelse(Glare45=="Severe","Severe","Mild/None")
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
                Glare45c,
                Glare90c,
                swell,
                Observer,
                seasonYear
  )
# Humpbacks
sp <-  "humpback"
df.hw <- all_ap_sf %>% filter(Species %like% sp) %>% data.frame()
hw <- df.hw %>% dplyr::select(Region.Label,
                              # Survey=SurveyID,
                              Sample.Label,
                              distance,
                              Species,
                              size = Group_Size,
                              Clumped_Vis
                              # Clumped_Beaufort,
                              # glare,
                              # swell,
                              # Observer
)
# find transects with no sightings and create NA distance rows
obs <- full_join(hw, effort) #%>% #left_join(reg) %>% 

un.2 <- ds(data=obs, truncation=2, key="un")
# summary(un.2)
# encounter rate and group size
er <- un.2$dht$individuals$summary[c(1:4),c(1,5,6,8,9)] %>% 
  transmute(Season=Region,
            n     =round(n,     digits=0), 
            ER    =round(ER   , digits=2),
            cv.ER =round(cv.ER, digits=2),
            GS=round(mean.size, digits=2)
  )
# abundance
Nhat_t <- un.2$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>% 
  transmute(Season=Label,
            N=    round(Estimate,digits=0), 
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))
# density
d <- un.2$dht$individuals$D[c(1:4),c(1,2,4,5,6)]  %>% 
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2),
            cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_hw <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble() 
ab_hw_data <- ab_hw[order(ab_hw$N),]

names(ab_hw) <- c("Season","n","ER","cv.ER","GS","N","cv.N","L95.N","U95.N","D","cv.D","L95.D","U95.D")


#-----------------------------------------------------------
sp <-  "harbour porpoise"
df.hp <- all_ap_sf %>% filter(Species %like% sp) %>% 
  mutate(is.na(Reticle)) %>% data.frame()

hn.bfc.obs0.7 <- ds(data=df.hp, truncation=0.7, key="hn", formula=~Clumped_Beaufort+Observer)
hn.bfc0.7     <- ds(data=df.hp, truncation=0.7, key="hn", formula=~Clumped_Beaufort)
hn.bfc.szc0.7 <- ds(data=df.hp, truncation=0.7, key="hn", formula=~Clumped_Beaufort+Clumped_Group_Size)

save(hn.bfc.obs0.7,
     hn.bfc0.7    ,
     hn.bfc.szc0.7,
     df.hp,
     file="data/hp-models.RData")
}

hp <- df.hp %>% dplyr::select(Region.Label,
                              Sample.Label,
                              distance,
                              Species,
                              size = Group_Size,
                              Clumped_Beaufort,
                              Observer)
# find transects with no sightings and create NA distance rows
obs <- full_join(hp, effort) #%>% #left_join(reg) %>% 

hn.bfc.obs0.7 <- ds(data=obs, truncation=0.7, key="hn", formula=~Clumped_Beaufort+Observer)
# summary(hn.bfc.obs0.7)

# encounter rate and group size
er <- hn.bfc.obs0.7$dht$individuals$summary[c(1:4),c(1,5,6,8,9)] %>% 
  transmute(Season=Region,
            n     =round(n,         digits=0), 
            ER    =round(ER,        digits=2),
            cv.ER =round(cv.ER,     digits=2),
            GS=    round(mean.size, digits=2))
# abundance
Nhat_t <- hn.bfc.obs0.7$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>% 
  transmute(Season=Label,
            N=    round(Estimate,digits=0), 
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))

# density
d <- hn.bfc.obs0.7$dht$individuals$D[c(1:4),c(1,2,4,5,6)] %>% 
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2),
            cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_hp <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble() 
ab_hp_data <- ab_hp[order(ab_hp$N),]
names(ab_hp) <- c("Season","n","ER","cv.ER","GS","N","cv.N","L95.N","U95.N","D","cv.D","L95.D","U95.D")

#--------------------------------------------------------------

sp <-  "Dall's porpoise"
df.dp <- all_ap_sf %>% filter(Species %like% sp) %>% 
  dplyr::mutate(Clumped_Group_Size2 = factor(dplyr::case_when(
    Group_Size==1 ~ "1",
    Group_Size==2 ~ "2",
    Group_Size>2 ~ "3+"
  ), levels=  c("1","2","3+"))) %>% data.frame()

dp <- df.dp %>% dplyr::select(Region.Label,
                              Sample.Label,
                              distance,
                              Species,
                              size = Group_Size,
                              glare,
                              Clumped_Swell)
# find transects with no sightings and create NA distance rows
obs <- full_join(dp, effort) #%>% #left_join(reg) %>% 

dp0.8.hn.sw..g45c     <-ds(obs, key = "hn", truncation = 0.8, formula=~swell*Glare45c)
# summary(hn.sw.gl0.9)

# encounter rate and group size
er <- hn.swc.gl0.9$dht$individuals$summary[c(1:4),c(1,5,6,8,9)]  %>% 
  transmute(Season=Region,
            n     =round(n,     digits=0), 
            ER    =round(ER,    digits=2),
            cv.ER =round(cv.ER, digits=2),
            GS=round(mean.size, digits=2))
# abundance
Nhat_t <- hn.swc.gl0.9$dht$individuals$N[c(1:4),c(1,2,4,5,6)] %>% 
  transmute(Season=Label,
            N=    round(Estimate,digits=0), 
            cv.N= round(cv,      digits=2),
            L95.N=round(lcl,     digits=0),
            U95.N=round(ucl,     digits=0))
# density
d <- hn.swc.gl0.9$dht$individuals$D[c(1:4),c(1,2,4,5,6)] %>% 
  transmute(Season=Label,
            D =    round(Estimate*25,digits=2), # *25 cuz density=indivs/25km^2
            cv.D = round(cv,         digits=2),
            L95.D= round(lcl*25,     digits=2),
            U95.D= round(ucl*25,     digits=2))

ab_dp <- full_join(er, Nhat_t) %>% full_join(d) %>% tibble::as_tibble() #%>% 
# mutate_if(is.numeric, round, digits = 2) 
ab_dp_data <- ab_dp[order(ab_dp$N),]
names(ab_dp) <- c("Season","n","ER","cv.ER","GS","N","cv.N","L95.N","U95.N","D","cv.D","L95.D","U95.D")

#-----------------------------------------------------------
plot_ab_hp <- ab_hp %>% dplyr::select(Season, "Estimated Abundance"=N, L95.N, U95.N) %>% mutate(Species = "Harbour porpoise")
plot_ab_dp <- ab_dp %>% dplyr::select(Season, "Estimated Abundance"=N, L95.N, U95.N) %>% mutate(Species = "Dall's porpoise")
plot_ab_hw <- ab_hw %>% dplyr::select(Season, "Estimated Abundance"=N, L95.N, U95.N) %>% mutate(Species = "Humpback whale")
plot_ab <- rbind(plot_ab_hp, plot_ab_dp, plot_ab_hw) %>%
  mutate(Species=factor(Species, levels = lev),
         Season=factor(Season, levels = levels(all_ap_sf$season)))
plot_ab$Species %<>% droplevels()

pal <- brewer.pal(9, "Set1")[c(1:9)]
cols <- c('Humpback whale' = pal[1],
          'Harbour porpoise' = pal[3],
          'Dall\'s porpoise' = pal[4])
shape <- c('Humpback whale' = 23,
           'Harbour porpoise' = 24,
           'Dall\'s porpoise' = 22)
ggplot(plot_ab, aes(Season, `Estimated Abundance`), colour="black") +
  geom_errorbar(aes(ymin = L95.N, ymax = U95.N,group=Species),
                width=.3,
                # position=position_dodge(.3),
                # size=0.8
  ) +
  geom_point(aes(fill=Species,
                 shape=Species),
             size=3.5,
             position=position_dodge(.3)) +
  scale_fill_manual(values=cols)+
  scale_shape_manual(values=shape)+
  
  theme_classic()+
  theme(legend.text = element_text(size=fig_legend_size),
        legend.title=element_blank(),
        axis.text= element_text(size=fig_axis_size),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size=14))
