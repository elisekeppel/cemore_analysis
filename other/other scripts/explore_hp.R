sp <- "Harbour Porpoise"
w <- 0.7
hp <- all_ap_sf %>% filter(Species == sp) %>% data.frame() %>% dplyr::mutate(bfc = case_when(
    Beaufort <2 ~ "0-1",
    Beaufort == 2 ~ "2",
    Beaufort > 2 ~ "3+"
  ))
hn.obs.bfc0.7   <- ds(data=hp, truncation=0.7, key="hn", formula=~SightedBy+bfc)

par(mfrow=c(1,3))
plot(hn.obs.bfc0.7, showpoints=F)
text(x=0.7, y=1.1, label=sp, adj=c(1,1))
text(x=0.6, y=1, label=paste("bins = 25"),adj=c(1,1))
par(mfrow=c(1,2))
plot(hn.obs.bfc0.7, showpoints=F, breaks = seq(0,0.7,0.045))
plot(hn.obs.bfc0.7ret, showpoints=F, breaks = seq(0,0.7,0.045))
# plot(hn.obs.bfc0.7, showpoints=F, breaks = c(0,seq(0.1,0.7,0.045)))

text(x=0.7, y=1.1, label=sp, adj=c(1,1))
text(x=0.6, y=1, label=paste("bins = 15"),adj=c(1,1))

plot(hn.obs.bfc0.7,showpoints=F, breaks = seq(0,0.7,0.07))
text(x=0.7, y=1.1, label=sp ,adj=c(1,1))
text(x=0.6, y=1, label= "bins = 10",adj=c(1,1))
0.7/15

#--------------------------------
sp <- "Dalls Porpoise"
w=0.9
dp <- all_ap_sf %>% filter(Species == sp) %>% data.frame() 

par(mfrow=c(1,2))
hn.0.9_ret   <- ds(data=dp_ret, truncation=0.9, key="hn")    
hn.0.9   <- ds(data=dp, truncation=0.9, key="hn")    

par(mfrow=c(1,2))
# plot(hn.0.9)
text(x=0.8, y=1.1, label=sp,adj=c(1,1))
text(x=0.8, y=1, label=paste( "bins = 10"),adj=c(1,1))

plot(hn.0.9, showpoints=F, breaks = seq(0,0.9,0.06))
plot(hn.0.9_ret, showpoints=F, breaks = seq(0,0.9,0.06))

text(x=0.8, y=1.1, label=sp,adj=c(1,1))

text(x=0.8, y=1, label=paste( "bins = 15"),adj=c(1,1))

plot(hn.0.9, showpoints=F, breaks = seq(0,0.9,0.13))
text(x=0.8, y=1.1, label=sp,adj=c(1,1))
text(x=0.8, y=1, label=paste( "bins = 7"),adj=c(1,1))
0.9/7

#--------------
dp_ret <- sf_ret %>% data.frame()%>% dplyr::mutate(bfc = case_when(
  Beaufort <2 ~ "0-1",
  Beaufort == 2 ~ "2",
  Beaufort > 2 ~ "3+"
))
hn.obs.bfc0.7ret   <- ds(data=hp_ret, truncation=0.7, key="hn", formula=~SightedBy+bfc)

