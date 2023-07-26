# April 5, 2023
t <- data.frame(month_abb = factor(c(month.abb[8:12], month.abb, month.abb)), year=c(rep(2020, 5), rep(2021,12), rep(2022, 12)))

sgt_sum <- all_ap_sf %>% filter(Species %like% "humpback") %>% group_by(year, month_abb) %>% summarise(n=sum(Group_Size)) %>% 
  data.frame() %>% dplyr::select(-geometry) %>% full_join(t)

eff_sum <- all_effort_lines %>% data.frame() %>% 
  group_by(year, month_abb) %>% summarise(dist=sum(length_km)) %>% data.frame()%>% full_join(t)

all <- full_join(sgt_sum, eff_sum) %>% arrange(year, month_abb) 
all$yearMonth <- paste(all$year, all$month_abb, sep="_")
l <- unique(all$yearMonth)
all$yearMonth <- factor(all$yearMonth, levels = l)

par(mfrow=c(2,1))

ggplot(data=all, aes(x=year, y=n, fill=month_abb)) +
  geom_bar(stat="identity", position ="dodge") 
ggplot(data=all, aes(x=year, y=dist, fill=month_abb)) +
  geom_bar(stat="identity", position ="dodge") 

long <- all %>% 
  tidyr::pivot_longer(cols = c(n, dist), names_to = "var", values_to = "value")




ggplot(data=long, aes(x=yearMonth, y=value, fill=var, color=var, alpha=var)) +
  geom_bar(stat="identity", position ="identity") +
  scale_colour_manual(values=c("lightblue4", "red")) +
  scale_fill_manual(values=c("lightblue", "pink")) +
  scale_alpha_manual(values=c(.3, .8)) +
  theme(axis.text.x=element_text(angle=90))

s <- all %>% mutate(sgt_p_km=n/dist)
ggplot(data=s, aes(x=year, y=sgt_p_km, fill=month_abb)) +
  geom_bar(stat="identity", position ="dodge") +
  theme(axis.text.x=element_text(angle=90))