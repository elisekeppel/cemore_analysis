# April 5, 2023
t <- data.frame(month_abb = factor(c(month.abb[8:12], month.abb, month.abb, month.abb[1:12])), year=c(rep(2020, 5), rep(2021,12), rep(2022, 12), rep(2023,12)))

sgt_sum <- all_ap_sf %>% 
  filter(date <"2023-01-01") %>% 
  filter(Species %like% "humpback") %>% 
  group_by(year, month_abb) %>% 
  summarise(n=sum(Group_Size)) %>% 
  data.frame() %>% 
  dplyr::select(-geometry) %>%
  full_join(t)

eff_sum <- all_effort_lines %>% data.frame() %>% 
  filter(date <"2023-01-01") %>% 
  group_by(year, season, month_abb) %>% summarise(dist=sum(length_km)) %>% data.frame()%>% full_join(t)

all <- full_join(sgt_sum, eff_sum) %>%
  arrange(year, month_abb) %>% 
  mutate(n_per_km=n/dist)
all$yearMonth <- paste(all$year, all$month_abb, sep="_")
l <- unique(all$yearMonth)
all$yearMonth <- factor(all$yearMonth, levels = l)

# par(mfrow=c(2,1))
ggplot(data=all, aes(x=yearMonth, y=n, fill=season)) +
  geom_bar(stat="identity", position ="dodge") +
  theme(axis.text.x=element_text(angle=90)) +
  ggtitle("hw counts by month & year")
ggsave("summaries/hw_counts_by_month_and_year.png")
ggplot(data=all, aes(x=yearMonth, y=n_per_km, fill=season)) +
  geom_bar(stat="identity", position ="dodge") +
  theme(axis.text.x=element_text(angle=90)) +
  ggtitle("hw n per km by month & year")
ggsave("summaries/hw_n_per_km_by_month_and_year.png")
ggplot(data=all, aes(x=yearMonth, y=dist, fill=season)) +
  geom_bar(stat="identity", position ="dodge") +
  theme(axis.text.x=element_text(angle=90)) +
  ggtitle("effort (km) by month & year")
ggsave("summaries/effort_km_by_month_and_year.png")

eff <- all_effort_lines %>% filter(date <"2023-01-01") %>% 
  group_by(SurveyID,season) %>% 
  summarise(l=sum(length_km)) %>% 
  st_drop_geometry()
s <- all_ap_sf %>% filter(date <"2023-01-01", Species %like% "hump") %>% 
  group_by(SurveyID) %>% 
  summarise(n=sum(Group_Size)) %>% 
  st_drop_geometry()
d <- left_join(eff, s) %>% mutate(n_per_km=n/l)
ggplot(d) +
  geom_col(aes(x=SurveyID, y=n_per_km, fill=season)) +
  theme(axis.text.x = element_text(angle=90))

#####################################################\
# SEASON YEAR
#####################################################
eff <- all_effort_lines %>% filter(date <"2023-01-01") %>% 
  group_by(seasonYear, season) %>% 
  summarise(l=sum(length_km)) %>% 
  st_drop_geometry()
s <- all_ap_sf %>% filter(date <"2023-01-01", Species %like% "hump") %>% 
  group_by(seasonYear) %>% 
  summarise(n=sum(Group_Size)) %>% 
  st_drop_geometry()
d <- left_join(eff, s) %>% mutate(n_per_km=n/l)
ggplot(d) +
  geom_col(aes(x=seasonYear, y=n_per_km, fill=season)) +
  theme(axis.text.x = element_text(angle=90))


#####################################################
# TO 2023
#####################################################

eff2023 <- all_effort_lines %>% 
  # filter(date <"2023-01-01") %>% 
  group_by(SurveyID,season) %>% 
  summarise(l=sum(length_km)) %>% 
  st_drop_geometry()
s2023 <- all_ap_sf %>% 
  # filter(date <"2023-01-01") %>% 
  filter(Species %like% "hump") %>% 
  group_by(SurveyID) %>% 
  summarise(n=sum(Group_Size)) %>% 
  st_drop_geometry()
d2023 <- left_join(eff2023, s2023) %>% mutate(n_per_km=n/l)
ggplot(d2023) +
  geom_col(aes(x=SurveyID, y=n_per_km, fill=season)) +
  theme(axis.text.x = element_text(angle=90))

#####################################################
# SEASON YEAR
#####################################################
eff2023 <- all_effort_lines %>% 
  # filter(date <"2023-01-01") %>% 
  group_by(seasonYear, season) %>% 
  summarise(l=sum(length_km)) %>% 
  st_drop_geometry()
s2023 <- all_ap_sf %>% 
  # filter(date <"2023-01-01") %>% 
  filter(Species %like% "hump") %>% 
  group_by(seasonYear) %>% 
  summarise(n=sum(Group_Size)) %>% 
  st_drop_geometry()
d2023 <- left_join(eff2023, s2023) %>% mutate(n_per_km=n/l)
ggplot(d2023) +
  geom_col(aes(x=seasonYear, y=n_per_km, fill=season)) +
  theme(axis.text.x = element_text(angle=90))




##########################################################################
max <- all$dist[which(!is.na(all$dist))] %>% max()
all <- all %>% mutate(std_dist=dist/max)
all <- all %>% mutate(no_eff=case_when(
  is.na(dist) ~ 400,
  !is.na(dist) ~ 0
))

long <- all %>% 
  tidyr::pivot_longer(cols = c(n, dist), names_to = "var", values_to = "value")

ggplot(data=long, aes(x=yearMonth, y=value, fill=var, color=var, alpha=var)) +
  geom_bar(stat="identity", position ="identity") +
  scale_colour_manual(values=c("lightblue4", "red")) +
  scale_fill_manual(values=c("lightblue", "pink")) +
  scale_alpha_manual(values=c(.3, .8)) +
  theme(axis.text.x=element_text(angle=90))


s <- all %>% mutate(sgt_p_km=n/std_dist)
ggplot(data=s, aes(x=yearMonth, y=sgt_p_km, fill=month_abb)) +
  geom_bar(stat="identity", position ="dodge") +
  ggnewscale::new_scale_fill() +
  geom_bar(data=s, aes(x=yearMonth, y=no_eff), stat="identity", fill="grey85") +
  theme(axis.text.x=element_text(angle=90))
