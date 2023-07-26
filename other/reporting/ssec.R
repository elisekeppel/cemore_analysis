# for ssec

sp <- "Harbour"
ap_sf <- all_ap_sf %>% filter(Species %like% sp)
ap_sf$Species %<>% droplevels()
# to order legend symbols consistently
sp <- unique(c(levels(ap_sf$Species)))

# to size symbols by count
ap_sf <- ap_sf %>% mutate(Count =case_when(
  BestNumber == 1 ~ "1",
  BestNumber %in% c(2:5) ~ "2:5",
  BestNumber >5 ~ ">5"
) %>% factor(levels = c("1", "2:5", ">5")))

g_hp <- ggplot() +
  geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  labs(fill = "Depth (m)") +
  scale_fill_gradientn(colours = col_ramp(20)) +
  ggnewscale::new_scale("fill") +
  
  geom_sf(data = coast_file, stroke = 0.05, fill = "light yellow", colour = "grey 60")+
  geom_sf(data = all_effort_lines, stroke = 0.25, aes(colour = factor(year)))+
  scale_colour_manual(name = "Effort in year", values = c("pink", "grey 40")) +
  ggnewscale::new_scale_colour()+
  geom_sf(data = ap_sf, stroke = 0.01, alpha = 0.5, aes(colour = factor(year), size = Count)) +#
  scale_colour_manual("Sightings in year", values = c("red", "black")) +
  scale_size_manual(values = c(1.5,2,3)) +
  # scale_fill_manual(values = cols, name = "Sightings", breaks = sp)   +
  # scale_shape_manual(values = shape, name = "Sightings", breaks = sp) +
  
  guides(alpha= "none") +
  # guides(shape = guide_legend(override.aes = list(size=2))) +
  
  ggtitle(paste0("CeMoRe survey ", sp, " sightings to ", survey_title)) +
  # annotation_custom(leg1Grob, xmin=-124.8, xmax=-124.95, ymin=47.9, ymax=48.1) +
  coord +
  ylab("")+xlab("") + 
  theme(axis.text.x = element_text(angle = 90)) +
  # facet_wrap(~month_abb, drop = F)
  facet_wrap(~factor(season, levels = c("Winter", "Spring", "Summer", "Fall")), drop = F)
g_hp
# ggsave("output_maps/HW monthly sightings effort by year to oct 2021.png")
ggsave(paste0("output_maps/", sp, " seasonal sightings effort by year to ",month_abb, " ", year, ".png"))


all_ap_sf%<>% arrange(year, month) 
lev <- unique(all_ap_sf$SurveyID )
all_ap_sf$SurveyID %<>% factor(levels = lev)

t <- plot_hist(all_ap_sf, "Humpback")
t+facet_grid(. ~season)
ggsave("output_maps/ssec/HW monthly sgt dist to oct 2021.png")

unique(ap_sf$Species)

#-------------------------------------------------------------------------
# --------------- just october 2020 vs 2021
#-------------------------------------------------------------------------

oct_hp <- ap_sf %>% filter(month == 10)
oct_lines <- all_effort_lines %>% filter(month == 10)

# to size symbols by count
oct_hp <- oct_hp %>% mutate(Count =case_when(
  BestNumber == 1 ~ "1",
  BestNumber %in% c(2:5) ~ "2:5",
  BestNumber >5 ~ ">5"
) %>% factor(levels = c("1", "2:5", ">5")))

coord <- ggplot2::coord_sf(xlim = c(-125.4, -123), ylim = c(48.2, 49.44), crs = sf::st_crs(4326)) 

ggplot() +
geom_raster(aes(x=x,y=y,fill = z), data = bathy) +  labs(fill = "Depth (m)") +
  scale_fill_gradientn(colours = col_ramp(20)) +
  ggnewscale::new_scale("fill") +
  
  geom_sf(data = coast_file, stroke = 0.05, fill = "light yellow", colour = "grey 60")+
  geom_sf(data = oct_lines, stroke = 0.25, colour = "grey 40")+
  geom_sf(data = oct_hp, stroke = 0.01, alpha = 0.5, aes(size = Count)) +#
  scale_size_manual(values = c(1.5,2,3)) +
  # scale_fill_manual(values = cols, name = "Sightings", breaks = sp)   +
  # scale_shape_manual(values = shape, name = "Sightings", breaks = sp) +
  
  guides(alpha= "none") +
  # guides(shape = guide_legend(override.aes = list(size=2))) +
  
  ggtitle(paste0("CeMoRe survey Humpback sightings Oct 2020 vs Oct 2021 ", survey_title)) +
  # annotation_custom(leg1Grob, xmin=-124.8, xmax=-124.95, ymin=47.9, ymax=48.1) +
  coord +
  ylab("")+xlab("") + facet_wrap(~year, drop = F)
ggsave("output_maps/ssec/HP sightings oct 2020 vs oct 2021.png")

#-------------------------------------------------------------------------
# --------------- hist
#-------------------------------------------------------------------------

by_season = T
  title <- paste0("All CeMoRe surveys from Aug 2020 - ", survey_title)
  col <- (RColorBrewer::brewer.pal(9L, "Blues")[4:7])
  col_ramp <- colorRampPalette(col)
  
  s <- all_ap_sf %>%
    filter(!is.na(PSD_nm))
  if(!is.null(sp)) s %<>% filter(Species %like% sp)
  
  g <- ggplot() +
    geom_histogram(data = s, aes(PSD_nm, fill = as.character(beauf)), bins = 30) +
    scale_fill_manual(name ="Beaufort",values= col_ramp(5)) +
    ggtitle(paste0("Distances to ", sp, " observations"), subtitle = survey_title) +
    xlab("Distance (nm)") +
    ylab("Count") 
  g
  ggsave("output_hist/hw all dist to oct 2021.png")
  
  # to create text for facet grid (for non-facetted plot, group by 'Event') to get only one line for plot text
  # to facet by second variable, add desired variable to 'group_by' here
  y <- ggplot(data = s) + geom_histogram(aes(PSD_nm)) + facet_grid(season ~ .)
  y <- ggplot_build(y)
  y <- y$data[[1]]$y %>% max()
  
  tx <- s %>%
    group_by(season) %>%
    dplyr::summarise(N = n(),
                     max_dist = paste0(round(max(PSD_nm),2), " nm"),
                     med_dist = paste0(round(median(PSD_nm),2), " nm"),
                     mean_dist = paste0(round(mean(PSD_nm),2), " nm"))
  
  g <-  g  +    geom_text(data = tx, aes(x = (0.75 * max(s$PSD_nm)), y = y, label = paste0("N = ", N))) +
    geom_text(data = tx, aes(x = (0.75 * max(s$PSD_nm)), y = 0.80 * y, label = paste0("Max distance = ", max_dist))) +
    geom_text(data = tx, aes(x = (0.75 * max(s$PSD_nm)), y = 0.60 * y, label = paste0("Median distance = ", med_dist))) +
    geom_text(data = tx, aes(x = (0.75 * max(s$PSD_nm)), y = 0.40 * y, label = paste0("Mean distance = ", mean_dist))) +
    facet_wrap( ~factor(season, levels = c("Winter", "Spring", "Summer", "Fall")))
g
ggsave("output_hist/hw all seasonal dist to oct 2021.png")

# t <- plot_hist(all_ap_sf, "Humpback")
# t+facet_wrap( ~factor(season, levels = c("Winter", "Spring", "Summer", "Fall")))


#-------------------------------------------------------------------------
# --------------- conditions
#-------------------------------------------------------------------------
ggplot() +
  geom_sf(data = coast_file, stroke = 0.05, fill = "light yellow", colour = "grey 60")+
  geom_sf(data = all_effort_lines, stroke = 2, aes(colour = factor(Bf))) +
  scale_colour_manual(values = c("blue", "green", "yellow", "orange", "red", "purple"))+
    coord #+
  facet_wrap(~factor(season, levels = c("Winter", "Spring", "Summer", "Fall")))
ggsave("output_maps/all bf lines to oct 2021.png")

ggplot() +
  geom_histogram(data = all_effort_lines, aes(Bf)) 

all_effort_lines %<>% mutate(length_km = as.numeric(st_length(geometry))/1000)
ggplot() +
  geom_boxplot(data = all_effort_lines, aes(group = as.character(Bf), length_km)) 
  
ggsave("output_hist/bf hist to oct 2021.png")

ggplot() +
  geom_histogram(data = all_effort_lines, aes(Bf)) +
  facet_wrap(~factor(season, levels = c("Winter", "Spring", "Summer", "Fall")))

ggsave("output_hist/bf hist seasonal to oct 2021.png")


x <- all_effort_lines %>% mutate(length = st_length(geometry))
y <- x %>% dplyr::select(year, month, length)
y <- x %>% dplyr::select(year, month, length) %>% as.data.frame()
y <- x %>% dplyr::select(year, month, length) %>% st_as_sf()
y <- x %>% dplyr::select(year, month, length) %>% data.frame()
y <- data.frame(year = x$year, month_abb= x$month_abb, length = x$length) %>%
  group_by(year, month_abb) %>% 
  summarise(total_survey_length_km = sum(length/1000))
