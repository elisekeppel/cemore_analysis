# #---------------------------------------------------------------
# # -------------- plot sighting distance histograms ---------------------
# #---------------------------------------------------------------
# plot_hist <- function(data, sp = NULL){
#   title <- paste0("All CeMoRe surveys from Aug 2020 - ", survey_title)
#   col <- (RColorBrewer::brewer.pal(9L, "Blues")[4:7])
#   col_ramp <- colorRampPalette(col)
#   
#   s <- data %>%
#     filter(!is.na(PSD_nm))
#   if(!is.null(sp)) s %<>% filter(Species %like% sp)
#   
#   # to get y value for height of plot text
#   y <- ggplot(data = s) + geom_histogram(aes(PSD_nm)) 
#   y <- ggplot_build(y)
#   y <- y$data[[1]]$y %>% max()
#   
#   # to create text for facet grid (for non-facetted plot, group by 'Event') to get only one line for plot text
#   # to facet by second variable, add desired variable to 'group_by' here
#   
#   tx <- s %>%
#     # group_by(Event) %>%
#     group_by(Species) %>%
#     dplyr::summarise(N = n(),
#               max_dist = paste0(round(max(PSD_nm),2), " nm"),
#               med_dist = paste0(round(median(PSD_nm),2), " nm"),
#               mean_dist = paste0(round(mean(PSD_nm),2), " nm"))
#   
#   g <- ggplot() +
#     geom_histogram(data = s, aes(PSD_nm, fill = as.character(beauf)), bins = 30) +
#     scale_fill_manual(name ="Beaufort",values= col_ramp(5)) +
#     ggtitle(paste0("Distances to ", sp, " observations"), subtitle = paste0("Aug 2020 to ", survey_title)) +
#     xlab("Distance (nm)") +
#     ylab("Count") +
#     geom_text(data = tx, aes(x = (0.75 * max(s$PSD_nm)), y = y, label = paste0("N = ", N))) +
#     geom_text(data = tx, aes(x = (0.75 * max(s$PSD_nm)), y = 0.80 * y, label = paste0("Max distance = ", max_dist))) +
#     geom_text(data = tx, aes(x = (0.75 * max(s$PSD_nm)), y = 0.60 * y, label = paste0("Median distance = ", med_dist))) +
#     geom_text(data = tx, aes(x = (0.75 * max(s$PSD_nm)), y = 0.40 * y, label = paste0("Mean distance = ", mean_dist))) 
#   
#   g
# }
  
  
  
  
  #---------------------------------------------------------------
  #--------  VERSION WITH BY_SEASON ARGUMENT IS WORKING  ---------
  #---------------------------------------------------------------
  
  plot_hist <- function(data, sp = NULL, by_season = T, n_bins = 30){
    title <- paste0("All CeMoRe surveys from Aug 2020 - ", survey_title)
    col_bf <- rev(RColorBrewer::brewer.pal(11L, "Spectral")[2:11])
    col_ramp_bf <- colorRampPalette(col_bf)
    
    s <- data %>%
      filter(!is.na(PSD_nm))
    if(!is.null(sp)) s %<>% filter(Species %like% sp)
    
    g <- ggplot() +
      geom_histogram(data = s, aes(PSD_nm, fill = as.character(beauf)), binwidth = 0.1, center = 0.05) +
      
                     # ggplot() +
                     #   geom_histogram(data = hw, aes(PSD_nm, fill = as.character(beauf)),
                     #                  binwidth = 0.1, center = 0.05)+
                     #   # scale_x_continuous(breaks = seq(0,4,0.5))
                     #   xlim(0,4)
                     
      scale_fill_manual(name ="Beaufort",values= col_ramp(5)) +
      ggtitle(paste0("Distances to ", sp, " observations")) + #, subtitle = paste0("Aug 2020 to ", survey_title)
      xlab("Distance (nm)") +
      ylab("Count") 
    
    # to create text for facet grid (for non-facetted plot, group by 'Event') to get only one line for plot text
    # to facet by second variable, add desired variable to 'group_by' here
    if(!by_season){
      # to get y value for height of plot text
      y <- ggplot(data = s) + geom_histogram(aes(PSD_nm)) 
      y <- ggplot_build(y)
      y <- y$data[[1]]$y %>% max()
      
      tx <- s %>%
        group_by(Species) %>%
        dplyr::summarise(N = n(),
                         max_dist = paste0(round(max(PSD_nm),2), " nm"),
                         med_dist = paste0(round(median(PSD_nm),2), " nm"),
                         mean_dist = paste0(round(mean(PSD_nm),2), " nm"))
      
      g <-  g  +    geom_text(data = tx, aes(x = (0.75 * max(s$PSD_nm)), y = y, label = paste0("N = ", N))) +
        geom_text(data = tx, aes(x = (0.75 * max(s$PSD_nm)), y = 0.80 * y, label = paste0("Max distance = ", max_dist))) +
        geom_text(data = tx, aes(x = (0.75 * max(s$PSD_nm)), y = 0.60 * y, label = paste0("Median distance = ", med_dist))) +
        geom_text(data = tx, aes(x = (0.75 * max(s$PSD_nm)), y = 0.40 * y, label = paste0("Mean distance = ", mean_dist))) 
      
    }else{
      # to get y value for height of plot text
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
        facet_grid(season~.)
    }
    
    
    g +  geom_text(data = all_ap_sf, aes(x = (1 * max(s$PSD_nm)), y = 1 * y, label = paste0("Bins = ", n_bins))) 

  }
  #---------------------------------------------------------------
  #---------------------------------------------------------------
  #---------------------------------------------------------------
  # 
# # for single N, max/mean/median (= non-facetted)
#   # annotate("text", label = paste0("N = ", nrow(s)), x = (0.75 * max(s$PSD_nm)), y = y) +#
#   # annotate("text", label = paste0("Max distance = ", paste0(round(max(s$PSD_nm),2), " nm")), x = (0.75 * max(s$PSD_nm)), y = 0.93 * y) +
#   # annotate("text", label = paste0("Median distance = ", paste0(round(median(s$PSD_nm),2), " nm")), x = (0.75 * max(s$PSD_nm)), y = 0.85 * y) +
#   # annotate("text", label = paste0("Mean distance = ", paste0(round(mean(s$PSD_nm),2), " nm")), x = (0.75 * max(s$PSD_nm)), y = 0.76 * y)
#   
# s %>% group_by(beauf) %>% summarise(n = n())
# ggsave(paste0("output_hist/ssec/", survey_title, " ", sp, " seasonal sighting distance hist.png"))
# # ----------------------------------------------------------------------

  # 
  # To label the actual spans, not the middle of the bar, which is what you need for something like an age histogram, use something like this:
  #   
  #   ggplot(Df, aes(Age)) +
  #   geom_histogram(
  #     breaks = seq(10, 90, by = 10), 
  #     aes(fill = ..count..,
  #         colour = "black")) + 
  #   scale_x_continuous(breaks = seq(10, 90, by=10))