
pacman::p_load(sf,
               estimatr,
               lubridate,
               geosphere,
               tidyverse,
               terra,
               ggplot2,
               broom,
               dplyr,
               spData,
               tibble,
               knitr,
               kableExtra,
               patchwork)


setwd("")

RJ_data<-read.csv("RJ_ALL_VARIABLES.csv")
PE_data<-read.csv("PE_ALL_VARIABLES.csv")


RJ_timing<-RJ_data%>%
  mutate(delta=share_security_2018-share_security_2014, RJ=1, 
         violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                             0.5),1,0))%>%
  select(RJ, delta, row_id, lat.x, long.x, two_weeks_300_2018_beforeTRUE,
         two_weeks_500_2018_beforeTRUE, two_weeks_300_2018_beforeFALSE,
         two_weeks_500_2018_beforeFALSE, one_week_300_2018_beforeTRUE,
         one_week_500_2018_beforeTRUE, one_week_300_2018_beforeFALSE,
         one_week_500_2018_beforeFALSE, violent_area)%>%
  filter(!is.na(delta))

PE_timing<-PE_data%>%
  mutate(delta=share_security_2018-share_security_2014, RJ=0, 
         violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  select(RJ, delta, row_id, lat.x, long.x, two_weeks_300_2018_beforeTRUE,
         two_weeks_500_2018_beforeTRUE, two_weeks_300_2018_beforeFALSE,
         two_weeks_500_2018_beforeFALSE, one_week_300_2018_beforeTRUE,
         one_week_500_2018_beforeTRUE, one_week_300_2018_beforeFALSE,
         one_week_500_2018_beforeFALSE, violent_area)%>%
  filter(!is.na(delta))


setwd("")
gunshots_RJ<-read.csv("gunshots.csv")%>%filter(date<="2019-04-07" & date>="2018-04-07")%>%
  filter(policeAction=="False", state=="Rio de Janeiro")
gunshots_PE<-read.csv("gunshots.csv")%>%filter(date<="2019-04-07" & date>="2018-04-07")%>%
  filter(policeAction=="False", state=="Pernambuco")

PE_sf <- PE_timing %>%
  st_as_sf(coords = c("long.x", "lat.x"), crs = 4326) %>%
  st_transform(31983)

gunshots_PE_sf <- gunshots_PE %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(31983)

RJ_sf <- RJ_timing %>%
  st_as_sf(coords = c("long.x", "lat.x"), crs = 4326) %>%
  st_transform(31983)

gunshots_RJ_sf <- gunshots_RJ %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(31983)
target_date <- as.Date("2018-10-07")
PE_timing$occurrence300 <- NA
RJ_timing$occurrence300<-NA
PE_timing$occurrence500 <- NA
RJ_timing$occurrence500<-NA

for (i in 1:nrow(PE_sf)) {
  point_i <- PE_sf[i, ]
  distances <- as.numeric(st_distance(point_i, gunshots_PE_sf))
  within_radius <- which(distances <= 300)
  dates_within_radius <- gunshots_PE_sf[within_radius, ]$date
  if (length(dates_within_radius) > 0) {
    dates_converted <- as.Date(dates_within_radius)
    closest_date <- dates_converted[which.min(abs(dates_converted - target_date))]
    PE_timing$occurrence300[i] <-  as.character(closest_date)
  } else {
    PE_timing$occurrence300[i] <- NA
  }
}

for (i in 1:nrow(PE_sf)) {
  point_i <- PE_sf[i, ]
  distances <- as.numeric(st_distance(point_i, gunshots_PE_sf))
  within_radius <- which(distances <= 500)
  dates_within_radius <- gunshots_PE_sf[within_radius, ]$date
  if (length(dates_within_radius) > 0) {
    dates_converted <- as.Date(dates_within_radius)
    closest_date <- dates_converted[which.min(abs(dates_converted - target_date))]
    PE_timing$occurrence500[i] <-  as.character(closest_date)
  } else {
    PE_timing$occurrence500[i] <- NA
  }
}

for (i in 1:nrow(RJ_sf)) {
  point_i <- RJ_sf[i, ]
  distances <- as.numeric(st_distance(point_i, gunshots_RJ_sf))
  within_radius <- which(distances <= 300)
  dates_within_radius <- gunshots_RJ_sf[within_radius, ]$date
  if (length(dates_within_radius) > 0) {
    dates_converted <- as.Date(dates_within_radius)
    closest_date <- dates_converted[which.min(abs(dates_converted - target_date))]
    RJ_timing$occurrence300[i] <-  as.character(closest_date)
  } else {
    RJ_timing$occurrence300[i] <- NA
  }
}



for (i in 1:nrow(RJ_sf)) {
  point_i <- RJ_sf[i, ]
  distances <- as.numeric(st_distance(point_i, gunshots_RJ_sf))
  within_radius <- which(distances <= 500)
  dates_within_radius <- gunshots_RJ_sf[within_radius, ]$date
  if (length(dates_within_radius) > 0) {
    dates_converted <- as.Date(dates_within_radius)
    closest_date <- dates_converted[which.min(abs(dates_converted - target_date))]
    RJ_timing$occurrence500[i] <-  as.character(closest_date)
  } else {
    RJ_timing$occurrence500[i] <- NA
  }
}


Timing<-bind_rows(RJ_timing, PE_timing)


Timing<-Timing%>%mutate(
  running300=as.numeric(as.Date(occurrence300)-target_date),
  treat300=ifelse(running300<0,1,0),
  running500=as.numeric(as.Date(occurrence500)-target_date),
  treat500=ifelse(running500<0,1,0),
)



Figure1<-Timing  %>% filter(abs(running500)<=60,
                   (two_weeks_500_2018_beforeTRUE==0 | two_weeks_500_2018_beforeFALSE==0),
                   violent_area==1) %>%
  ggplot(aes(x=running500, y=delta, colour=as.factor(treat500))) + 
  stat_summary_bin(fun='mean', bins = 60, size=1.5, geom='point') + geom_smooth(
    method="lm_robust", formula = y ~ x, data= Timing%>% filter(abs(running500) <= 30,
                                                                (two_weeks_500_2018_beforeTRUE==0 | two_weeks_500_2018_beforeFALSE==0),
                                                                violent_area==1), se=FALSE) +
  geom_vline(xintercept=0, lty=2) + geom_vline(xintercept=-30, lty=3) + geom_vline(xintercept=30, lty=3)+ 
  theme_bw() + 
  theme(legend.position="none",
        axis.title.x = element_blank(),  
        axis.title.y = element_blank()) +
  ggtitle("Aggregated and 500m Radius")




Figure2<-Timing  %>% filter(abs(running300)<=60,
                            (two_weeks_300_2018_beforeTRUE==0 | two_weeks_300_2018_beforeFALSE==0),
                            violent_area==1) %>%
  ggplot(aes(x=running500, y=delta, colour=as.factor(treat500))) + 
  stat_summary_bin(fun='mean', bins = 60, size=1.5, geom='point') + geom_smooth(
    method="lm_robust", formula = y ~ x, data= Timing%>% filter(abs(running300) <= 30,
                                                                (two_weeks_300_2018_beforeTRUE==0 | two_weeks_300_2018_beforeFALSE==0),
                                                                violent_area==1), se=FALSE) +
  geom_vline(xintercept=0, lty=2) + geom_vline(xintercept=-30, lty=3) + geom_vline(xintercept=30, lty=3)+ 
  theme_bw() + 
  theme(legend.position="none",
        axis.title.x = element_blank(),  
        axis.title.y = element_blank()) +
  ggtitle("Aggregated and 300m Radius")
  
  
aggregated_plot <- (Figure1 + Figure2) + 
  plot_annotation(
    caption = "Days from the 2018 General Election of Closest Gunshot Occurence",
    theme = theme(
      plot.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5, size = 12, margin = margin(t = 10))
    )
  ) & 
  theme(
    plot.margin = margin(5, 5, 5, 5),
    axis.title.x = element_blank()
  )




Figure3<-Timing  %>% filter(abs(running500)<=60,
                            (two_weeks_500_2018_beforeTRUE==0 | two_weeks_500_2018_beforeFALSE==0),
                            violent_area==1, RJ==1) %>%
  ggplot(aes(x=running500, y=delta, colour=as.factor(treat500))) + 
  stat_summary_bin(fun='mean', bins = 60, size=1.5, geom='point') + geom_smooth(
    method="lm_robust", formula = y ~ x, data= Timing%>% filter(abs(running500) <= 30,
                                                                (two_weeks_500_2018_beforeTRUE==0 | two_weeks_500_2018_beforeFALSE==0),
                                                                violent_area==1, RJ==1), se=FALSE) +
  geom_vline(xintercept=0, lty=2) + geom_vline(xintercept=-30, lty=3) + geom_vline(xintercept=30, lty=3)+ 
  theme_bw() + 
  theme(legend.position="none",
        axis.title.x = element_blank(),  
        axis.title.y = element_blank()) +
  ggtitle("RJ and 500m Radius")




Figure4<-Timing  %>% filter(abs(running300)<=60,
                            (two_weeks_300_2018_beforeTRUE==0 | two_weeks_300_2018_beforeFALSE==0),
                            violent_area==1, RJ==1) %>%
  ggplot(aes(x=running500, y=delta, colour=as.factor(treat500))) + 
  stat_summary_bin(fun='mean', bins = 60, size=1.5, geom='point') + geom_smooth(
    method="lm_robust", formula = y ~ x, data= Timing%>% filter(abs(running300) <= 30,
                                                                (two_weeks_300_2018_beforeTRUE==0 | two_weeks_300_2018_beforeFALSE==0),
                                                                violent_area==1, RJ==1), se=FALSE) +
  geom_vline(xintercept=0, lty=2) + geom_vline(xintercept=-30, lty=3) + geom_vline(xintercept=30, lty=3)+ 
  theme_bw() + 
  theme(legend.position="none",
        axis.title.x = element_blank(),  
        axis.title.y = element_blank()) +
  ggtitle("RJ and 300m Radius")


RJ_plot <- (Figure3 + Figure4) + 
  plot_annotation(
    caption = "Days from the 2018 General Election of Closest Gunshot Occurence",
    theme = theme(
      plot.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5, size = 12, margin = margin(t = 10))
    )
  ) & 
  theme(
    plot.margin = margin(5, 5, 5, 5),
    axis.title.x = element_blank()
  )

Figure5<-Timing  %>% filter(abs(running500)<=60,
                            (two_weeks_500_2018_beforeTRUE==0 | two_weeks_500_2018_beforeFALSE==0),
                            violent_area==1, RJ==0) %>%
  ggplot(aes(x=running500, y=delta, colour=as.factor(treat500))) + 
  stat_summary_bin(fun='mean', bins = 60, size=1.5, geom='point') + geom_smooth(
    method="lm_robust", formula = y ~ x, data= Timing%>% filter(abs(running500) <= 30,
                                                                (two_weeks_500_2018_beforeTRUE==0 | two_weeks_500_2018_beforeFALSE==0),
                                                                violent_area==1, RJ==0), se=FALSE) +
  geom_vline(xintercept=0, lty=2) + geom_vline(xintercept=-30, lty=3) + geom_vline(xintercept=30, lty=3)+ 
  theme_bw() + 
  theme(legend.position="none",
        axis.title.x = element_blank(),  
        axis.title.y = element_blank()) +
  ggtitle("PE and 500m Radius")




Figure6<-Timing  %>% filter(abs(running300)<=60,
                            (two_weeks_300_2018_beforeTRUE==0 | two_weeks_300_2018_beforeFALSE==0),
                            violent_area==1, RJ==0) %>%
  ggplot(aes(x=running500, y=delta, colour=as.factor(treat500))) + 
  stat_summary_bin(fun='mean', bins = 60, size=1.5, geom='point') + geom_smooth(
    method="lm_robust", formula = y ~ x, data= Timing%>% filter(abs(running300) <= 30,
                                                                (two_weeks_300_2018_beforeTRUE==0 | two_weeks_300_2018_beforeFALSE==0),
                                                                violent_area==1, RJ==0), se=FALSE) +
  geom_vline(xintercept=0, lty=2) + geom_vline(xintercept=-30, lty=3) + geom_vline(xintercept=30, lty=3)+ 
  theme_bw() + 
  theme(legend.position="none",
        axis.title.x = element_blank(),  
        axis.title.y = element_blank()) +
  ggtitle("PE and 300m Radius")


PE_plot <- (Figure5 + Figure6) + 
  plot_annotation(
    caption = "Days from the 2018 General Election of Closest Gunshot Occurence",
    theme = theme(
      plot.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5, size = 12, margin = margin(t = 10))
    )
  ) & 
  theme(
    plot.margin = margin(5, 5, 5, 5),
    axis.title.x = element_blank()
  )

Disc_plot <- (aggregated_plot + RJ_plot + PE_plot)




    


