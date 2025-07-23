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

#Reading datasets#

setwd("")
gunshots<-read.csv("gunshots.csv")
setwd("")
RJ_data<-read.csv("RJ_ALL_VARIABLES.csv")
PE_data<-read.csv("PE_ALL_VARIABLES.csv")


#Histograms#
hist_RJ<-ggplot() +
  geom_density(aes(x = RJ_data$share_security_2014, color = "2014"), size = 1) +
  geom_density(aes(x = RJ_data$share_security_2018, color = "2018"), size = 1) +
  theme_bw() +
  labs(
    x = "", 
    y= "",
    color = "Year",
    title= "Rio de Janeiro Metro Area"
  )+
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size=11)
  )



hist_PE<-ggplot() +
  geom_density(aes(x = PE_data$share_security_2014, color = "2014"), size = 1) +
  geom_density(aes(x = PE_data$share_security_2018, color = "2018"), size = 1) +
  theme_bw() +
  labs(
    x = "", 
    y= "",
    color = "Year",
    title= "Recife Metro Area"
  )+
  scale_x_continuous(limits = c(0, 0.25)) +
  theme(
    plot.title = element_text(hjust = 0.5, size=11)
  )

histogram_plot<-(hist_RJ + hist_PE)+
  plot_annotation(
    theme = theme(
      plot.title = element_text(hjust = 0.5)
    )
  ) & 
  theme(
    plot.margin = margin(5, 5, 5, 5),
    axis.title.x = element_blank()
  )




#Time Series# 

Recife_occurrence<-gunshots%>%filter(date>="2016-01-01" & date<="2020-12-31" & 
                                       state=="Pernambuco", policeAction=="False")
daily_counts_PE <- Recife_occurrence %>%
  mutate(date_day = as.Date(date)) %>%
  mutate(month = floor_date(as.Date(date), "month")) %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  ungroup()%>%
  mutate(Location = "Greater Recife")

RJ_occurrence<-gunshots%>%filter(date>="2016-01-01" & date<="2020-12-31" & 
                                   state=="Rio de Janeiro", policeAction=="False")
daily_counts_RJ <- RJ_occurrence %>%
  mutate(date_day = as.Date(date)) %>%
  mutate(month = floor_date(as.Date(date), "month")) %>%
  group_by(month) %>%
  summarise(count = n()) %>%
  ungroup()%>%
  mutate(Location = "Greater Rio de Janeiro")

daily_counts <- bind_rows(daily_counts_PE, daily_counts_RJ)

Sys.setlocale("LC_TIME", "C")
Time_Series <- ggplot(daily_counts, aes(x = month, y = count, color = Location)) +
  geom_line(size = 1) +
  geom_vline(
    xintercept = as.numeric(as.Date("2018-10-07")), 
    linetype = "dashed", 
    color = "black"
  ) +
  annotate(
    "text",
    x = as.Date("2018-10-07"),
    y = max(daily_counts$count, na.rm = TRUE) * 0.95, 
    label = "2018 General Elections",
    angle = 0,
    vjust = -0.5,
    hjust = -0.05,
    color = "black",
    size = 3.5
  ) +
  theme_bw() +
  labs(
    x = "",
    y = "",
    color = "Location"
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b-%y"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

Time_Series


#Scatter plots#

RJ_data<-RJ_data%>%mutate(RJ=1)
PE_data<-PE_data%>%mutate(RJ=0)

polling_station<-bind_rows(RJ_data, PE_data)%>%
  mutate(delta=share_security_2018-share_security_2014)%>%
  select(RJ, delta, total_nonpolice_2018_500, total_police_2018_500)

  total_gunshot_graph<-ggplot(polling_station, aes(x = total_nonpolice_2018_500, 
                                                   y = delta, color = as.factor(RJ))) +
    geom_point() +
    theme_bw() +
    labs(
      x = "Gunshots within a 500-Meter Radius of Polling Station", 
      y = "Percentage Points",
      color = "Metro Area"
    ) +
    scale_color_manual(
      values = c("0" = "royalblue4", "1" = "tomato3"),
      labels = c("Greater Recife", "Greater Rio de Janeiro")
    ) +
    scale_x_continuous(limits = c(0, 50)) +
    theme(
      plot.title = element_text(hjust = 0.5)
    )


police_gunshot_graph<-ggplot(polling_station, aes(x = total_police_2018_500, 
                                                 y = delta, color = as.factor(RJ))) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Police Gunshots within a 500-Meter Radius of Polling Station", 
    y = "",
    title = "Police Violence and the Increase in Vote Share for Law and Order Candidates",
    color = "Metro Area"
  ) +
  scale_color_manual(
    values = c("0" = "royalblue4", "1" = "tomato3"),
    labels = c("Greater Recife", "Greater Rio de Janeiro")
  ) +
  scale_x_continuous(limits = c(0, 50)) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
