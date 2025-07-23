#Loading Libraries
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
               cobalt)

#Loading datasets
file_path<-""
setwd(file_path)
RJ_panel<-read.csv("RJ_panel.csv")

file_path<-""
setwd(file_path)
PE_panel<-read.csv("PE_panel.csv")

file_path<-""
setwd(file_path)
gunshots<-read.csv("gunshots.csv")


RJ_loc<-RJ_panel%>%select(NR_ZONA,NR_LOCAL_VOTACAO, cidade, nome_local, lat, long)
PE_loc<-PE_panel%>%select(NR_ZONA,NR_LOCAL_VOTACAO, cidade, nome_local, lat, long)


PE_gunshots<-gunshots%>%filter(state=="Pernambuco")
RJ_gunshots<-gunshots%>%filter(state=="Rio de Janeiro")

non_police_PE<-PE_gunshots%>%filter(policeAction=="False")
non_police_RJ<-RJ_gunshots%>%filter(policeAction=="False")
police_PE<-PE_gunshots%>%filter(policeAction=="True")
police_RJ<-RJ_gunshots%>%filter(policeAction=="True")


RJ_loc <- RJ_loc %>%
  mutate(row_id = row_number())
RJ_loc_sf <- st_as_sf(RJ_loc, coords = c("long", "lat"), crs = 4326)
RJ_loc_proj <- st_transform(RJ_loc_sf, 31983)


PE_loc <- PE_loc %>%
  mutate(row_id = row_number())
PE_loc_sf <- st_as_sf(PE_loc, coords = c("long", "lat"), crs = 4326)
PE_loc_proj <- st_transform(PE_loc_sf, 31983)


####Creating counts for 2018#####

#creating parameter df 

parameters_2018 <- tibble(
  before = logical(),
  time = character(),
  start_date = as.POSIXct(character()),
  end_date = as.POSIXct(character()),
  distance = numeric()
)

before_vec <- rep(c(TRUE, FALSE), times = 24)
time_vec <- rep(c("one_week", "two_weeks", "three_weeks", "two_weeks"), each = 12)
distance_vec <- rep(c(300, 300, 300, 300, 1000, 1000, 1000, 1000, 1200, 1200,
                      1300, 1300), times = 4)

start_date_vec <- as.POSIXct(unlist(mapply(function(b, t) {
  if (!b) {
    ymd_hms("2018-10-07 18:00:00")
  } else {
    case_when(
      t == "one_week"   ~ ymd_hms("2018-09-30 00:00:00"),
      t == "two_weeks" ~ ymd_hms("2018-09-23 00:00:00"),
      t == "three_weeks"   ~ ymd_hms("2018-09-16 00:00:00"),
      TRUE              ~ ymd_hms("2018-09-07 00:00:00")
    )
  }
}, before_vec, time_vec, SIMPLIFY = FALSE)), origin = "1970-01-01", tz = "UTC")

end_date_vec <- as.POSIXct(unlist(mapply(function(b, t) {
  if (b) {
    ymd_hms("2018-10-07 08:00:00")
  } else {
    case_when(
      t == "one_week"   ~ ymd_hms("2018-10-14 23:59:59"),
      t == "two_weeks" ~ ymd_hms("2018-10-21 23:59:59"),
      t == "three_weeks"   ~ ymd_hms("2018-10-28 23:59:59"),
      TRUE              ~ ymd_hms("2018-11-07 23:59:59")
    )
  }
}, before_vec, time_vec, SIMPLIFY = FALSE)), origin = "1970-01-01", tz = "UTC")

parameters_2018 <- parameters_2018 %>%
  add_row(
    before = before_vec,
    time = time_vec,
    start_date = start_date_vec,
    end_date = end_date_vec,
    distance = distance_vec
  )



#Creating counts for RJ 
for (i in 1:nrow(parameters_2018)) {
  before<-parameters_2018$before[i]
  time_label <- parameters_2018$time[i]
  start_date <- parameters_2018$start_date[i]
  end_date<-parameters_2018$end_date[i]
  distance <- parameters_2018$distance[i]
  RJ_gunshots_sf <- non_police_RJ %>%
    filter(date >= start_date, date <= end_date) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  RJ_gunshots_proj <- st_transform(RJ_gunshots_sf, 31983)
  RJ_loc_buffer <- st_buffer(RJ_loc_proj, dist = distance)
  intersection_list <- st_intersects(RJ_loc_buffer, RJ_gunshots_proj)
  new_col <- paste0(time_label, "_", distance, "_2018", "_before", before)
  RJ_loc[[new_col]] <- lengths(intersection_list)
}


#Creating counts for PE 
for (i in 1:nrow(parameters_2018)) {
  before<-parameters_2018$before[i]
  time_label <- parameters_2018$time[i]
  start_date <- parameters_2018$start_date[i]
  end_date<-parameters_2018$end_date[i]
  distance <- parameters_2018$distance[i]
  PE_gunshots_sf <- non_police_PE %>%
    filter(date >= start_date, date <= end_date) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  PE_gunshots_proj <- st_transform(PE_gunshots_sf, 31983)
  PE_loc_buffer <- st_buffer(PE_loc_proj, dist = distance)
  intersection_list <- st_intersects(PE_loc_buffer, PE_gunshots_proj)
  new_col <- paste0(time_label, "_", distance, "_2018", "_before", before)
  PE_loc[[new_col]] <- lengths(intersection_list)
}


####Creating counts for 2022#####

parameters_2022 <- tibble(
  before = logical(),
  time = character(),
  start_date = as.POSIXct(character()),
  end_date = as.POSIXct(character()),
  distance = numeric()
)


start_date_vec <- as.POSIXct(unlist(mapply(function(b, t) {
  if (!b) {
    ymd_hms("2022-10-02 18:00:00")
  } else {
    case_when(
      t == "one_week"   ~ ymd_hms("2022-09-25 00:00:00"),
      t == "two_weeks" ~ ymd_hms("2022-09-18 00:00:00"),
      t == "three_weeks"   ~ ymd_hms("2022-09-11 00:00:00"),
      TRUE              ~ ymd_hms("2022-09-02 00:00:00")
    )
  }
}, before_vec, time_vec, SIMPLIFY = FALSE)), origin = "1970-01-01", tz = "UTC")

end_date_vec <- as.POSIXct(unlist(mapply(function(b, t) {
  if (b) {
    ymd_hms("2022-10-02 08:00:00")
  } else {
    case_when(
      t == "one_week"   ~ ymd_hms("2022-10-09 23:59:59"),
      t == "two_weeks" ~ ymd_hms("2022-10-16 23:59:59"),
      t == "three_weeks"   ~ ymd_hms("2022-10-23 23:59:59"),
      TRUE              ~ ymd_hms("2022-11-02 23:59:59")
    )
  }
}, before_vec, time_vec, SIMPLIFY = FALSE)), origin = "1970-01-01", tz = "UTC")

parameters_2022 <- parameters_2022 %>%
  add_row(
    before = before_vec,
    time = time_vec,
    start_date = start_date_vec,
    end_date = end_date_vec,
    distance = distance_vec
  )


#Creating counts for RJ 
for (i in 1:nrow(parameters_2022)) {
  before<-parameters_2022$before[i]
  time_label <- parameters_2022$time[i]
  start_date <- parameters_2022$start_date[i]
  end_date<-parameters_2022$end_date[i]
  distance <- parameters_2022$distance[i]
  RJ_gunshots_sf <- non_police_RJ %>%
    filter(date >= start_date, date <= end_date) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  RJ_gunshots_proj <- st_transform(RJ_gunshots_sf, 31983)
  RJ_loc_buffer <- st_buffer(RJ_loc_proj, dist = distance)
  intersection_list <- st_intersects(RJ_loc_buffer, RJ_gunshots_proj)
  new_col <- paste0(time_label, "_", distance, "_2022", "_before", before)
  RJ_loc[[new_col]] <- lengths(intersection_list)
}


#Creating counts for PE 
for (i in 1:nrow(parameters_2022)) {
  before<-parameters_2022$before[i]
  time_label <- parameters_2022$time[i]
  start_date <- parameters_2022$start_date[i]
  end_date<-parameters_2022$end_date[i]
  distance <- parameters_2022$distance[i]
  PE_gunshots_sf <- non_police_PE %>%
    filter(date >= start_date, date <= end_date) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  PE_gunshots_proj <- st_transform(PE_gunshots_sf, 31983)
  PE_loc_buffer <- st_buffer(PE_loc_proj, dist = distance)
  intersection_list <- st_intersects(PE_loc_buffer, PE_gunshots_proj)
  new_col <- paste0(time_label, "_", distance, "_2022", "_before", before)
  PE_loc[[new_col]] <- lengths(intersection_list)
}


###Creating final dataset and exporting it 
Recife_Metro_data <- PE_loc %>%
  left_join(PE_panel, by = c("NR_ZONA", "NR_LOCAL_VOTACAO"))

RJ_Metro_data <- RJ_loc %>%
  left_join(RJ_panel, by = c("NR_ZONA", "NR_LOCAL_VOTACAO"))


###Creating total violence variables 

#Non police gunshot 
start_date <- as.POSIXct("2016-07-05", tz = "UTC")
end_date <- as.POSIXct("2018-10-07 18:00:00", tz = "UTC")
distances <- c(300, 300, 1000, 1000, 1200, 13000)

non_police_PE_filtered <- non_police_PE %>%
  filter(date >= start_date, date <= end_date) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(31983)

non_police_RJ_filtered <- non_police_RJ %>%
  filter(date >= start_date, date <= end_date) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(31983)


for (distance in distances) {
  buffer <- st_buffer(PE_loc_proj, dist = distance)
  intersected <- st_intersects(buffer, non_police_PE_filtered)
  new_col <- paste0("total_nonpolice_2018_", distance)
  Recife_Metro_data[[new_col]] <- lengths(intersected)
}

for (distance in distances) {
  buffer <- st_buffer(RJ_loc_proj, dist = distance)
  intersected <- st_intersects(buffer, non_police_RJ_filtered)
  new_col <- paste0("total_nonpolice_2018_", distance)
  RJ_Metro_data[[new_col]] <- lengths(intersected)
}

end_date <- as.POSIXct("2022-10-02 18:00:00", tz = "UTC")

non_police_PE_filtered <- non_police_PE %>%
  filter(date >= start_date, date <= end_date) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(31983)

non_police_RJ_filtered <- non_police_RJ %>%
  filter(date >= start_date, date <= end_date) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(31983)


for (distance in distances) {
  buffer <- st_buffer(PE_loc_proj, dist = distance)
  intersected <- st_intersects(buffer, non_police_PE_filtered)
  new_col <- paste0("total_nonpolice_2022_", distance)
  Recife_Metro_data[[new_col]] <- lengths(intersected)
}

for (distance in distances) {
  buffer <- st_buffer(RJ_loc_proj, dist = distance)
  intersected <- st_intersects(buffer, non_police_RJ_filtered)
  new_col <- paste0("total_nonpolice_2022_", distance)
  RJ_Metro_data[[new_col]] <- lengths(intersected)
}


#Police gunshot 

start_date <- as.POSIXct("2016-07-05", tz = "UTC")
end_date <- as.POSIXct("2018-10-07 18:00:00", tz = "UTC")

police_PE_filtered <- police_PE %>%
  filter(date >= start_date, date <= end_date) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(31983)

police_RJ_filtered <- police_RJ %>%
  filter(date >= start_date, date <= end_date) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(31983)


for (distance in distances) {
  buffer <- st_buffer(PE_loc_proj, dist = distance)
  intersected <- st_intersects(buffer, police_PE_filtered)
  new_col <- paste0("total_police_2018_", distance)
  Recife_Metro_data[[new_col]] <- lengths(intersected)
}

for (distance in distances) {
  buffer <- st_buffer(RJ_loc_proj, dist = distance)
  intersected <- st_intersects(buffer, police_RJ_filtered)
  new_col <- paste0("total_police_2018_", distance)
  RJ_Metro_data[[new_col]] <- lengths(intersected)
}

end_date <- as.POSIXct("2022-10-02 18:00:00", tz = "UTC")

police_PE_filtered <- police_PE %>%
  filter(date >= start_date, date <= end_date) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(31983)

police_RJ_filtered <- police_RJ %>%
  filter(date >= start_date, date <= end_date) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(31983)


for (distance in distances) {
  buffer <- st_buffer(PE_loc_proj, dist = distance)
  intersected <- st_intersects(buffer, police_PE_filtered)
  new_col <- paste0("total_police_2022_", distance)
  Recife_Metro_data[[new_col]] <- lengths(intersected)
}

for (distance in distances) {
  buffer <- st_buffer(RJ_loc_proj, dist = distance)
  intersected <- st_intersects(buffer, police_RJ_filtered)
  new_col <- paste0("total_police_2022_", distance)
  RJ_Metro_data[[new_col]] <- lengths(intersected)
}



setwd("")

write.csv(Recife_Metro_data, "PE_ALL_VARIABLES.csv")
write.csv(RJ_Metro_data, "RJ_ALL_VARIABLES.csv")


#### Regression  Analysis ####

RJ_data<-read.csv("RJ_ALL_VARIABLES.csv")
PE_data<-read.csv("PE_ALL_VARIABLES.csv")


#Adding two months in 2018 counts to the dataset 
start_before <- as.POSIXct("2018-08-07", tz = "UTC")
election_day <- as.POSIXct("2018-10-07", tz = "UTC")
end_after <- as.POSIXct("2018-12-07", tz = "UTC")
RJ_sf <- RJ_data %>%
  st_as_sf(coords = c("long.x", "lat.x"), crs = 4326) %>%
  st_transform(31983)


setwd("")
gunshots<-read.csv("gunshots.csv")
gunshots_sf <- gunshots %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(31983)
distances <- c(300, 500, 1000)
periods <- list(
  "beforeTRUE" = c(start_before, election_day),
  "beforeFALSE" = c(election_day, end_after)
)

gunshots_sf$date <- as.Date(gunshots_sf$date, format = "%d/%m/%Y")
for (distance in distances) {
  for (period_name in names(periods)) {
    dates <- periods[[period_name]]
    
    # Filter gunshots in the date window
    gunshots_filtered <- gunshots_sf %>%
      filter(date >= dates[1], date < dates[2])
    
    # Create spatial buffer
    buffer <- st_buffer(RJ_sf, dist = distance)
    
    # Check intersection: each buffer vs all filtered gunshots
    intersected <- st_intersects(buffer, gunshots_filtered)
    
    # Column name for results
    new_col <- paste0("two_months_", distance, "_2018_", period_name)
    
    # Save counts of intersected gunshots
    RJ_data[[new_col]] <- lengths(intersected)
  }
}


PE_sf <- PE_data %>%
  st_as_sf(coords = c("long.x", "lat.x"), crs = 4326) %>%
  st_transform(31983)




for (distance in distances) {
  for (period_name in names(periods)) {
    dates <- periods[[period_name]]
    gunshots_filtered <- gunshots_sf %>%
      filter(date >= dates[1], date < dates[2])
    
    buffer <- st_buffer(PE_sf, dist = distance)
    intersected <- st_intersects(buffer, gunshots_filtered)
    
    new_col <- paste0("two_months_", distance, "_2018_", period_name)
    PE_data[[new_col]] <- lengths(intersected)
  }
}

#Adding two months counts in 2022 to the dataset
start_before <- as.POSIXct("2022-08-02", tz = "UTC")
election_day <- as.POSIXct("2022-10-02", tz = "UTC")
end_after <- as.POSIXct("2022-12-02", tz = "UTC")

periods <- list(
  "beforeTRUE" = c(start_before, election_day),
  "beforeFALSE" = c(election_day, end_after)
)
for (distance in distances) {
  for (period_name in names(periods)) {
    dates <- periods[[period_name]]
    gunshots_filtered <- gunshots_sf %>%
      filter(date >= dates[1], date < dates[2])
    
    buffer <- st_buffer(RJ_sf, dist = distance)
    intersected <- st_intersects(buffer, gunshots_filtered)
    
    new_col <- paste0("two_months_", distance, "_2022_", period_name)
    RJ_data[[new_col]] <- lengths(intersected)
  }
}

for (distance in distances) {
  for (period_name in names(periods)) {
    dates <- periods[[period_name]]
    gunshots_filtered <- gunshots_sf %>%
      filter(date >= dates[1], date < dates[2])
    
    buffer <- st_buffer(PE_sf, dist = distance)
    intersected <- st_intersects(buffer, gunshots_filtered)
    
    new_col <- paste0("two_months_", distance, "_2022_", period_name)
    PE_data[[new_col]] <- lengths(intersected)
  }
}

# Including controls 
setwd("")
demographic_data<-read.csv("demographic_data.csv")


###Regressions###


##First table (one week)##

#300 m and aggregated#

RJ_2018<-RJ_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_300_2018_beforeTRUE >= 1 & one_week_300_2018_beforeFALSE == 0) |
      (one_week_300_2018_beforeTRUE==0 &  one_week_300_2018_beforeFALSE == 1)
  )%>%
  mutate(treatment=ifelse((one_week_300_2018_beforeTRUE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=1)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "RJ", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

PE_2018<-PE_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_300_2018_beforeTRUE >= 1 & one_week_300_2018_beforeFALSE == 0) |
      (one_week_300_2018_beforeTRUE==0 &  one_week_300_2018_beforeFALSE == 1)
  )%>%
  mutate(treatment=ifelse((one_week_300_2018_beforeTRUE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=0)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "PE", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

reg_data1<-bind_rows(RJ_2018, PE_2018)%>%
  mutate(police_violence=police_violence/10^4)%>%
  filter(!is.na(delta))


reg1<-lm_robust(data=reg_data1, delta~violent_area*treatment+police_violence
                +RJ+polling_size+share_male+share_above_65+
                  share_higher_education, clusters = bairro)


#500 m and Aggregated#

RJ_2018<-RJ_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_500_2018_beforeTRUE >= 1 & one_week_500_2018_beforeFALSE == 0) |
      (one_week_500_2018_beforeTRUE==0 &  one_week_500_2018_beforeFALSE == 1)
  )%>%
  mutate(treatment=ifelse((one_week_500_2018_beforeTRUE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=1)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "RJ", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

PE_2018<-PE_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_500_2018_beforeTRUE >= 1 & one_week_500_2018_beforeFALSE == 0) |
      (one_week_500_2018_beforeTRUE==0 &  one_week_500_2018_beforeFALSE == 1)
  )%>%
  mutate(treatment=ifelse((one_week_500_2018_beforeTRUE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=0)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "PE", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

reg_data2<-bind_rows(RJ_2018, PE_2018)%>%
  mutate(police_violence=police_violence/10^4)%>%
  filter(!is.na(delta))

reg2<-lm_robust(data=reg_data2, delta~violent_area*treatment+police_violence
                +RJ+polling_size+share_male+share_above_65+
                  share_higher_education, clusters = bairro)

#300m and RJ#
reg3<-lm_robust(data=reg_data1%>%filter(RJ==1), delta~violent_area*treatment+police_violence
                +polling_size+share_male+share_above_65+
                  share_higher_education, clusters = bairro)

#500 m and RJ#
reg4<-lm_robust(data=reg_data2%>%filter(RJ==1), delta~violent_area*treatment+police_violence
                +polling_size+share_male+share_above_65+
                  share_higher_education, clusters = bairro)
#300m and PE#
reg5<-lm_robust(data=reg_data1%>%filter(RJ==0), delta~violent_area*treatment+police_violence
                +polling_size+share_male+share_above_65+
                  share_higher_education, clusters = bairro)


#500 m and PE#
reg6<-lm_robust(data=reg_data2%>%filter(RJ==0), delta~violent_area*treatment+police_violence
                +polling_size+share_male+share_above_65+
                  share_higher_education, clusters = bairro)




##Controling for time trend and spatial spillover##


#Spatial 300
RJ_2018<-RJ_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_300_2018_beforeTRUE >= 1 & one_week_1000_2018_beforeFALSE == 0) |
      (one_week_1000_2018_beforeTRUE==0 &  one_week_300_2018_beforeFALSE == 1)
  )%>%
  mutate(treatment=ifelse((one_week_300_2018_beforeTRUE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=1)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "RJ", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

PE_2018<-PE_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_300_2018_beforeTRUE >= 1 & one_week_1000_2018_beforeFALSE == 0) |
      (one_week_800_2018_beforeTRUE==0 &  one_week_300_2018_beforeFALSE == 1)
  )%>%
  mutate(treatment=ifelse((one_week_300_2018_beforeTRUE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=0)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "PE", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

reg_data7<-bind_rows(RJ_2018, PE_2018)%>%
  mutate(police_violence=police_violence/10^4)%>%
  filter(!is.na(delta))


reg7<-lm_robust(data=reg_data7, delta~violent_area*treatment+police_violence
                +RJ+polling_size+share_male+share_above_65+
                  share_higher_education, clusters = bairro)


#Spatial 500
RJ_2018<-RJ_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_500_2018_beforeTRUE >= 1 & one_week_800_2018_beforeFALSE == 0) |
      (one_week_800_2018_beforeTRUE==0 &  one_week_500_2018_beforeFALSE == 1)
  )%>%
  mutate(treatment=ifelse((one_week_500_2018_beforeTRUE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=1)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "RJ", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

PE_2018<-PE_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_500_2018_beforeTRUE >= 1 & one_week_1000_2018_beforeFALSE == 0) |
      (one_week_1000_2018_beforeTRUE==0 &  one_week_500_2018_beforeFALSE == 1)
  )%>%
  mutate(treatment=ifelse((one_week_500_2018_beforeTRUE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=0)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "PE", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

reg_data8<-bind_rows(RJ_2018, PE_2018)%>%
  mutate(police_violence=police_violence/10^4)%>%
  filter(!is.na(delta))


reg8<-lm_robust(data=reg_data8, delta~violent_area*treatment+police_violence
                +RJ+polling_size+share_male+share_above_65+
                  share_higher_education, clusters = bairro)

#Time trend 300

RJ_2018<-RJ_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_300_2018_beforeTRUE >= 1 & one_month_300_2018_beforeFALSE == 0 & 
       one_week_300_2018_beforeTRUE==one_month_300_2018_beforeTRUE) |
      (one_month_300_2018_beforeTRUE == 0 & one_week_300_2018_beforeFALSE >= 1 &
         one_week_300_2018_beforeFALSE==one_month_300_2018_beforeFALSE)
  )%>%
  mutate(treatment=ifelse((one_week_300_2018_beforeTRUE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=1)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "RJ", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

PE_2018<-PE_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_300_2018_beforeTRUE >= 1 & one_month_300_2018_beforeFALSE == 0 & 
       one_week_300_2018_beforeTRUE==one_month_300_2018_beforeTRUE) |
      (one_month_300_2018_beforeTRUE == 0 & one_week_300_2018_beforeFALSE >= 1 &
         one_week_300_2018_beforeFALSE==one_month_300_2018_beforeFALSE)
  )%>%
  mutate(treatment=ifelse((one_week_300_2018_beforeTRUE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=0)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "PE", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

reg_data9<-bind_rows(RJ_2018, PE_2018)%>%
  mutate(police_violence=police_violence/10^4)%>%
  filter(!is.na(delta))


reg9<-lm_robust(data=reg_data9, delta~violent_area*treatment+police_violence
                +RJ+polling_size+share_male+share_above_65+
                  share_higher_education, clusters = bairro)


#Time trend 500

RJ_2018<-RJ_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_500_2018_beforeTRUE >= 1 & one_month_500_2018_beforeFALSE == 0 & 
       one_week_500_2018_beforeTRUE==one_month_500_2018_beforeTRUE) |
      (one_month_500_2018_beforeTRUE == 0 & one_week_500_2018_beforeFALSE >= 1 &
         one_week_500_2018_beforeFALSE==one_month_500_2018_beforeFALSE)
  )%>%
  mutate(treatment=ifelse((one_week_500_2018_beforeTRUE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=1)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "RJ", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

PE_2018<-PE_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_500_2018_beforeTRUE >= 1 & one_month_500_2018_beforeFALSE == 0 & 
       one_week_500_2018_beforeTRUE==one_month_500_2018_beforeTRUE) |
      (one_month_500_2018_beforeTRUE == 0 & one_week_500_2018_beforeFALSE >= 1 &
         one_week_500_2018_beforeFALSE==one_month_500_2018_beforeFALSE)
  )%>%
  mutate(treatment=ifelse((one_week_500_2018_beforeTRUE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=0)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "PE", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

reg_data10<-bind_rows(RJ_2018, PE_2018)%>%
  mutate(police_violence=police_violence/10^4)%>%
  filter(!is.na(delta))


reg10<-lm_robust(data=reg_data10, delta~violent_area*treatment+police_violence
                 +RJ+polling_size+share_male+share_above_65+
                   share_higher_education, clusters = bairro)

##Placebo tests##

#300 and after#

RJ_2018<-RJ_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_300_2018_beforeFALSE >= 1 & one_week_300_2018_beforeFALSE == 
       two_weeks_300_2018_beforeFALSE) |
      (one_week_300_2018_beforeFALSE==0 &  two_weeks_300_2018_beforeFALSE >=1)
  )%>%
  mutate(treatment=ifelse((one_week_300_2018_beforeFALSE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=1)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "RJ", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

PE_2018<-PE_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_300_2018_beforeFALSE >= 1 & one_week_300_2018_beforeFALSE == 
       two_weeks_300_2018_beforeFALSE) |
      (one_week_300_2018_beforeFALSE==0 &  two_weeks_300_2018_beforeFALSE >=1)
  )%>%
  mutate(treatment=ifelse((one_week_300_2018_beforeFALSE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=0)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "PE", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

reg_data11<-bind_rows(RJ_2018, PE_2018)%>%
  mutate(police_violence=police_violence/10^4)%>%
  filter(!is.na(delta))


reg11<-lm_robust(data=reg_data11, delta~violent_area*treatment+police_violence
                 +RJ+polling_size+share_male+share_above_65+
                   share_higher_education, clusters = bairro)

#500 and after#

RJ_2018<-RJ_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_500_2018_beforeFALSE >= 1 & one_week_500_2018_beforeFALSE == 
       two_weeks_500_2018_beforeFALSE) |
      (one_week_500_2018_beforeFALSE==0 &  two_weeks_500_2018_beforeFALSE >=1)
  )%>%
  mutate(treatment=ifelse((one_week_500_2018_beforeFALSE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=1)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "RJ", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

PE_2018<-PE_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_500_2018_beforeFALSE >= 1 & one_week_500_2018_beforeFALSE == 
       two_weeks_500_2018_beforeFALSE) |
      (one_week_500_2018_beforeFALSE==0 &  two_weeks_500_2018_beforeFALSE >=1)
  )%>%
  mutate(treatment=ifelse((one_week_500_2018_beforeFALSE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=0)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "PE", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

reg_data12<-bind_rows(RJ_2018, PE_2018)%>%
  mutate(police_violence=police_violence/10^4)%>%
  filter(!is.na(delta))


reg12<-lm_robust(data=reg_data12, delta~violent_area*treatment+police_violence
                 +RJ+polling_size+share_male+share_above_65+
                   share_higher_education, clusters = bairro)

#300 and before#

RJ_2018<-RJ_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_300_2018_beforeTRUE >= 1 & one_week_300_2018_beforeTRUE == 
       two_weeks_300_2018_beforeTRUE) |
      (one_week_300_2018_beforeTRUE==0 &  two_weeks_300_2018_beforeTRUE >=1)
  )%>%
  mutate(treatment=ifelse((one_week_300_2018_beforeTRUE == 0),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=1)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "RJ", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

PE_2018<-PE_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_300_2018_beforeTRUE >= 1 & one_week_300_2018_beforeTRUE == 
       two_weeks_300_2018_beforeTRUE) |
      (one_week_300_2018_beforeTRUE==0 &  two_weeks_300_2018_beforeTRUE >=1)
  )%>%
  mutate(treatment=ifelse((one_week_300_2018_beforeTRUE == 0),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=0)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "PE", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

reg_data13<-bind_rows(RJ_2018, PE_2018)%>%
  mutate(police_violence=police_violence/10^4)%>%
  filter(!is.na(delta))


reg13<-lm_robust(data=reg_data13, delta~violent_area*treatment+police_violence
                 +RJ+polling_size+share_male+share_above_65+
                   share_higher_education, clusters = bairro)


#500 and before#

RJ_2018<-RJ_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_500_2018_beforeTRUE >= 1 & one_week_500_2018_beforeTRUE == 
       two_weeks_500_2018_beforeTRUE) |
      (one_week_500_2018_beforeTRUE==0 &  two_weeks_500_2018_beforeTRUE >=1)
  )%>%
  mutate(treatment=ifelse((one_week_500_2018_beforeTRUE == 0),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=1)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "RJ", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

PE_2018<-PE_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_500_2018_beforeTRUE >= 1 & one_week_500_2018_beforeTRUE == 
       two_weeks_500_2018_beforeTRUE) |
      (one_week_500_2018_beforeTRUE==0 &  two_weeks_500_2018_beforeTRUE >=1)
  )%>%
  mutate(treatment=ifelse((one_week_500_2018_beforeTRUE == 0),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=0)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "PE", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

reg_data14<-bind_rows(RJ_2018, PE_2018)%>%
  mutate(police_violence=police_violence/10^4)%>%
  filter(!is.na(delta))


reg14<-lm_robust(data=reg_data14, delta~violent_area*treatment+police_violence
                 +RJ+polling_size+share_male+share_above_65+
                   share_higher_education, clusters = bairro)

## Two weeks ##

#300 m and aggregated#

RJ_2018<-RJ_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (two_weeks_300_2018_beforeTRUE >= 1 & two_weeks_300_2018_beforeFALSE == 0) |
      (two_weeks_300_2018_beforeTRUE==0 &  two_weeks_300_2018_beforeFALSE == 1)
  )%>%
  mutate(treatment=ifelse((two_weeks_300_2018_beforeTRUE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=1)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "RJ", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

PE_2018<-PE_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (two_weeks_300_2018_beforeTRUE >= 1 & two_weeks_300_2018_beforeFALSE == 0) |
      (two_weeks_300_2018_beforeTRUE==0 &  two_weeks_300_2018_beforeFALSE == 1)
  )%>%
  mutate(treatment=ifelse((two_weeks_300_2018_beforeTRUE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=0)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "PE", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

reg_data15<-bind_rows(RJ_2018, PE_2018)%>%
  mutate(police_violence=police_violence/10^4)%>%
  filter(!is.na(delta))


reg15<-lm_robust(data=reg_data15, delta~violent_area*treatment+police_violence
                 +RJ+polling_size+share_male+share_above_65+
                   share_higher_education, clusters = bairro)


#500 m and Aggregated#

RJ_2018<-RJ_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (two_weeks_500_2018_beforeTRUE >= 1 & two_weeks_500_2018_beforeFALSE == 0) |
      (two_weeks_500_2018_beforeTRUE==0 &  two_weeks_500_2018_beforeFALSE == 1)
  )%>%
  mutate(treatment=ifelse((two_weeks_500_2018_beforeTRUE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=1)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "RJ", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

PE_2018<-PE_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (two_weeks_500_2018_beforeTRUE >= 1 & two_weeks_500_2018_beforeFALSE == 0) |
      (two_weeks_500_2018_beforeTRUE==0 &  two_weeks_500_2018_beforeFALSE == 1)
  )%>%
  mutate(treatment=ifelse((two_weeks_500_2018_beforeTRUE >= 1),1,0))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, treatment,
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=0)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "PE", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

reg_data16<-bind_rows(RJ_2018, PE_2018)%>%
  mutate(police_violence=police_violence/10^4)%>%
  filter(!is.na(delta))

reg16<-lm_robust(data=reg_data16, delta~violent_area*treatment+police_violence
                 +RJ+polling_size+share_male+share_above_65+
                   share_higher_education, clusters = bairro)

#300m and RJ#
reg17<-lm_robust(data=reg_data15%>%filter(RJ==1), delta~violent_area*treatment+police_violence
                 +polling_size+share_male+share_above_65+
                   share_higher_education, clusters = bairro)

#500 m and RJ#
reg18<-lm_robust(data=reg_data16%>%filter(RJ==1), delta~violent_area*treatment+police_violence
                 +polling_size+share_male+share_above_65+
                   share_higher_education, clusters = bairro)
#300m and PE#
reg19<-lm_robust(data=reg_data15%>%filter(RJ==0), delta~violent_area*treatment+police_violence
                 +polling_size+share_male+share_above_65+
                   share_higher_education, clusters = bairro)

#500 m and PE#
reg20<-lm_robust(data=reg_data16%>%filter(RJ==0), delta~violent_area*treatment+police_violence
                 +polling_size+share_male+share_above_65+
                   share_higher_education, clusters = bairro)



###Spatial Distribution###

RJ<-reg_data2%>%filter(RJ==1)

RJ_map<-st_read(
  "")
RJ_shape <- RJ %>%
  st_as_sf(coords = c("long.x", "lat.x"), crs = 4326) %>%
  st_transform(crs = st_crs(RJ_map))
limit_RMRJ<-st_bbox(RJ_shape)
map_RJ <- ggplot() +
  geom_sf(data=RJ_map,fill="#A9DFBF") +
  geom_sf(data=RJ_shape, aes(color=factor(treatment)),shape = 19,
          size = 1.25, alpha=0.8)+
  scale_color_manual(
    values = c("1" = "green4", "0" = "orangered2"),
    name = "Group",
    labels = c("1" = "Treated", "0" = "Control")
  ) +
  labs(x = "",
       y = "",
       title = "Greater Rio de Janeiro") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        legend.position = "none") +
  coord_sf(ylim = c(limit_RMRJ["ymin"]-0.05, limit_RMRJ["ymax"]+0.05),
           xlim = c(limit_RMRJ["xmin"]-0.05, limit_RMRJ["xmax"]+0.05))


PE<-reg_data2%>%filter(RJ==0)
PE_map<-st_read(
  "")
PE_shape <- PE %>%
  st_as_sf(coords = c("long.x", "lat.x"), crs = 4326) %>%
  st_transform(crs = st_crs(PE_map))
limit_RMR<-st_bbox(PE_shape)
map_PE <- ggplot() +
  geom_sf(data=PE_map,fill="#A9DFBF") +
  # Adding capital.shp
  geom_sf(data=PE_shape, aes(color=factor(treatment)),shape = 19,
          size = 1.25, alpha=0.8)+
  scale_color_manual(
    values = c("1" = "green4", "0" = "orangered2"),
    name = "Group",
    labels = c("1" = "Treated", "0" = "Control")
  ) +
  # Including labels
  labs(x = "",
       y = "",
       title = "Greater Recife") +
  scale_x_continuous(breaks = seq(floor(limit_RMR["xmin"] - 0.05),
                                  ceiling(limit_RMR["xmax"] + 0.05),
                                  by = 0.2)) +
  # Changing title format
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  # Cropping image
  coord_sf(ylim = c(limit_RMR["ymin"]-0.05, limit_RMR["ymax"]+0.05),
           xlim = c(limit_RMR["xmin"]-0.05, limit_RMR["xmax"]+0.05))

aggregate_map<-(map_RJ+map_PE)

### Balance Check ###
RJ_complement<-RJ_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_500_2018_beforeTRUE == 0 & one_week_500_2018_beforeFALSE == 0) |
      (one_week_500_2018_beforeTRUE >= 1 & one_week_500_2018_beforeFALSE >= 1)
  )%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, 
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=1)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "RJ", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))

PE_complement<-PE_data %>%
  mutate(violent_area=ifelse(total_nonpolice_2018_1000>=quantile(total_nonpolice_2018_1000,
                                                                 0.5),1,0))%>%
  filter(
    (one_week_500_2018_beforeTRUE == 0 & one_week_500_2018_beforeFALSE == 0) |
      (one_week_500_2018_beforeTRUE >= 1 & one_week_500_2018_beforeFALSE >= 1))%>%
  select(NR_ZONA, NR_LOCAL_VOTACAO, cidade.x, nome_local.x, lat.x, long.x, bairro,
         endereco, share_security_2014, share_security_2018, 
         total_police_2018_1000, violent_area)%>%
  mutate(delta=share_security_2018-share_security_2014,
         election_2018=1, RJ=0)%>%
  rename(police_violence="total_police_2018_1000")%>%
  left_join(demographic_data %>% filter(SG_UF == "PE", ANO_ELEICAO==2018)%>%select(
    NR_LOCAL_VOTACAO, NR_ZONA, polling_size, share_above_65, share_higher_education,
    share_male), 
    by = c("NR_LOCAL_VOTACAO", "NR_ZONA"))


complement<-bind_rows(RJ_complement, PE_complement)%>%
  filter(!is.na(share_security_2018))

controlRJ1<-reg_data1%>%filter(RJ==1 & treatment==0)
treatRJ1<-reg_data1%>%filter(RJ==1 & treatment==1)

### Arguing as-if randomness ###

RJ_reg2_treat<-reg_data2%>%filter(RJ==1 & treatment==1)%>%select(NR_ZONA, NR_LOCAL_VOTACAO, 
                                                                 lat.x, long.x)

RJ_reg2_control<-reg_data2%>%filter(RJ==1 & treatment==0)%>%select(NR_ZONA, NR_LOCAL_VOTACAO, 
                                                                   lat.x, long.x)
PE_reg2_treat<-reg_data2%>%filter(RJ==0 & treatment==1)%>%select(NR_ZONA, NR_LOCAL_VOTACAO, 
                                                                 lat.x, long.x)

PE_reg2_control<-reg_data2%>%filter(RJ==0 & treatment==0)%>%select(NR_ZONA, NR_LOCAL_VOTACAO, 
                                                                   lat.x, long.x)

date_seq <- seq.Date(from = as.Date("2016-06-01"), to = as.Date("2021-12-01"), by = "month")

monthly_count <- tibble(
  date = date_seq,
  RJ_treat = NA_real_,
  RJ_control = NA_real_,
  PE_treat = NA_real_,
  PE_control = NA_real_
)


##Creating counts of RJ control## 

monthly_count <- monthly_count %>%
  mutate(
    start_date = date,
    end_date = ceiling_date(date, "month") - days(1)
  )
RJ_coords <- RJ_reg2_control %>%
  filter(!is.na(lat.x), !is.na(long.x)) %>%
  mutate(
    lat.x = as.numeric(lat.x),
    long.x = as.numeric(long.x)
  ) %>%
  select(long.x, lat.x) %>%
  as.matrix()

#Loop#
for (i in 1:nrow(monthly_count)) {
  start_date <- monthly_count$start_date[i]
  end_date <- monthly_count$end_date[i]
  gun_month <- gunshots %>%
    filter(policeAction=="False")%>%
    filter(date >= start_date, date <= end_date) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    mutate(
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    )
  count <- 0
  for (j in 1:nrow(gun_month)) {
    shot_coord <- c(gun_month$longitude[j], gun_month$latitude[j])
    if (length(shot_coord) == 2 && all(!is.na(shot_coord))) {
      distances <- distHaversine(shot_coord, RJ_coords)
      
      if (any(distances <= 500, na.rm = TRUE)) {
        count <- count + 1
      }
    }
  }
  monthly_count$RJ_control[i] <- count/nrow(RJ_reg2_control)
}


##Creating counts of RJ treatment## 

RJ_coords <- RJ_reg2_treat %>%
  filter(!is.na(lat.x), !is.na(long.x)) %>%
  mutate(
    lat.x = as.numeric(lat.x),
    long.x = as.numeric(long.x)
  ) %>%
  select(long.x, lat.x) %>%
  as.matrix()

#Loop#
for (i in 1:nrow(monthly_count)) {
  start_date <- monthly_count$start_date[i]
  end_date <- monthly_count$end_date[i]
  gun_month <- gunshots %>%
    filter(policeAction=="False")%>%
    filter(date >= start_date, date <= end_date) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    mutate(
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    )
  count <- 0
  for (j in 1:nrow(gun_month)) {
    shot_coord <- c(gun_month$longitude[j], gun_month$latitude[j])
    if (length(shot_coord) == 2 && all(!is.na(shot_coord))) {
      distances <- distHaversine(shot_coord, RJ_coords)
      
      if (any(distances <= 500, na.rm = TRUE)) {
        count <- count + 1
      }
    }
  }
  monthly_count$RJ_treat[i] <- count/nrow(RJ_reg2_treat)
}

##Creating counts of PE treatment## 

PE_coords <- PE_reg2_treat %>%
  filter(!is.na(lat.x), !is.na(long.x)) %>%
  mutate(
    lat.x = as.numeric(lat.x),
    long.x = as.numeric(long.x)
  ) %>%
  select(long.x, lat.x) %>%
  as.matrix()

#Loop#
for (i in 1:nrow(monthly_count)) {
  start_date <- monthly_count$start_date[i]
  end_date <- monthly_count$end_date[i]
  gun_month <- gunshots %>%
    filter(policeAction=="False")%>%
    filter(date >= start_date, date <= end_date) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    mutate(
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    )
  count <- 0
  for (j in 1:nrow(gun_month)) {
    shot_coord <- c(gun_month$longitude[j], gun_month$latitude[j])
    if (length(shot_coord) == 2 && all(!is.na(shot_coord))) {
      distances <- distHaversine(shot_coord, PE_coords)
      
      if (any(distances <= 500, na.rm = TRUE)) {
        count <- count + 1
      }
    }
  }
  monthly_count$PE_treat[i] <- count/nrow(PE_reg2_treat)
}

##Creating counts of PE control## 

PE_coords <- PE_reg2_control %>%
  filter(!is.na(lat.x), !is.na(long.x)) %>%
  mutate(
    lat.x = as.numeric(lat.x),
    long.x = as.numeric(long.x)
  ) %>%
  select(long.x, lat.x) %>%
  as.matrix()

#Loop#
for (i in 1:nrow(monthly_count)) {
  start_date <- monthly_count$start_date[i]
  end_date <- monthly_count$end_date[i]
  gun_month <- gunshots %>%
    filter(policeAction=="False")%>%
    filter(date >= start_date, date <= end_date) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    mutate(
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    )
  count <- 0
  for (j in 1:nrow(gun_month)) {
    shot_coord <- c(gun_month$longitude[j], gun_month$latitude[j])
    if (length(shot_coord) == 2 && all(!is.na(shot_coord))) {
      distances <- distHaversine(shot_coord, PE_coords)
      
      if (any(distances <= 500, na.rm = TRUE)) {
        count <- count + 1
      }
    }
  }
  monthly_count$PE_control[i] <- count/nrow(PE_reg2_control)
}

##Creating graph##
plot_data <- monthly_count %>%
  select(date, RJ_treat, RJ_control) %>%
  pivot_longer(cols = c(RJ_treat, RJ_control),
               names_to = "group", values_to = "value") %>%
  mutate(group = recode(group,
                        RJ_treat = "Treatment",
                        RJ_control = "Control"))
Sys.setlocale("LC_TIME", "C")
RJ_plot<-ggplot(plot_data, aes(x = date, y = value, color = group)) +
  geom_line(size = 1) +
  scale_x_date(
    date_breaks = "8 months",
    date_labels = "%b-%Y"  
  ) +
  labs(
    x="",
    y="",
    color = "Group",
    title = "Greater Rio de Janeiro"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

plot_data <- monthly_count %>%
  select(date, PE_treat, PE_control) %>%
  filter(date>="2018-04-01")%>%
  pivot_longer(cols = c(PE_treat, PE_control),
               names_to = "group", values_to = "value") %>%
  mutate(group = recode(group,
                        PE_treat = "Treatment",
                        PE_control = "Control"))

Sys.setlocale("LC_TIME", "C")
PE_plot<-ggplot(plot_data, aes(x = date, y = value, color = group)) +
  geom_line(size = 1) +
  scale_x_date(
    date_breaks = "5 months",
    date_labels = "%b-%Y"  
  ) +
  labs(
    x="",
    y="",
    color = "Group",
    title = "Greater Recife"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )


Figure5<-(RJ_plot + PE_plot)
