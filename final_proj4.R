# Augmented SCM

# libraries
library(tidyverse)
library(tigris)
library(sf)
library(lubridate)
library(stats)
library(ggplot2)
library(Synth)
library(kableExtra)
# california city boundaries: https://gis.data.ca.gov/datasets/5ed97ff8ffeb4eaca7e06a8237d18e13/explore?location=33.880751%2C-118.115250%2C9.74

# data
load(file = "/Users/clairemorton/Documents/_STATS209/final_project/ICPSR_37302/DS0001/data.rda")
data = da37302.0001
rm(da37302.0001)
data$CITY = trimws(as.character(data$CITY))
data$AGENCY_NAME = trimws(as.character(data$AGENCY_NAME))

# restrict to subsets of agency data
state = data[data$SAMPTYPE== "(5) State agency",]
local = data[data$SAMPTYPE== "(3) Local police",]
county = local %>%
  dplyr::filter(grepl("COUNTY", AGENCY_NAME, fixed = TRUE))
city = data %>%
  dplyr::filter(mapply(grepl, data$CITY, data$AGENCY_NAME)) %>% # city included in agency name -- implies city police department
  dplyr::filter(!grepl("COUNTY", AGENCY_NAME, fixed = TRUE))

filenames <- list.files("/Users/clairemorton/Documents/_STATS209/final_project/data/California", pattern="*.rds", full.names=TRUE)
ldf <- lapply(filenames, readRDS)
ldf <- lapply(ldf, "[", "date")
#res <- lapply(ldf, summary)
city_names <- substr(filenames, sapply(gregexpr(pattern="/", filenames), max)+1, str_length(filenames)-4)
names(ldf) <- city_names
ldf <- Map(cbind, ldf, city = city_names)
ldf <- do.call(rbind, ldf) # 2 column data frame with stop date and city name

# we have 7 million traffic stops from 11 cities
city = city %>% filter(AGENCY_STATE == "CA")
ca_cities_with_stops = city[city$CITY %in% toupper(city_names),]
# extract date of acquisition
dates = my(str_c(ca_cities_with_stops$Q_11_M, "-", ca_cities_with_stops$Q_11_Y)) # convert to
# dataframe of city name and date acquired
ca_cities_with_stops$date = dates
ca_cities_with_stops = ca_cities_with_stops %>%
  select(CITY, date)

# create dataframe with city, min and max traffic stop date, and date of implementation
summary <- ldf %>% group_by(city) %>%
  summarize(min = min(date, na.rm = T),
            max = max(date, na.rm = T))
summary$implemented = dates

# we can go until 2015-07-01 (so up to and including 2015-06)
ldf$date = format(ldf$date, "%Y-%m")

stops_by_month = summarize(ldf %>%group_by(date, city), count_stops = n())
stops_by_month$full_date = as.Date(paste("01-", stops_by_month$date, sep=""), format = "%d-%Y-%m")

# plot LA stops compared to control stops
control_stops_by_month = stops_by_month %>%
  filter(city %in% c("Los Angeles", "Long Beach", "Stockton", "Bakersfield", "San Bernardino", "San Francisco")) %>%
  mutate(city_la = ifelse(city == "Los Angeles", "Los Angeles", "California Cities with\nTraffic Stop Data")) %>%
  arrange(full_date)

avg <- control_stops_by_month %>%
  filter(full_date >= "2012-01-01") %>%
  filter(full_date <= "2013-12-01") %>%
  group_by(city) %>%
  #mutate(avg = count_stops/mean(count_stops))
  summarize(avg = mean(count_stops))

stops_by_month$normalized_stops = ifelse(stops_by_month$city == "Los Angeles", stops_by_month$count_stops/avg$avg[3],
                                         ifelse(stops_by_month$city == "Long Beach", stops_by_month$count_stops/avg$avg[2],
                                                ifelse(stops_by_month$city == "Stockton", stops_by_month$count_stops/avg$avg[6],
                                                       ifelse(stops_by_month$city == "Bakersfield", stops_by_month$count_stops/avg$avg[1],
                                                              ifelse(stops_by_month$city == "San Bernardino", stops_by_month$count_stops/avg$avg[4],
                                                                     ifelse(stops_by_month$city == "San Francisco", stops_by_month$count_stops/avg$avg[5],
                                                                            0))))))


controls_LA = c(.086, .484)

#https://www.census.gov/quickfacts/fact/table/longbeachcitycalifornia/PST04522https://stackoverflow.com/2
controls_LongBeach = c(.121, .439)

#https://www.census.gov/quickfacts/fact/table/stocktoncitycalifornia/PST045222
controls_Stockton = c(.113, .449)

#https://www.census.gov/quickfacts/fact/table/bakersfieldcitycalifornia/PST045222
controls_Bakersfield = c(.070, .520)

#https://www.census.gov/quickfacts/fact/table/sanbernardinocitycalifornia/PST045222
controls_SanBernardino = c(.125, .676)

# https://www.census.gov/quickfacts/fact/table/sanfranciscocitycalifornia/PST045222
controls_SanFrancisco = c(.052, .154)

controls = cbind.data.frame(c("Black", "Hispanic"), as.numeric(controls_LongBeach), 
                            as.numeric(controls_Stockton), as.numeric(controls_Bakersfield), 
                            as.numeric(controls_SanBernardino), as.numeric(controls_SanFrancisco),
                            as.numeric(controls_LA))
colnames(controls) = c("var", "Long Beach", "Stockton", "Bakersfield", "San Bernardino", "San Francisco", "Los Angeles")


data = stops_by_month %>%
  filter(full_date >= "2012-01-01" & full_date < "2015-07-01") %>%
  filter(city %in% c("Los Angeles", "Long Beach", "Stockton", "Bakersfield", "San Bernardino", "San Francisco")) %>%
  mutate(treated = ifelse(city == "Los Angeles" & full_date >= "2014-01-01", 1, 0))  %>%
  mutate(full_date = as.numeric(format(full_date,"%m"))/12-(1/12)+as.numeric(format(full_date,"%Y"))) %>%
  ungroup() %>%
  select(full_date, normalized_stops, treated, city) 

data = data %>%
  mutate(black = c(t(unname(controls[1,data$city]))),
         hisp = c(t(unname(controls[2,data$city]))))

# We control for the prop Black and Hispanic residents
library(augsynth)
syn <- augsynth(normalized_stops ~ treated|black +hisp, city, full_date, data,
                progfunc = "ridge", scm = T, V = rep(1, 24))
plot(syn)
syn$weights

gaps = c(syn$data$synth_data$Y1plot - syn$data$synth_data$Y0plot %*% syn$weights)

placebo_gaps = data.frame("time" = 1:length(gaps), "Los Angeles" = unname(gaps))

# run placebo test
for (placebo_city in c("Long Beach", "Stockton", "Bakersfield", "San Bernardino", "San Francisco")){
  data_placebo <- data %>%
    mutate(treated = ifelse(city == placebo_city & full_date >= "2014-01-01", 1, 0))
  syn = augsynth(normalized_stops ~ treated|black +hisp, city, full_date, data_placebo,
                  progfunc = "ridge", scm = T, V = rep(1, 25))
  gaps = c(syn$data$synth_data$Y1plot - syn$data$synth_data$Y0plot %*% syn$weights)
  placebo_gaps = cbind(placebo_gaps, data.frame(gaps))
}
names(placebo_gaps) = c("time", "Los Angeles", "Long Beach", "Stockton", "Bakersfield", "San Bernardino", "San Francisco")

placebo_gaps = pivot_longer(placebo_gaps, !time, names_to = "city", values_to = "gap")
placebo_gaps$city_la = ifelse(placebo_gaps$city == "Los Angeles", "Los Angeles", "California Cities with\nTraffic Stop Data")

ggplot(placebo_gaps, aes(x = time-24, y = gap, group = city, color = city_la))+
  geom_line()+
  labs(color = "City")+
  theme_classic()+
  geom_hline(yintercept = 0, linetype = "dashed")+
  xlab("Time Since LA BWC Implementation (Months)")+
  ylab("Error in Synthetic Control (True - Predicted)")+
  geom_vline(xintercept = 0, linetype = "dashed")
  
post_treat = placebo_gaps %>%
  group_by(city) %>%
  filter(time >= 36) %>%
  summarize(mean = mean(gap^2))

pre_treat = placebo_gaps %>%
  group_by(city) %>%
  filter(time < 24) %>%
  summarize(mean = mean(gap^2))

pre_treat$mean = post_treat$mean/pre_treat$mean
colnames(pre_treat) = c("City", "Posttreatment MSE/Pretreatment MSE")
pre_treat
