# Extract pre-treatment MSE for LA SCM and Ridge ASCM

# libraries
library(tidyverse)
library(tigris)
library(sf)
library(lubridate)
library(stats)
library(ggplot2)
library(Synth)
library(kableExtra)
library(augsynth)

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

control_stops_by_month = stops_by_month %>%
  filter(city %in% c("Los Angeles", "Long Beach", "Stockton", "Bakersfield", "San Bernardino", "San Francisco")) %>%
  mutate(city_la = ifelse(city == "Los Angeles", "Los Angeles", "California Cities with\nTraffic Stop Data")) %>%
  arrange(full_date)

ggplot(control_stops_by_month, aes(x = as.Date(full_date), y = normalized_stops, group = city, color = city))+geom_line()+
  ylab("Normalized Number of Stops")+theme_classic()+xlab("Date")+labs(color="City")+
  geom_vline(xintercept = as.Date("2014-01-01"))
ggsave("/Users/clairemorton/Documents/_STATS209/final_project/fig6.png")


# =====================================================================================================
# https://www.census.gov/quickfacts/fact/table/losangelescitycalifornia/PST045222
# controls: prop Black, prop Hispanic, prop pop in a traffic stop in previous 6 months to implementation
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
                            as.numeric(controls_SanBernardino), as.numeric(controls_SanFrancisco))
colnames(controls) = c("var", "Long Beach", "Stockton", "Bakersfield", "San Bernardino", "San Francisco")

# we can go until 2015-07-01 (so up to and including 2015-06)
control_stops = control_stops_by_month %>%
  filter(full_date >= "2012-01-01") %>%
  filter(full_date <= "2013-12-01")

controls_LA = data.frame(cbind(c("Black", "Hispanic"), controls_LA))
colnames(controls_LA) = c("date", "Los.Angeles")
treatment_stops = control_stops %>%
  filter(city == "Los Angeles") %>%
  select(date, normalized_stops)
names(treatment_stops) = c("date", "Los.Angeles")
treatment_predictors = rbind(data.frame(treatment_stops[c(1,2)]), controls_LA)

control_stops = control_stops %>%
  filter(city != "Los Angeles") %>%
  select(date, city, normalized_stops) %>%
  pivot_wider(names_from="city", values_from = "normalized_stops")
control_stops = rename(control_stops, "var" = "date")
control_predictors = rbind(control_stops, controls)

# outcomes
treatment_stops = stops_by_month %>%
  select(-c(count_stops)) %>%
  pivot_wider(names_from = city, values_from = normalized_stops) %>%
  filter(full_date >= as.Date("2014-01-01")) %>%
  filter(full_date < as.Date("2015-07-01")) %>%
  arrange(full_date) %>%
  select(-full_date) %>%
  select(c("date", "Los Angeles", "Long Beach", "Stockton", "Bakersfield", "San Bernardino", "San Francisco")) %>%
  ungroup()

treatment_outcomes = data.frame(treatment_stops[c(1,2)])
control_outcomes = data.frame(treatment_stops[-2])

X1 = matrix(as.numeric(as.matrix(treatment_predictors[-1])), ncol = 1)
X0 = matrix(as.numeric(as.matrix(control_predictors[-1])), ncol = 5)
Z1 = matrix(as.numeric(as.matrix(treatment_outcomes[-1])), ncol = 1)
Z0 = matrix(as.numeric(as.matrix(control_outcomes[-1])), ncol = 5)

rep(1, 26)
# synthesize control
synth.out <- synth(X1 = X1, X0 = X0, Z1 = Z1, Z0 = Z0,
                   method = "BFGS", custom.v=rep(1, 26) #   set optim() to use the BFGS quasi-Newton algorithm. 
)

# obtain gaps pre-treatment
gaps = c(X1 - X0 %*% synth.out$solution.w)

# calculate pre-treatment MSE
mean(gaps^2)


# ASCM Analysis

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

syn <- augsynth(normalized_stops ~ treated|black +hisp, city, full_date, data,
                progfunc = "Ridge", scm = T, min_1se = FALSE)

gaps = c(X1 - X0 %*% synth.out$solution.w)
gaps2 = c(syn$data$synth_data$X1 - syn$data$synth_data$X0 %*% syn$weights)

gapsdf = data.frame(g = gaps[1:24])
gaps2df = data.frame(g = gaps2[1:24])

mean(gapsdf$g^2)
mean(gaps2df$g^2)

# graph pre-treatment SCM vs ASCM gaps
ggplot(gapsdf, aes(x = 1:24, y = g))+
  geom_line()+
  geom_line(data = gaps2df, aes(x = 1:24, y = g, color = "red"))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_classic()

