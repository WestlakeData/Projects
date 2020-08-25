# Library Calls ####
library(tidyverse)
library(zoo)

#Download most recent dataset ####
download.file("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv", "./Data/OWID World Data.csv", method = "auto", quiet = FALSE, 
              mode = "w",
              cacheOK = TRUE,
              headers = NULL)

download.file("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", "./Data/us-counties.csv", method = "auto", quiet = FALSE, 
              mode = "w",
              cacheOK = TRUE,
              headers = NULL)

#Read in Datasets ####
world <- read.csv(file = "./Data/OWID World Data.csv")
county_census <- read.csv("./Data/county_census.csv")
us_counties <- read.csv(file = "./Data/us-counties.csv")

#Data Processing ####
# Sort us_counties list by location, date
us_counties$date <- as.Date(us_counties$date, format = "%Y-%m-%d")
us_counties_sort <- us_counties %>% arrange(state, county, date)

#Calculate new cases for each location and day
for (i in 1:nrow(us_counties_sort)){
  if (i==1){
    us_counties_sort$new_cases[i] <- NA
  }
  else if(us_counties_sort$county[i] != us_counties_sort$county[i-1]){
    us_counties_sort$new_cases[i] <- NA
  }
  else{
    us_counties_sort$new_cases[i] <- us_counties_sort$cases[i] - us_counties_sort$cases[i-1]
  }
}

#Calculate per Capita Data
us_co_sort_pop <- merge(x = us_counties_sort, y= county_census[ , c("state","county","X2019")], by = c("state", "county"), all.x = TRUE)

#Utah Data Subsets of interest ####
utah <- us_counties_sort %>% filter(state == "Utah")
top <- c("Salt Lake", "Utah", "Davis", "Cache", "Washington", "Weber")
top_6 <- utah %>% filter(county %in% top)
other_county <- utah %>% filter(!county %in% top)
utah_co <- utah %>% filter(county == "Utah")

#Hawaii Data Subset ####
hawaii <- us_counties_sort %>% filter(state == "Hawaii")
oahu <- hawaii %>% filter(county == "Honolulu")

#Plot Data ####
#Create Date axis labels

date_labels <- unique(utah$date)
last_date <- date_labels[length(date_labels)]
date_labels_sub <- date_labels[seq(1, length(date_labels), by = 14)] %>% append(last_date) %>% unique()


#Create Preliminary Plots
p1 <- ggplot(data = utah, aes(x = date, y= cases, color = county)) +
  geom_point() +
  scale_x_date(breaks = date_labels_sub)+ 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

p2 <- ggplot(data = top_6, aes(x = date, y= cases, color = county)) +
  geom_point() +
  scale_x_date(breaks = date_labels_sub)+ 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

p3 <- ggplot(data = other_county, aes(x = date, y= cases, color = county)) +
  geom_point() +
  scale_x_date(breaks = date_labels_sub)+ 
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1))

#Secondary Plots
p4 <- ggplot(data = utah, aes(x = date, y= new_cases)) +
  geom_col() +
  facet_wrap(~county) +
  geom_line(aes(y=rollmean(new_cases, 7, fill = NA)), color = "Red") +
  scale_x_date(date_breaks="weeks", date_labels="%b %e") +
  scale_color_manual("", breaks = "7-day MA", values = "Red") +
  labs(title = "Utah Cases by County",
       y = "New Cases",
       x = NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p5 <- ggplot(data = hawaii, aes(x = date, y= new_cases)) +
  geom_col() +
  facet_wrap(~county) +
  geom_line(aes(y=rollmean(new_cases, 7, fill = NA)), color = "Red") +
  scale_x_date(date_breaks="weeks", date_labels="%b %e") +
  scale_color_discrete(name = "7-Day MA") +
  labs(title = "Hawaii Cases by County",
       y = "New Cases",
       x = NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p6 <- ggplot(data = oahu, aes(x = date, y= new_cases)) +
  geom_col(alpha = 0.5) +
  geom_line(aes(y=rollmean(new_cases, 7, fill = NA)), color = "Red", size = 1.5) +
  scale_x_date(date_breaks="weeks", date_labels="%b %e") +
  scale_color_discrete(name = "7-Day MA") +
  labs(title = "Oahu Island Cases",
       y = "New Cases",
       x = NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p7 <- ggplot(data = top_6, aes(x = date, y= new_cases)) +
  geom_col() +
  facet_wrap(~county) +
  geom_line(aes(y=rollmean(new_cases, 7, fill = NA)), color = "Red") +
  scale_x_date(date_breaks="weeks", date_labels="%b %e") +
  scale_color_discrete(name = "7-Day MA") +
  labs(title = "Top 6 Utah Counties by Total New Cases",
       y = "New Cases",
       x = NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p8 <- ggplot(data = utah_co, aes(x = date, y= new_cases)) +
  geom_col(alpha = 0.5) +
  geom_line(aes(y=rollmean(new_cases, 7, fill = NA)), color = "Red", size = 1.5) +
  scale_x_date(date_breaks="weeks", date_labels="%b %e") +
  scale_color_continuous("Red", name = "7-day MA") +
  labs(title = "Utah County Cases",
       y = "New Cases",
       x = NULL) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "right")

p1
p2
p3
p4
p5
p6
p7
p8
