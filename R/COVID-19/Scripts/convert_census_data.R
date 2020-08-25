library(dplyr)
library(tidyr)
library(purrr)
library(readxl)

co_est2019_annres <- read_excel("Data/co-est2019-annres.xlsx", skip = 3)
co_est2019_annres <- co_est2019_annres[-c(3144:3149),]
names(co_est2019_annres)[1] <- "loc" 

co_est_19 <- separate(co_est2019_annres, loc, c("county", "state"), sep = ",")

co_est_19$county <- substring(co_est_19$county, 2)
co_est_19$county[1] <- "United States"

co_est_19$county <- gsub(" County", "", co_est_19$county)
co_est_19$state <- trimws(co_est_19$state)


write.csv(co_est_19, "./Data/county_census.csv")
