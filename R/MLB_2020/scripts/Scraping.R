#Install necessary packages ####
#install.packages('tidyverse')
#install.packages('rvest')
#install.packages("jsonlite")

#Library Calls ####
library(tidyverse)
library(rvest)
library(jsonlite)

#Scraping Operations -----
#Team Win/Loss Data ====
url <- 'https://www.baseball-reference.com/leagues/MLB/2020-standings.shtml'
url2 <- 'https://www.mlb.com/standings'
url3 <- 'https://www.baseball-reference.com/leagues/MLB-standings.shtml#expanded_standings_overall::none'
#Read HTML
webpage <- read_html(url)
web_data <-  html_nodes(webpage, 'tbody') %>% html_nodes('tr')

standings <- read_html(url3)
   
standings_body1 <- standings %>% html_nodes(xpath = "//*[@id='csv_expanded_standings_overall']")


#Remove HTML tags
web_data_no_tags <- gsub("<.*?>", "", web_data) %>% strsplit(split = "\n")




#web_data2 <- webpage %>% html_nodes('#all_expanded_standings_overall')
#web_data2

#MLB Data API ====
mlb_api <- "http://lookup-service-prod.mlb.com"

#Team Data Request
all_star_team <- "N"
year <- "2020"
sort_order <- "name_asc"
team_data_req <- paste("/json/named.team_all_season.bam?sport_code='mlb'&all_star_sw='", all_star_team,"'&sort_order=", sort_order,"&season='", year,"'", sep = "")
team_data_raw <- fromJSON(paste(mlb_api, team_data_req, sep = ''))
team_data <- as.data.frame(team_data_raw$team_all_season$queryResults$row)

#Extract Data ####
#Initialize Win/Loss Dataframe
win_loss <- data.frame(Team = character(30),Wins = integer(30), Losses = integer(30), Win_Perc = numeric(30), stringsAsFactors = FALSE)

for (i in 1:length(web_data_no_tags)) {
  win_loss$Team[i] <- web_data_no_tags[[ c(i,2) ]]
  win_loss$Wins[i] <- as.integer(web_data_no_tags[[ c(i,3) ]])
  win_loss$Losses[i] <- as.integer(web_data_no_tags[[ c(i,4) ]])
  win_loss$Win_Perc[i] <- as.numeric(web_data_no_tags[[ c(i,5) ]])

}

saveRDS(win_loss, file = "./data/win_loss.RDS")
saveRDS(team_data, "./data/team_data.RDS")

