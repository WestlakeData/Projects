#Library Calls ####
library(tidyverse)

#Run Supporting Scripts ####
source("./scripts/Scraping.R")

#Load Data ####
win_loss <- readRDS("./data/win_loss.RDS")
team_data <- readRDS("./data/team_data.RDS")