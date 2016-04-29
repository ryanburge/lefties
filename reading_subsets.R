setwd("D:/Baseball")

library(lubridate)

master <- read.csv("D:/Baseball/Master.csv", stringsAsFactors = FALSE)
salary <- read.csv("salaries.csv", stringsAsFactors = FALSE)
pitching <- read.csv("pitching.csv", stringsAsFactors = FALSE)


master$finalGame <- as.Date(master$finalGame, "%m/%d/%Y")
master$year <- year(master$finalGame)


master <- subset(master, master$year >=2005)
pitching <- subset(pitching, pitching$year >=2005)
salary <- subset(salary, yearID >=2005)

salary$year <- salary$yearID
salary$player_id <- salary$playerID
salary$yearID <- NULL
salary$playerID <- NULL

df <- merge(salary, pitching, by=c("year", "player_id"))

master$player_id <- master$playerID

df <- merge(df, master, by=c("year", "player_id"))

df$throw <- df$throws.y