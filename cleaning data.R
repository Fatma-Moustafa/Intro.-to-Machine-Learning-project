## load datasets
games <- read.csv("G:/ML/project/games.csv")
games_details <- read.csv("G:/ML/project/games_details.csv")
players <- read.csv("G:/ML/project/players.csv")
ranking <- read.csv("G:/ML/project/ranking.csv")
teams <- read.csv("G:/ML/project/teams.csv")

##to get a look at the games dataset
summary(games)
library(dplyr)

d <- games_details %>% select(-3: -10, -13, -16, -19, -29)
summary(d)
d <- na.exclude(d)
summary(d)
d <- d %>%group_by(GAME_ID, TEAM_ID) %>% summarise(across(everything(), sum))
summary(d)

## to remove the columns with useless data

g<- games %>% select(-3,-7: -20)
g <- na.exclude(g)
# te rename HOME_TEAM to TEAM


#merge games and details into one dataset
gd <- merge(g,d,by="GAME_ID")

# select merged dataset of home and for away
gd_home <- subset(gd, TEAM_ID == HOME_TEAM_ID )
names(gd_home)[8] <- "FGM_H"
names(gd_home)[9] <- "FGA_H"
names(gd_home)[10] <- "FG3M_H"
names(gd_home)[11] <- "FG3A_H"
names(gd_home)[12] <- "FTM_H"
names(gd_home)[13] <- "FTA_H"
names(gd_home)[14] <- "OREB_H"
names(gd_home)[15] <- "DREB_H"
names(gd_home)[16] <- "REB_H"
names(gd_home)[17] <- "AST_H"
names(gd_home)[18] <- "STL_H"
names(gd_home)[19] <- "BLK_H"
names(gd_home)[20] <- "TO_H"
names(gd_home)[21] <- "PF_H"
names(gd_home)[22] <- "PTS_H"
gd_home<- gd_home %>% select(-7)

gd_away <- subset(gd, TEAM_ID == VISITOR_TEAM_ID)
names(gd_away)[8] <- "FGM_A"
names(gd_away)[9] <- "FGA_A"
names(gd_away)[10] <- "FG3M_A"
names(gd_away)[11] <- "FG3A_A"
names(gd_away)[12] <- "FTM_A"
names(gd_away)[13] <- "FTA_A"
names(gd_away)[14] <- "OREB_A"
names(gd_away)[15] <- "DREB_A"
names(gd_away)[16] <- "REB_A"
names(gd_away)[17] <- "AST_A"
names(gd_away)[18] <- "STL_A"
names(gd_away)[19] <- "BLK_A"
names(gd_away)[20] <- "TO_A"
names(gd_away)[21] <- "PF_A"
names(gd_away)[22] <- "PTS_A"
gd_away<- gd_away %>% select(-7)


#merge home and away into one dataset
gdHA <- merge(gd_home,gd_away,all=TRUE)


summary(gdHA)


# to have the dataset divided by seasons
test_season <- subset(gdHA, SEASON == '2020' )



