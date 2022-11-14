data <- read.csv("G:/ML/project/data.csv")
summary(data)

require(tidyverse)
##plots 

ggplot(data, aes(x=FGA_H , y=FGM_H, color=HOME_TEAM_WINS)) + geom_point() +
  labs(title = "Field Goals Attempted and Made by HOME to WIN")
ggplot(data, aes(x=FG3A_H , y=FG3M_H, color=HOME_TEAM_WINS)) + geom_point() +
  labs(title = "3 points Attempted and Made to win")

ggplot(data, aes(x=FTA_H , y=FTM_H, color=HOME_TEAM_WINS)) + geom_point() +
  labs(title = "Free Throws Attempted and Made to win")

ggplot(data, aes(x=OREB_H , y=DREB_H, color=HOME_TEAM_WINS)) + geom_point() +
  labs(title = "Offensive and DEfensive rebound to win")
ggplot(data, aes(x=REB_H , y=DREB_H, color=HOME_TEAM_WINS)) + geom_point() +
  labs(title = "Rebound and DEfensive rebound to win")

ggplot(data, aes(x=AST_H , y=STL_H, color=HOME_TEAM_WINS)) + geom_point() +
  labs(title = "Assist and steals to win")
ggplot(data, aes(x=BLK_H , y=TO_H, color=HOME_TEAM_WINS)) + geom_point() +
  labs(title = "BLockes shots and Turnover to win")

ggplot(data, aes(x=PF_H , y=TO_H, color=HOME_TEAM_WINS)) + geom_point() +
  labs(title = "Personal Fouls and Turnover to win")

ggplot(data, aes(x=FGM_H , y=FG3M_H, color=HOME_TEAM_WINS, size= FTM_H)) + geom_point() +
  labs(title = "Field Goals, 3points and free throws to win")

ggplot(data, aes(x=FGM_H , y=FG3M_H, color=HOME_TEAM_WINS, size= AST_H)) + geom_point() +
  labs(title = "Field Goals, 3points and assists to win")

ggplot(data, aes(x=FGM_H , y=FG3M_H, color=HOME_TEAM_WINS, size= DREB_H)) + geom_point() +
  labs(title = "Field Goals, 3points and defensive rebounds to win")

ggplot(data, aes(x=FGM_H , y=FG3M_H, color=HOME_TEAM_WINS, size= STL_H)) + geom_point() +
  labs(title = "Field Goals, 3points and steals to win")

