
s19 <- read.csv("G:/ML/project/modification 1/Season Data/s2019.csv")
s20 <- read.csv("G:/ML/project/modification 1/Season Data/s2020.csv")
s21<- read.csv("G:/ML/project/modification 1/Season Data/s2021.csv")
data<- rbind(s19,s20,s21)
size <- floor(0.80*nrow(data))
set.seed(1234)
data$HOME_TEAM_WINS <- as.factor(data$HOME_TEAM_WINS)
train_ind<- sample(seq_len(nrow(data)), size=size)
train_set <- data[train_ind, ]
test_set <- data[-train_ind, ]
train_set<-subset(train_set, select = -c(1,2,3,4,23,41,45,46,50,51))
test_set<-subset(test_set, select = -c(1,2,3,4,23,41,45,46,50,51))

library(caret)
require(tidyverse)
require(e1071)
library(rminer)
library(pROC)

F0 =svm(HOME_TEAM_WINS ~ ., data= train_set, importance=T)
 
w <- t(F0$coefs) %*% F0$SV                 # weight vectors
w <- apply(w, 2, function(v){sqrt(sum(v^2))})  # weight
w <- sort(w, decreasing = T)
print(w)

#TOP 5 important variables
M5=HOME_TEAM_WINS ~ FGP_H+FGP_A+RANKING_H+FGM_H+FGM_A
F5 =svm(M5 , data= train_set)
F5
pred5= predict(F5, test_set)
confusionMatrix(table(pred5, test_set$HOME_TEAM_WINS))
roc5 <- roc( test_set$HOME_TEAM_WINS, predictor =as.numeric(pred5))
plot(roc5, col = "blue")
auc(roc5)
##Accuracy : 0.8595  

#TOP 10 important variables
M10=HOME_TEAM_WINS ~ FGP_H+FGP_A+RANKING_H+FGM_H+FGM_A+FTM_A+RANKING_A+TO_A+FTA_A+TO_H
F10 =svm(M10 , data= train_set)
F10
pred10= predict(F10, test_set)
confusionMatrix(table(pred10, test_set$HOME_TEAM_WINS))
roc10 <- roc( test_set$HOME_TEAM_WINS, predictor =as.numeric(pred10))
plot(roc10, col = "blue")
auc(roc10)
##Accuracy : 0.8909  


#TOP 15 important variables
M15=HOME_TEAM_WINS ~ FGP_H+FGP_A+RANKING_H+FGM_H+FGM_A+FTM_A+RANKING_A+TO_A+FTA_A+TO_H+FG3P_H+FG3M_A+FG3P_A+FG3M_H+PF_H
F15 =svm( M15 , data= train_set)
F15
pred15= predict(F15, test_set)
confusionMatrix(table(pred15, test_set$HOME_TEAM_WINS))
roc15 <- roc( test_set$HOME_TEAM_WINS, predictor =as.numeric(pred15))
plot(roc15, col = "blue")
auc(roc15)
## Accuracy : 0.8909

#TOP 20 important variables
M20=HOME_TEAM_WINS ~ FGP_H+FGP_A+RANKING_H+FGM_H+FGM_A+FTM_A+RANKING_A+TO_A+FTA_A+TO_H+FG3P_H+FG3M_A+FG3P_A+FG3M_H+PF_H+REB_A+STL_A+DREB_A+FTP_A+REB_H
F20 =svm( M20 , data= train_set)
F20
pred20= predict(F20, test_set)
confusionMatrix(table(pred20, test_set$HOME_TEAM_WINS))
roc20 <- roc( test_set$HOME_TEAM_WINS, predictor =as.numeric(pred20))
plot(roc20, col = "blue")
auc(roc20)
## Accuracy : 0.9131 

#TOP 25 important variables
M25= HOME_TEAM_WINS ~ FGP_H+FGP_A+RANKING_H+FGM_H+FGM_A+FTM_A+RANKING_A+TO_A+FTA_A+TO_H +FG3P_H+FG3M_A+FG3P_A+FG3M_H+PF_H+REB_A+STL_A+DREB_A+FTP_A+REB_H +AST_A+PF_A+FTM_H+OREB_H+ OREB_A 
F25 =svm(M25, data= train_set)
F25
pred25= predict(F25, test_set)
confusionMatrix(table(pred25, test_set$HOME_TEAM_WINS))
roc25 <- roc( test_set$HOME_TEAM_WINS, predictor =as.numeric(pred25))
plot(roc25, col = "blue")
auc(roc25)
##Accuracy : 0.9501 

#TOP 30 important variables
M30= HOME_TEAM_WINS ~ FGP_H+FGP_A+RANKING_H+FGM_H+FGM_A+FTM_A+RANKING_A+TO_A+FTA_A+TO_H +FG3P_H+FG3M_A+FG3P_A+FG3M_H+PF_H+REB_A+STL_A+DREB_A+FTP_A+REB_H +AST_A+PF_A+FTM_H+OREB_H+ OREB_A+DREB_H+FTA_H+LOSE_A+FGA_H+LOSE_H 
F30 =svm(M30, data= train_set)
F30
pred30= predict(F30, test_set)
confusionMatrix(table(pred30, test_set$HOME_TEAM_WINS))
roc30 <- roc( test_set$HOME_TEAM_WINS, predictor =as.numeric(pred30))
plot(roc30, col = "blue")
auc(roc30)
##Accuracy : 0.9575 

#TOP 35 important variables
M35= HOME_TEAM_WINS ~ FGP_H+FGP_A+RANKING_H+FGM_H+FGM_A+FTM_A+RANKING_A+TO_A+FTA_A+TO_H +FG3P_H+FG3M_A+FG3P_A+FG3M_H+PF_H+REB_A+STL_A+DREB_A+FTP_A+REB_H +AST_A+PF_A+FTM_H+OREB_H+ OREB_A+DREB_H+FTA_H+LOSE_A+FGA_H+LOSE_H+BLK_A+FTP_H+AST_H+FG3A_A+BLK_H  
F35 =svm(M35, data= train_set)
F35
pred35= predict(F35, test_set)
confusionMatrix(table(pred35, test_set$HOME_TEAM_WINS))
roc35 <- roc( test_set$HOME_TEAM_WINS, predictor =as.numeric(pred35))
plot(roc35, col = "blue")
auc(roc35)
##Accuracy : 0.9538 

#ALL variables included(40)
M40=HOME_TEAM_WINS ~ FGP_H+FGP_A+RANKING_H+FGM_H+FGM_A+FTM_A+RANKING_A+TO_A+FTA_A+TO_H +FG3P_H+FG3M_A+FG3P_A+FG3M_H+PF_H+REB_A+STL_A+DREB_A+FTP_A+REB_H +AST_A+PF_A+FTM_H+OREB_H+ OREB_A+DREB_H+FTA_H+LOSE_A+FGA_H+LOSE_H+BLK_A+FTP_H+AST_H+FG3A_A+BLK_H+ WIN_H+WIN_A +STL_H +FGA_A+FG3A_H 
F40 =svm(M40, data= train_set, importance=T)
F40
pred40= predict(F40, test_set)
confusionMatrix(table(pred40, test_set$HOME_TEAM_WINS))
roc40 <- roc( test_set$HOME_TEAM_WINS, predictor =as.numeric(pred40))
plot(roc40, col = "blue")
auc(roc40)
#Accuracy : 0.9593

library(ggplot2)

IMP_VAR= c(5,10,15,20,25,30,35,40)
perc= seq(80,100, by = 0.5)
ACC = c(85.95, 89.09, 89.09, 91.31, 95.01, 95.75, 95.3,  95.93 )
SEN = c (84.06,  88.45, 85.66, 89.24, 94.02, 94.82,  94.02, 94.42)
SP= c(87.59, 89.66, 92.07, 93.10, 95.86, 96.55, 96.55, 97.24)

df <- data.frame(IMP_VAR, ACC )
theme_set(theme_bw())


ggplot(df, aes(x = IMP_VAR)) +
  geom_line(aes( y = ACC, color = "Accuracy"), size = 1) +
  geom_line(aes( y = SEN, color = "Sensitivity"), size = 1) +
  geom_line(aes( y = SP, color = "Specificity"), size = 1) +
  labs(x = "MOST IMPORTANT VARIABLES",y = "Performance" ,  color = "Legend") +
  scale_y_continuous(limits = c(80, 100)) +
  scale_color_manual(name = "Legend", values = c("Accuracy" = "purple", "Sensitivity" =  "green", "Specificity" =  "hot pink"))


#TOP 10 important variables, remove FTA_A >>9vars
M1=HOME_TEAM_WINS ~ FGP_H+FGP_A+RANKING_H+FGM_H+FGM_A+FTM_A+RANKING_A+TO_A+TO_H
F1 =svm(M1 , data= train_set)
F1
pred1= predict(F1, test_set)
confusionMatrix(table(pred1, test_set$HOME_TEAM_WINS))
##Accuracy : 0.8909  

#TOP 15 important variables, remove FG3P_H, FG3P_A, PF_H>> 11vars
M2=HOME_TEAM_WINS ~ FGP_H+FGP_A+RANKING_H+FGM_H+FGM_A+FTM_A+RANKING_A+TO_A+TO_H+FG3M_A+FG3M_H
F2 =svm( M2 , data= train_set)
F2
pred2= predict(F2, test_set)
confusionMatrix(table(pred2, test_set$HOME_TEAM_WINS))
##  Accuracy : 0.8965

#TOP 20 important variables,remove STL_A, FTP_A,REB_H, DREB_A>>12vars
M3=HOME_TEAM_WINS ~ FGP_H+FGP_A+RANKING_H+FGM_H+FGM_A+FTM_A+RANKING_A+TO_A+TO_H+FG3M_A+FG3M_H+REB_A
F3 =svm( M3 , data= train_set) 
F3
pred3= predict(F3, test_set)
confusionMatrix(table(pred3, test_set$HOME_TEAM_WINS))
##  Accuracy : 0.9205 

#TOP 25 important variables, remove OREB_H >> 16vars
M4= HOME_TEAM_WINS ~ FGP_H+FGP_A+RANKING_H+FGM_H+FGM_A+FTM_A+RANKING_A+TO_A+TO_H+FG3M_A+FG3M_H+REB_A+FTP_A+REB_H +AST_A+PF_A+FTM_H+ OREB_A 
F4 =svm(M4, data= train_set)
F4
pred4= predict(F4, test_set)
confusionMatrix(table(pred4, test_set$HOME_TEAM_WINS))
##Accuracy : 0.9612  

#TOP 30 important variables, remove LOSE_H, FTA_H>>19vars
M5= HOME_TEAM_WINS ~ FGP_H+FGP_A+RANKING_H+FGM_H+FGM_A+FTM_A+RANKING_A+TO_A+TO_H+FG3M_A+FG3M_H+REB_A+FTP_A+REB_H +AST_A+PF_A+FTM_H+ OREB_A +DREB_H+LOSE_A+FGA_H 
F5 =svm(M5, data= train_set)
F5
pred5= predict(F5, test_set)
confusionMatrix(table(pred5, test_set$HOME_TEAM_WINS))
##Accuracy : 0.9649 

#TOP 35 important variables, remove BLK_A, FTP_H, AST_H, FG3A_A, BLK_H >> 19vars
##Accuracy : 0.9649

#ALL variables included(40),  WIN_H,WIN_A,STL_H,FGA_A,FG3A_H >> 19vars
##Accuracy : 0.9649 



