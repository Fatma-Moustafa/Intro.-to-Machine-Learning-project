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
test_seat<-subset(test_set, select = -c(1,2,3,4,23,41,45,46,50,51))

require(rlang)
library(caret)
require(tidyverse)
require(e1071)
library(rminer)

#ALL variables included(40)
M0=HOME_TEAM_WINS ~ .
#Accuracy : 0.9593 

#BEST TOP 16 important variables
M_best16= HOME_TEAM_WINS ~ FGP_H+FGP_A+RANKING_H+FGM_H+FGM_A+FTM_A+RANKING_A+TO_A+TO_H+FG3M_A+FG3M_H+REB_A+FTP_A+REB_H +AST_A+PF_A+FTM_H+ OREB_A 
##Accuracy : 0.9612  

#BEST TOP 19 important variables
M_best19= HOME_TEAM_WINS ~ FGP_H+FGP_A+RANKING_H+FGM_H+FGM_A+FTM_A+RANKING_A+TO_A+TO_H+FG3M_A+FG3M_H+REB_A+FTP_A+REB_H +AST_A+PF_A+FTM_H+ OREB_A +DREB_H+LOSE_A+FGA_H 
##Accuracy : 0.9649 

cv.error = function(formula, learner, data, k, ...) {
  indexes = sample(nrow(data))
  errs = c(1:k) %>% map_dbl(function(i) {
    indexes.test = indexes[c((nrow(data)/k*(i-1)+1):(nrow(data)/k*i))]
    m = learner(formula, data[-indexes.test,], ...)
    predicted.y = predict(m, data[indexes.test,], type = "class")
    actual.y = data[indexes.test, as.character(f_lhs(formula))]
    confusion.matrix = table(actual.y, predicted.y)
    1-sum(diag(confusion.matrix))/sum(confusion.matrix)
  })
  names(errs) = paste0("fold", c(1:k))
  errs
}

results0 = expand_grid(kernel=c("linear","polynomial","radial","sigmoid"), cost=exp(seq(-6,8,1))) %>% rowwise() %>% mutate(error = mean(cv.error(M0, svm, train_set, 10, kernel=kernel, cost=cost, degree=2)))
results0 %>% ggplot(aes(x=cost,y=error,color=kernel)) + geom_line() + scale_x_log10() + geom_point()

results1 = expand_grid(kernel=c("linear","polynomial","radial","sigmoid"), cost=exp(seq(-6,8,1))) %>% rowwise() %>% mutate(error = mean(cv.error(M_best16, svm, train_set, 10, kernel=kernel, cost=cost, degree=2)))
results1 %>% ggplot(aes(x=cost,y=error,color=kernel)) + geom_line() + scale_x_log10() + geom_point()

results2 = expand_grid(kernel=c("linear","polynomial","radial","sigmoid"), cost=exp(seq(-6,8,1))) %>% rowwise() %>% mutate(error = mean(cv.error(M_best19, svm, train_set, 10, kernel=kernel, cost=cost, degree=2)))
results2 %>% ggplot(aes(x=cost,y=error,color=kernel)) + geom_line() + scale_x_log10() + geom_point()


