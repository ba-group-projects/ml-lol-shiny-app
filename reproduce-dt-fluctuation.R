# import libraries
library(caret)
library(tree)
library(randomForest)
library(dplyr)
library(corrplot)
# library(rattle)
# library(rpart)
# library(rpart.plot)

source("custom-functions.r")

# read data
lol.ori = read.csv("high_diamond_ranked_10min.csv", header = TRUE)

# summary of data
# str(lol.ori)
# summary(lol.ori)

# pre-process data
sum(is.na(lol.ori)) # check missing values
lol.ori = lol.ori[,-1]

set.seed(100)
lol.blue = lol.ori[sample(which(lol.ori$blueWins == 1,), 240),]
lol.red = lol.ori[sample(which(lol.ori$blueWins == 0,), 240),]
lol.sample = rbind(lol.blue, lol.red)

# feature engineering
par(mfrow=c(1,1))
blue.features = lol.sample[,c(2:20)]
# corrplot(cor(blue.features), tl.col = "black", diag = FALSE)
drop.blue.features = c('blueDragons', 'blueHeralds', 'blueKills', 
                       'blueDeaths', 'blueAssists', 'blueTotalGold',
                       'blueAvgLevel', 'blueTotalExperience', 
                       'blueExperienceDiff', 'blueGoldPerMin', 
                       'blueTotalMinionsKilled')
blue.features = blue.features[, !(colnames(blue.features) %in% drop.blue.features)]

red.features = lol.sample[,-c(1, 2:20)]
# corrplot(cor(blue.features, red.features), tl.col = "black", diag = TRUE)
drop.red.features = c('redFirstBlood','redKills', 'redDeaths', 'redAssists', 
                      'redEliteMonsters', 'redDragons', 'redTotalGold', 
                      'redAvgLevel', 'redTotalExperience', 
                      'redTotalMinionsKilled', 'redGoldDiff', 
                      'redExperienceDiff', 'redCSPerMin', 'redGoldPerMin')
red.features = red.features[, !(colnames(red.features) %in% drop.red.features)]
blueWins = lol.sample$blueWins

lol = cbind(blue.features, red.features, blueWins)
# corrplot(cor(lol), tl.col = "black", diag = FALSE)
drop.more.features = c('blueWardsPlaced', 'blueWardsDestroyed', 'blueTowersDestroyed',
                       'redWardsPlaced', 'redWardsDestroyed', 'redTowersDestroyed')
lol = lol[, !(colnames(lol) %in% drop.more.features)]

# modify classification column
lol$blueWins[lol$blueWins == 1] <- "Blue"
lol$blueWins[lol$blueWins == 0] <- "Red"
lol$blueWins = factor(lol$blueWins)

set.seed(100)
# random split to training and test set
train.index1 = createDataPartition(lol$blueWins, p = 0.6, list = FALSE)
train1 = lol[train.index1,]
test1 = lol[-train.index1,]

set.seed(100)
train.index2 = createDataPartition(lol$blueWins, p = 0.7, list = FALSE)
train2 = lol[train.index2,]
test2 = lol[-train.index2,]

#######################
# BASIC DECISION TREE #
#######################

par(mfrow=c(1,2))
# >>> TRAIN 1 <<<
### tree library
set.seed(100)
lol.dt.basic1 = tree(blueWins ~ . , train1)
summary(lol.dt.basic1)
# have a look at the details of the tree
lol.dt.basic1
# # plot the tree
plot(lol.dt.basic1)
title("60% Training Set")
text(lol.dt.basic1,pretty=1)

# performance measure using confusion matrix
pred.dt.basic1 = predict(lol.dt.basic1, newdata = test1[,-ncol(test1)], type = "class")
confusionMatrix(pred.dt.basic1, test1[,ncol(test1)])

# >>> TRAIN 2 <<<
### tree library
set.seed(100)
lol.dt.basic2 = tree(blueWins ~ . , train2)
summary(lol.dt.basic2)
# have a look at the details of the tree
lol.dt.basic2
# # plot the tree
plot(lol.dt.basic2)
title("70% Training Set")
text(lol.dt.basic2,pretty=1)

# performance measure using confusion matrix
pred.dt.basic2 = predict(lol.dt.basic2, newdata = test2[,-ncol(test2)], type = "class")
confusionMatrix(pred.dt.basic2, test2[,ncol(test2)])

########################
# PRUNED DECISION TREE #
########################

par(mfrow=c(1,2))
# >>> TRAIN 1 <<<
# cross validation to select best tuning parameter alpha (cost complexity pruning)
fitcontrol.dt = trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(100)
# train and prune decision tree
lol.dt.pruned1 = train(train1[,-ncol(lol)], train1[,ncol(lol)], method = "rpart",
                      metric="Accuracy", tuneLength=10,
                      trControl = fitcontrol.dt) # >>> rpart = alpha

test = train(train1[,-ncol(lol)], train1[,ncol(lol)], method = "rpart", cp = 0)
test
# mtryGrid=expand.grid(cp=c(0,0.0001,0.001,0.01,0.1))
# lol.dt.pruned1 = train(train1[,-ncol(lol)], train1[,ncol(lol)], method = "rpart",
#                       metric="Accuracy", tuneGrid=mtryGrid, trControl = fitcontrol.dt)

# summary of decision tree
lol.dt.pruned1

#To look at the details of this tree
print(lol.dt.pruned1$finalModel)

# plot decision tree
plot(lol.dt.pruned1$finalModel, main = "60% Training Set")
text(lol.dt.pruned1$finalModel,pretty=1)

# performance measure using confusion matrix
pred.dt.pruned1 = predict(lol.dt.pruned1, newdata = test1[,-ncol(test1)])
confusionMatrix(pred.dt.pruned1, test1[,ncol(test1)])

# get fancy trees by rattle
# fancyRpartPlot(lol.dt.pruned1$finalModel, palettes=c("Blues", "Reds"))

# >>> TRAIN 2 <<<
# cross validation to select best tuning parameter alpha (cost complexity pruning)
fitcontrol.dt = trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(100)
# train and prune decision tree
lol.dt.pruned2 = train(train2[,-ncol(lol)], train2[,ncol(lol)], method = "rpart",
                      metric="Accuracy", tuneLength=10,
                      trControl = fitcontrol.dt) # >>> rpart = alpha

# mtryGrid=expand.grid(cp=c(0,0.0001,0.001,0.01,0.1))
# lol.dt.pruned2 = train(train2[,-ncol(lol)], train2[,ncol(lol)], method = "rpart",
#                        metric="Accuracy", tuneGrid=mtryGrid, trControl = fitcontrol.dt)

# summary of decision tree
lol.dt.pruned2

#To look at the details of this tree
print(lol.dt.pruned2$finalModel)

# plot decision tree
plot(lol.dt.pruned2$finalModel, main = "70% Training Set")
text(lol.dt.pruned2$finalModel,pretty=1)

# performance measure using confusion matrix
pred.dt.pruned2 = predict(lol.dt.pruned2, newdata = test2[,-ncol(test2)])
confusionMatrix(pred.dt.pruned2, test2[,ncol(test2)])

#######################
# BASIC RANDOM FOREST #
#######################
par(mfrow=c(1,1))
# >>> TRAIN 1 <<<
set.seed(5)
lol.rf.basic1=randomForest(blueWins~.,data=train1,mtry=sqrt(ncol(lol)-1),
                          importance=TRUE,ntree=500)
lol.rf.basic1

# performance measure using confusion matrix
pred.rf.basic1=predict(lol.rf.basic1,newdata=test1[,-ncol(test1)])
confusionMatrix(pred.rf.basic1, test1[,ncol(test1)])

#######importance of variables
# detach(package:rattle)
importance(lol.rf.basic1) # >>> variable importance measure
varImpPlot(lol.rf.basic1)

# >>> TRAIN 2 <<<
set.seed(5)
lol.rf.basic2=randomForest(blueWins~.,data=train2,mtry=sqrt(ncol(lol)-1),
                           importance=TRUE,ntree=500)
lol.rf.basic2

# performance measure using confusion matrix
pred.rf.basic2=predict(lol.rf.basic2,newdata=test2[,-ncol(test2)])
confusionMatrix(pred.rf.basic2, test2[,ncol(test2)])

#######importance of variables
# detach(package:rattle)
importance(lol.rf.basic2) # >>> variable importance measure
varImpPlot(lol.rf.basic2)

#######################
# TUNED RANDOM FOREST #
#######################

# >>> TRAIN 1 <<<
# cross validation to select m features to randomly sample
fitcontrol.rf = trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(6)
# train random forest
lol.rf.tuned1 = train(train1[,-ncol(lol)], train1[,ncol(lol)], method = "rf", 
                     metric="Accuracy", tuneLength=5, 
                     trControl = fitcontrol.rf, ntree = 500) # >>> rpart = alpha

# summary of decision tree
lol.rf.tuned1

# performance measure using confusion matrix
pred.rf.tuned1 = predict(lol.rf.tuned1, newdata = test1[,-ncol(test1)])
confusionMatrix(pred.rf.tuned1, test1[,ncol(test1)])

#To look at the details of this tree
print(lol.rf.tuned1$finalModel)

# variable importance
varImp(lol.rf.tuned1)
plot(varImp(lol.rf.tuned1), main="lol.rf.tuned1")

# >>> TRAIN 2 <<<
# cross validation to select m features to randomly sample
fitcontrol.rf = trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(6)
# train random forest
lol.rf.tuned2 = train(train2[,-ncol(lol)], train2[,ncol(lol)], method = "rf", 
                      metric="Accuracy", tuneLength=5, 
                      trControl = fitcontrol.rf, ntree = 500) # >>> rpart = alpha

# summary of decision tree
lol.rf.tuned2

# performance measure using confusion matrix
pred.rf.tuned2 = predict(lol.rf.tuned2, newdata = test2[,-ncol(test2)])
confusionMatrix(pred.rf.tuned2, test2[,ncol(test2)])

#To look at the details of this tree
print(lol.rf.tuned2$finalModel)

# variable importance
varImp(lol.rf.tuned2)
plot(varImp(lol.rf.tuned2), main="lol.rf.tuned2")

################################################################################

# # compare decision tree summary
# summary(lol.dt.basic)
# lol.dt.pruned
# 
# # compare random forest summary
# lol.rf.basic
# lol.rf.tuned
# lol.rf.tuned$finalModel

# compare decision tree performance
confusionMatrix(pred.dt.basic1, test1[,ncol(test1)])
confusionMatrix(pred.dt.pruned1, test1[,ncol(test1)])

confusionMatrix(pred.dt.basic2, test2[,ncol(test2)])
confusionMatrix(pred.dt.pruned2, test2[,ncol(test2)])

# compare random forest performance
confusionMatrix(pred.rf.basic1, test1[,ncol(test1)])
confusionMatrix(pred.rf.tuned1, test1[,ncol(test1)])

confusionMatrix(pred.rf.basic2, test2[,ncol(test2)])
confusionMatrix(pred.rf.tuned2, test2[,ncol(test2)])
