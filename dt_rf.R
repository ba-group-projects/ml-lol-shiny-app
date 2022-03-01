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
corrplot(cor(blue.features), tl.col = "black", diag = FALSE)
drop.blue.features = c('blueDragons', 'blueHeralds', 'blueKills', 
                       'blueDeaths', 'blueAssists', 'blueTotalGold',
                       'blueAvgLevel', 'blueTotalExperience', 
                       'blueExperienceDiff', 'blueGoldPerMin', 
                       'blueTotalMinionsKilled')
blue.features = blue.features[, !(colnames(blue.features) %in% drop.blue.features)]

red.features = lol.sample[,-c(1, 2:20)]
corrplot(cor(blue.features, red.features), tl.col = "black", diag = TRUE)
drop.red.features = c('redFirstBlood','redKills', 'redDeaths', 'redAssists', 
                      'redEliteMonsters', 'redDragons', 'redTotalGold', 
                      'redAvgLevel', 'redTotalExperience', 
                      'redTotalMinionsKilled', 'redGoldDiff', 
                      'redExperienceDiff', 'redCSPerMin', 'redGoldPerMin')
red.features = red.features[, !(colnames(red.features) %in% drop.red.features)]
blueWins = lol.sample$blueWins

lol = cbind(blue.features, red.features, blueWins)
corrplot(cor(lol), tl.col = "black", diag = FALSE)
drop.more.features = c('blueWardsPlaced', 'blueWardsDestroyed', 'blueTowersDestroyed',
                       'redWardsPlaced', 'redWardsDestroyed', 'redTowersDestroyed')
lol = lol[, !(colnames(lol) %in% drop.more.features)]

# modify classification column
lol$blueWins[lol$blueWins == 1] <- "Blue"
lol$blueWins[lol$blueWins == 0] <- "Red"
lol$blueWins = factor(lol$blueWins)

set.seed(100)
# random split to training and test set
train.index = createDataPartition(lol$blueWins, p = 0.6, list = FALSE)
# train.index = createDataPartition(lol$blueWins, p = 0.7, list = FALSE)
train = lol[train.index,]
test = lol[-train.index,]

#######################
# basic decision tree #
#######################

### tree library
set.seed(100)
lol.dt.basic = tree(blueWins ~ . , lol)
summary(lol.dt.basic)
# have a look at the details of the tree
lol.dt.basic
# # plot the tree
plot(lol.dt.basic)
text(lol.dt.basic,pretty=1)

# performance measure using confusion matrix
pred.dt.basic = predict(lol.dt.basic, newdata = test[,-ncol(test)], type = "class")
confusionMatrix(pred.dt.basic, test[,ncol(test)])

# ### rpart library
# set.seed(100)
# lol.dt.basic2 = rpart(blueWins ~ . , lol, method = 'class', cp = 10)
# summary(lol.dt.basic2)
# # have a look at the details of the tree
# lol.dt.basic2
# # # plot the tree
# # plot(lol.dt.basic2)
# # text(lol.dt.basic2,pretty=2)
# rpart.plot(lol.dt.basic2, box.palette="BuRd")
# 
# # performance measure using confusion matrix
# pred.dt.basic2 = predict(lol.dt.basic2, newdata = test[,-ncol(test)],type="class")
# confusionMatrix(pred.dt.basic2, test[,ncol(test)])

########################
# pruned decision tree #
########################

# cross validation to select best tuning parameter alpha (cost complexity pruning)
fitcontrol.dt = trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(100)
# train and prune decision tree
# lol.dt.pruned = train(train[,-ncol(lol)], train[,ncol(lol)], method = "rpart",
#                       metric="Accuracy", tuneLength=10, 
#                       trControl = fitcontrol.dt) # >>> rpart = alpha

mtryGrid=expand.grid(cp=c(0,0.0001,0.001,0.01,0.1))
lol.dt.pruned = train(train[,-ncol(lol)], train[,ncol(lol)], method = "rpart",
                      metric="Accuracy", tuneGrid=mtryGrid, trControl = fitcontrol.dt)

# summary of decision tree
lol.dt.pruned

#To look at the details of this tree
print(lol.dt.pruned$finalModel)

# plot decision tree
plot(lol.dt.pruned$finalModel)
text(lol.dt.pruned$finalModel,pretty=1)

# performance measure using confusion matrix
pred.dt.pruned = predict(lol.dt.pruned, newdata = test[,-ncol(test)])
confusionMatrix(pred.dt.pruned, test[,ncol(test)])

# get fancy trees by rattle
# fancyRpartPlot(lol.dt.pruned$finalModel, palettes=c("Blues", "Reds"))

#######################
# basic random forest #
#######################

set.seed(5)
lol.rf.basic=randomForest(blueWins~.,data=lol,mtry=sqrt(ncol(lol)-1),
                          importance=TRUE,ntree=500)
lol.rf.basic

# performance measure using confusion matrix
pred.rf.basic=predict(lol.rf.basic,newdata=test[,-ncol(test)])
confusionMatrix(pred.rf.basic, test[,ncol(test)])

#######importance of variables
# detach(package:rattle)
importance(lol.rf.basic) # >>> variable importance measure
varImpPlot(lol.rf.basic)

#######################
# tuned random forest #
#######################

# cross validation to select m features to randomly sample
fitcontrol.rf = trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(6)
# train random forest
lol.rf.tuned = train(train[,-ncol(lol)], train[,ncol(lol)], method = "rf", 
                     metric="Accuracy", tuneLength=5, 
                     trControl = fitcontrol.rf, ntree = 500) # >>> rpart = alpha

# mtryGrid=expand.grid(mtry=c(2,3,4,5,6))
# lol.rf = train(train[,-ncol(lol)], train[,ncol(lol)], method = "rf",
#                metric="Accuracy", tuneGrid=mtryGrid, trControl = fitcontrol.rf)

# summary of decision tree
lol.rf.tuned

# performance measure using confusion matrix
pred.rf.tuned = predict(lol.rf.tuned, newdata = test[,-ncol(test)])
confusionMatrix(pred.rf.tuned, test[,ncol(test)])

#To look at the details of this tree
print(lol.rf.tuned$finalModel)

# variable importance
varImp(lol.rf.tuned)
plot(varImp(lol.rf.tuned))

################################################################################

# compare decision tree summary
summary(lol.dt.basic)
lol.dt.pruned

# compare random forest summary
lol.rf.basic
lol.rf.tuned
lol.rf.tuned$finalModel

# compare decision tree performance
confusionMatrix(pred.dt.basic, test[,ncol(test)])
confusionMatrix(pred.dt.pruned, test[,ncol(test)])

# compare random forest performance
confusionMatrix(pred.rf.basic, test[,ncol(test)])
confusionMatrix(pred.rf.tuned, test[,ncol(test)])
