# import libraries
library(caret)
library(tree)
library(randomForest)
library(dplyr)
library(rattle)
library(corrplot)

# read data
lol.ori = read.csv("high_diamond_ranked_10min.csv", header = TRUE)

# summary of data
str(lol.ori)
summary(lol.ori)

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
                       'blueAvgLevel', 'blueTotalExperience', 'blueExperienceDiff',
                       'blueGoldPerMin', 'blueTotalMinionsKilled')
blue.features = blue.features[, !(colnames(blue.features) %in% drop.blue.features)]

red.features = lol.sample[,-c(1, 2:20)]
corrplot(cor(blue.features, red.features), tl.col = "black", diag = TRUE)
drop.red.features = c('redFirstBlood','redKills', 'redDeaths', 'redAssists', 
                      'redEliteMonsters', 'redDragons', 'redTotalGold', 
                      'redAvgLevel', 'redTotalExperience', 'redTotalMinionsKilled',
                      'redGoldDiff', 'redExperienceDiff', 'redCSPerMin', 'redGoldPerMin')
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
# train.index = createDataPartition(lol$blueWins, p = 0.6, list = FALSE)
train.index = createDataPartition(lol$blueWins, p = 0.7, list = FALSE)
train = lol[train.index,]
test = lol[-train.index,]

#################
# decision tree #
#################

set.seed(100)
lol.dt.normal = tree(blueWins ~ . , lol)
summary(lol.dt.normal)
# have a look at the details of the tree
lol.dt.normal
# # plot the tree
plot(lol.dt.normal)
text(lol.dt.normal,pretty=2)

pred.dt.normal = predict(lol.dt.normal, newdata = test[,-ncol(test)],type="class")
mean(pred.dt.normal == test[,ncol(test)])

########################
# decision tree pruned #
########################

# cross validation to select best tuning parameter alpha (cost complexity pruning)
fitcontrol.dt = trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(50)
# train and prune decision tree
# lol.dt.pruned = train(train[,-ncol(lol)], train[,ncol(lol)], method = "rpart",
#                       metric="Accuracy", tuneLength=5, trControl = fitcontrol.dt) # >>> rpart = alpha

mtryGrid=expand.grid(cp=c(0,0.0001,0.001,0.01,0.1))
# lol.rf = train(train[,-ncol(lol)], train[,ncol(lol)], method = "rf",
#                metric="Accuracy", tuneGrid=mtryGrid, trControl = fitcontrol.rf)
lol.dt.pruned = train(train[,-ncol(lol)], train[,ncol(lol)], method = "rpart",
                      metric="Accuracy", tuneGrid=mtryGrid, trControl = fitcontrol.dt)

# summary of decision tree
lol.dt.pruned

# performance measure
pred.dt.pruned = predict(lol.dt.pruned, newdata = test[,-ncol(test)])
mean(pred.dt.pruned == test[,ncol(test)])

#To look at the details of this tree
print(lol.dt.pruned$finalModel)

# get fancy trees by rattle
fancyRpartPlot(lol.dt.pruned$finalModel)

#################
# random forest #
#################

set.seed(50)
lol.rf.normal=randomForest(blueWins~.,data=lol,mtry=sqrt(ncol(lol)-1),
                           importance=TRUE,ntree=500)
lol.rf.normal
pred.rf.normal=predict(lol.rf.normal,newdata=test[,-ncol(test)])
mean(pred.rf.normal==test[,ncol(test)])
#######importance of variables
detach(package:rattle)
importance(lol.rf.normal) # >>> variable importance measure
varImpPlot(lol.rf.normal)

#######################
# random forest tuned #
#######################

# cross validation to select m features to randomly sample
fitcontrol.rf = trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(100)
# train random forest
lol.rf.tuned = train(train[,-ncol(lol)], train[,ncol(lol)], method = "rf", metric="Accuracy", 
               tuneLength=5, trControl = fitcontrol.rf, ntree = 500) # >>> rpart = alpha

# mtryGrid=expand.grid(mtry=c(2,3,4,5,6))
# lol.rf = train(train[,-ncol(lol)], train[,ncol(lol)], method = "rf",
#                metric="Accuracy", tuneGrid=mtryGrid, trControl = fitcontrol.rf)

# summary of decision tree
lol.rf.tuned

# performance measure
pred.rf.tuned = predict(lol.rf.tuned, newdata = test[,-ncol(test)])
mean(pred.rf.tuned == test[,ncol(test)])

#To look at the details of this tree
print(lol.rf.tuned$finalModel)

# variable importance
varImp(lol.rf.tuned)
plot(varImp(lol.rf.tuned))

#####################################################################

lol.dt.normal
lol.dt.pruned$finalModel
