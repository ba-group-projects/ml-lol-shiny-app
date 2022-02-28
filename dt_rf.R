# import libraries
library(caret)
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
last.colname = names(lol.ori)[ncol(lol.ori)] # get header of last column
lol.ori = lol.ori %>% relocate(blueWins, .after = all_of(last.colname)) # reorder classification column
set.seed(100)
lol.blue = lol.ori[sample(which(lol.ori$blueWins == 1,), 240),]
lol.red = lol.ori[sample(which(lol.ori$blueWins == 0,), 240),]
lol = rbind(lol.blue, lol.red)

# correlation matrix
### >>> create 1 corr plot of blue features vs blue features
### >>> create 1 corr plot of blue features vs red features

par(mfrow=c(1,1))
corrplot(cor(lol[,-c(1,ncol(lol))]), tl.col = "black") #, type = "upper"
lol.cor = cor(lol[,-c(1,ncol(lol))])

# drop.blue.features = c(1)

drop.red.features = c('redFirstBlood', 'redKills', 'redDeaths', 'redAssists',
                      'redDragons', 'redTotalGold', 'redAvgLevel', 'redTotalExperience',
                      'redGoldDiff', 'redExperienceDiff', 'redGoldPerMin')

### dropped variables from example
# repeated = ['redfirstblood', redkills', 'reddeaths', 'redgolddiff','redfirstblood',
#             'redexperiencediff', 'redcspermin', 'redgoldpermin', 'redheralds',
#             'blueavglevel', 'bluecspermin', 'bluegoldpermin']

# modify classification column
lol$blueWins[lol$blueWins == 1] <- "Blue"
lol$blueWins[lol$blueWins == 0] <- "Red"
lol$blueWins = factor(lol$blueWins)

# random split to training and test set
train.index = createDataPartition(lol$blueWins, p = 0.6, list = FALSE)
train = lol[train.index,]
test = lol[-train.index,]

#################
# decision tree #
#################

# cross validation to select best tuning parameter alpha (cost complexity pruning)
fitcontrol.dt = trainControl(method = "repeatedcv", number = 5, repeats = 5)

set.seed(1)
# train and prune decision tree
lol.dt = train(train[,-ncol(lol)], train[,ncol(lol)], method = "rpart",
               metric="Accuracy", tuneLength=5, trControl = fitcontrol.dt) # >>> rpart = alpha

# summary of decision tree
lol.dt

# performance measure
pred.dt = predict(lol.dt, newdata = test[,-ncol(test)])
mean(pred.dt == test[,ncol(test)])

#To look at the details of this tree
print(lol.dt$finalModel)

# get fancy trees by rattle
fancyRpartPlot(lol.dt$finalModel)

#################
# random forest #
#################

# cross validation to select m features to randomly sample
fitcontrol.rf = trainControl(method = "repeatedcv", number = 5, repeats = 5)

set.seed(1)
# train random forest
lol.rf = train(train[,-ncol(lol)], train[,ncol(lol)], method = "rf",
               metric="Accuracy", tuneLength=5, trControl = fitcontrol.rf) # >>> rpart = alpha

# mtryGrid=expand.grid(mtry=c(2,3,4,5,6))
# lol.rf = train(train[,-ncol(lol)], train[,ncol(lol)], method = "rf",
#                metric="Accuracy", tuneGrid=mtryGrid, trControl = fitcontrol.rf)

# summary of decision tree
lol.rf

# performance measure
pred.rf = predict(lol.rf, newdata = test[,-ncol(test)])
mean(pred.rf == test[,ncol(test)])

#To look at the details of this tree
print(lol.rf$finalModel)

# variable importance
varImp(lol.rf)
plot(varImp(lol.rf))