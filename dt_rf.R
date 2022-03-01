# import libraries
library(caret)
library(dplyr)
library(rattle)

# read data
    # heart=read.csv("Heart.csv",header = TRUE)
lol.ori = read.csv("high_diamond_ranked_10min.csv", header = TRUE)

# summary of data
str(lol.ori)
summary(lol.ori)

# pre-process data
sum(is.na(lol.ori)) # check missing values
last.colname = names(lol.ori)[ncol(lol.ori)] # get header of last column
lol.ori = lol.ori %>% relocate(blueWins, .after = all_of(last.colname)) # reorder classification column
lol.ori$blueWins[lol.ori$blueWins == 1] <- "Blue"
lol.ori$blueWins[lol.ori$blueWins == 0] <- "Red"
lol.ori$blueWins = factor(lol.ori$blueWins)
set.seed(100)
lol.blue = lol.ori[sample(which(lol.ori$blueWins == "Blue",), 240),]
lol.red = lol.ori[sample(which(lol.ori$blueWins == "Red",), 240),]
lol = rbind(lol.blue, lol.red)

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
fitcontrol.rf = trainControl(method = "repeatedcv", number = 5, repeats = 5) # number is fold, repeats is number of times to repeat

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