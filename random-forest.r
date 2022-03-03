# import libraries
library(caret)
library(tree)
library(randomForest)
library(dplyr)
library(corrplot)
# library(rattle)

source("custom-functions.r")

# read data
lol.ori = read.csv("high_diamond_ranked_10min.csv", header = TRUE)

# preprocess data
set.seed(100)
lol = preprocess.data(lol.ori)

# train test split
set.seed(100)
train.test = train.test.split(lol, 0.6)
train = train.test[[1]]
test = train.test[[2]]

# tuned random forest
set.seed(100)
rf.tuned.output = train.rf.basic(200, train, test)

rf.tuned = rf.tuned.output[[1]]
pred.train.acc = rf.tuned.output[[2]]
pred.test.acc = rf.tuned.output[[3]]

plot(varImp(rf.tuned))

user.input = data.frame(blueFirstBlood = c(0),
                        blueEliteMonsters = c(0),
                        blueTotalJungleMinionsKilled = c(36),
                        blueGoldDiff = c(1550),
                        blueCSPerMin = c(21.8),
                        redHeralds = c(1),
                        redTotalJungleMinionsKilled = c(52))

a = predict(rf.tuned, newdata=test[,-ncol(test)][1,])
b = predict(rf.tuned, user.input)
a == b