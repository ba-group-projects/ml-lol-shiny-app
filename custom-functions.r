train.test.split<-function(data, p){
  train.index = createDataPartition(data$blueWins, p = p, list = FALSE)
  train = data[train.index,]
  test = data[-train.index,]
  
  return(list(train,test))
}

preprocess.data<-function(lol.ori){
  lol.ori = lol.ori[,-1]
  
  lol.blue = lol.ori[sample(which(lol.ori$blueWins == 1,), 240),]
  lol.red = lol.ori[sample(which(lol.ori$blueWins == 0,), 240),]
  lol.sample = rbind(lol.blue, lol.red)
  
  blue.features = lol.sample[,c(2:20)]
  drop.blue.features = c('blueDragons', 'blueHeralds', 'blueKills', 
                         'blueDeaths', 'blueAssists', 'blueTotalGold',
                         'blueAvgLevel', 'blueTotalExperience', 
                         'blueExperienceDiff', 'blueGoldPerMin', 
                         'blueTotalMinionsKilled')
  blue.features = blue.features[, !(colnames(blue.features) %in% drop.blue.features)]
  
  red.features = lol.sample[,-c(1, 2:20)]
  drop.red.features = c('redFirstBlood','redKills', 'redDeaths', 'redAssists', 
                        'redEliteMonsters', 'redDragons', 'redTotalGold', 
                        'redAvgLevel', 'redTotalExperience', 
                        'redTotalMinionsKilled', 'redGoldDiff', 
                        'redExperienceDiff', 'redCSPerMin', 'redGoldPerMin')
  red.features = red.features[, !(colnames(red.features) %in% drop.red.features)]
  blueWins = lol.sample$blueWins
  
  lol = cbind(blue.features, red.features, blueWins)
  drop.more.features = c('blueWardsPlaced', 'blueWardsDestroyed', 'blueTowersDestroyed',
                         'redWardsPlaced', 'redWardsDestroyed', 'redTowersDestroyed')
  lol = lol[, !(colnames(lol) %in% drop.more.features)]
  
  # modify classification column
  lol$blueWins[lol$blueWins == 1] <- "Blue"
  lol$blueWins[lol$blueWins == 0] <- "Red"
  lol$blueWins = factor(lol$blueWins)
  
  return(lol)
}

train.dt.basic<-function(data){

  label.var = factor(data$blueWins)
  # set.seed(100)
  model = tree(label.var ~ ., data)
  
  return(model)
}

train.dt.pruned<-function(input){
  return(input * 1)
}

# - Random Forest
#   - train
#     - input
#       - training size
#       - Number of trees
#     - output
#       - Variable importance
#       - Accuracy(trainning,testing)
#   - predict
#     - input
#       - ???
#     - output
#       - prediction
#       - accuracy

train.rf.tuned<-function(ntree, train, test){
  fitcontrol.rf = trainControl(method = "repeatedcv", number = 10, repeats = 2)
  model = train(train[,-ncol(train)], train[,ncol(train)], method = "rf", 
                metric="Accuracy", tuneLength=5,
                trControl = fitcontrol.rf, ntree = ntree)
  
  pred.train = predict(model, newdata = train[,-ncol(train)])
  pred.train.acc = round(mean(pred.train == train[,ncol(train)])*100, 1)
  
  pred.test = predict(model, newdata = test[,-ncol(test)])
  pred.test.acc = round(mean(pred.test == test[,ncol(test)])*100,1)
  
  return(list(model, pred.train.acc, pred.test.acc))
}

predict.rf.tuned<-function(model, user.input){
  prediction = predict(model, user.input)
  return(prediction)
}
