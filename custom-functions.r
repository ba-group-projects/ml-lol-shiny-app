train.dt.basic<-function(data, label.var){
  model = tree(label.var ~ ., data)
  
  return(model)
}

train.dt.pruned<-function(input){
  return(input * 1)
}

train.rf.basic<-function(data, label.var, ntree, seed){
  set.seed(seed)
  model = randomForest(label.var ~ ., data, mtry = sqrt(ncol(data)-1),
                       importance=TRUE,ntree=500)
  return(model)
}

train.rf.tuned<-function(input){
  return(input * 1)
}