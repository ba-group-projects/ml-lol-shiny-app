# import libraries
library(tree)
library(caret)

# read data
heart=read.csv("Heart.csv",header = TRUE)

# pre-process data
sum(is.na(heart)) #check missing values
heart=heart[complete.cases(heart),]
heart$AHD=factor(heart$AHD)
heart$ChestPain=factor(heart$ChestPain)
heart$Thal=factor(heart$Thal)

# random split to training and test set
set.seed(888)
train.index=createDataPartition(heart$AHD,p=0.7,list=FALSE)
train=heart[train.index,]
test=heart[-train.index,]

#################
# decision tree #
#################

# cross validation to select best tuning parameter alpha (cost complexity pruning)
fitcontrol=trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(5)s
# train and prune decision tree
heart.rpart=train(train[,-ncol(heart)],train[,ncol(heart)], 
                  method = "rpart", tuneLength=5,
                  trControl = fitcontrol) # >>> rpart = alpha

# summary of decision tree
heart.rpart

# performance measure
pred.heart.rpart=predict(heart.rpart,newdata=test[,-ncol(test)])
mean(pred.heart.rpart==test[,ncol(test)])

# #To look at the details of this tree 
# print(heart.rpart$finalModel)
# # plot the tree
# plot(heart.rpart$finalModel) 
# text(heart.rpart$finalModel,pretty=1)
# #### get fancy trees by rattle
# library(rattle)
# fancyRpartPlot(heart.rpart$finalModel)

#################
# random forest #
#################

# cross validation to select m features to randomly sample

# train random forest