# import libraries
library(tree)
library(caret)

# read data
    # heart=read.csv("Heart.csv",header = TRUE)
lol.ori=read.csv("high_diamond_ranked_10min.csv",header = TRUE)

# summary of data
str(lol.ori)
summary(lol.ori)

# pre-process data
sum(is.na(lol.ori)) # check missing values
set.seed(888)
lol.blue=lol.ori[sample(which(lol.ori$blueWins == 1,),240),]
lol.red=lol.ori[sample(which(lol.ori$blueWins == 0,),240),]
lol = rbind(lol.blue,lol.red)

# random split to training and test set
train.index=createDataPartition(lol$blueWins,p=0.7,list=FALSE)
train=heart[train.index,]
test=heart[-train.index,]

#################
# decision tree #
#################

# cross validation to select best tuning parameter alpha (cost complexity pruning)
fitcontrol=trainControl(method = "repeatedcv", number = 10, repeats = 10)

set.seed(5)
# train and prune decision tree
heart.rpart=train(train[,-ncol(heart)],train[,ncol(heart)], 
                  method = "rpart", tuneLength=10,
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