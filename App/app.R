#######################
### IMPORT PACKAGES ###
#######################
library(shinydashboard)
library(shinyjs)
library(caret)
library(tree)
library(randomForest)
library(dplyr)
library(rattle)
library(corrplot)
library(rpart)
library(rpart.plot)
library(stringr)

########################
### CUSTOM FUNCTIONS ###
########################
# split dataset into train and test
train.test.split <- function(data, p) {
  set.seed(100)
  train.index <- createDataPartition(data$blueWins, p = p, list = FALSE)
  train <- data[train.index, ]
  test <- data[-train.index, ]

  return(list(train, test))
}

# train decision tree
createTree <- function(train_data, min_split = NULL, min_bucket = NULL, max_depth = NULL, optimise = TRUE) {
  # according to users' input, set the parameters and create a decision tree
  if (optimise == TRUE) {
    set.seed(100)
    tree <- rpart(
      "blueWins ~ .",
      method = "class", # sets it up as a classification problem
      data = train_data,
      parms = list(split = "gini") # set the split criterion
    )
  } else {
    set.seed(100)
    tree <- rpart(
      "blueWins ~ .",
      method = "class", # sets it up as a classification problem
      data = train_data,
      parms = list(split = "gini"), # set the split criterion
      minsplit = min_split,
      minbucket = min_bucket,
      maxdepth = max_depth,
      cp = 0 # complexity parameter, at zero prevents pruning on branches
    )
  }

  return(tree)
}

# assess trained decision tree on test data
useTree <- function(tree, data) {
  # Takes a tree generated by rpart and a filename (string) as input and
  # then predicts the labels of the data in that file using the tree. It
  # returns a dataframe with two bool (0,1) columns: prediction and truth.
  prediction <- predict(tree, data, type = "class")
  results <- as.data.frame(prediction)
  results$truth <- data$blueWins

  return(results)
}

# extract important features from decision tree
get.dt.features <- function(tree) {
  # We will give our users the option to modify the features
  # The features we give them depends on the tree model they have generated
  # This function will return a list of features that the user can modify
  rpart.rules <- data.frame(labels(tree))[1][-1, ]
  imp.features <- c()
  for (feature in str_extract_all(rpart.rules, "\\w*")) {
    imp.features <- rbind(imp.features, feature[1])
  }
  output <- unique(imp.features)
  return(paste(output))
}

# train random forest
createRf <- function(train, ntree) {
  # Create a random forest according to users' training data and number of trees
  fitcontrol.rf <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
  set.seed(100)
  rf <- train(train[, -ncol(train)], train[, ncol(train)],
    method = "rf",
    metric = "Accuracy", tuneLength = 5,
    trControl = fitcontrol.rf, ntree = ntree
  )
  return(rf)
}

# assess trained random forest on test data
useRf <- function(rf, data) {
  # Takes a tree generated by rpart and a filename (string) as input and
  # then predicts the labels of the data in that file using the tree. It
  # returns a dataframe with two bool (0,1) columns: prediction and truth.
  prediction <- predict(rf, data)
  results <- as.data.frame(prediction)
  results$truth <- data$blueWins
  return(results)
}

# compute performance metrics for assessing model on test data
calcScores <- function(results) {
  # Takes a results dataframe as input and then calculates scores for
  # accuracy, true negative rate, and true positive rate. It returns a
  # list of formatted strings detailing these results.
  results <- table(results)
  # calculate the scores on which we'll judge our model to 2 decimal places
  accuracy <- round(100 * (results[1] + results[4]) / sum(results), 2)
  true_neg <- round(100 * results[1] / sum(results[1, ]), 2)
  true_pos <- round(100 * results[4] / sum(results[2, ]), 2)
  # the collapse argument removes the spacing which would otherwise be there
  return(list(
    paste(c("Overall Accuracy: ", accuracy, "%"), collapse = ""),
    paste(c("True Positive Rate: ", true_pos, "%"), collapse = ""),
    paste(c("True Negative Rate: ", true_neg, "%"), collapse = "")
  ))
}

# construct confusion matrix
resultsTable <- function(results) {
  # Takes a results dataframe as input and then reconstructs and returns
  # a dataframe which has a similar layout to the command line interface
  # output of R's table(...) function.
  data <- table(results)
  Outcomes <- c("Predicted Blue Win", "Predicted Red Win", "Total")
  # reconstruct the columns of R's table(...) CLI display
  c1 <- c(data[, 1], sum(data[, 1])) # data[, 1] is a length 2 vector
  c2 <- c(data[, 2], sum(data[, 1])) # data[, 2] is a length 2 vector
  c3 <- c(sum(data[, 1]), sum(data[2, ]), sum(data))
  # turn these columns back into a dataframe but with proper headers
  output <- data.frame(Outcomes)
  output$"Actually Blue Win" <- c1
  output$"Actually Red Win" <- c2
  output$"Total" <- c3
  return(output)
}

# make predictions using trained classification model
predict.winner <- function(model, user.input, model.type) {
  # Takes a model and a user input (a dataframe) and then predicts the
  # winner of the game. It returns a string of the predicted winner.
  if (model.type == "dt") {
    prediction <- predict(model, user.input)
    prediction <- colnames(data.frame(prediction))[max.col(data.frame(prediction))]
  } else {
    prediction <- paste(predict(model, user.input), "Team Wins!", sep = " ")
  }
  return(prediction)
}

##################################
### IMPORT AND PREPROCESS DATA ###
##################################

# read data
lol.ori <- read.csv("high_diamond_ranked_10min.csv", header = TRUE)

# summary of data
summary(lol.ori)

# pre-process data
sum(is.na(lol.ori)) # check missing values
lol.ori <- lol.ori[, -1]
set.seed(100) # set the seed for random number generator
lol.blue <- lol.ori[sample(which(lol.ori$blueWins == 1, ), 240), ]
lol.red <- lol.ori[sample(which(lol.ori$blueWins == 0, ), 240), ]
lol.sample <- rbind(lol.blue, lol.red)

# feature engineering
par(mfrow = c(1, 1))
blue.features <- lol.sample[, c(2:20)]

# plot correlation matrix
corrplot(cor(blue.features), tl.col = "black", diag = FALSE)
drop.blue.features <- c(
  "blueDragons", "blueHeralds", "blueKills",
  "blueDeaths", "blueAssists", "blueTotalGold",
  "blueAvgLevel", "blueTotalExperience", "blueExperienceDiff",
  "blueGoldPerMin", "blueTotalMinionsKilled"
)
blue.features <- blue.features[, !(colnames(blue.features) %in% drop.blue.features)]

red.features <- lol.sample[, -c(1, 2:20)]

# plot correlation matrix
corrplot(cor(blue.features, red.features), tl.col = "black", diag = TRUE)
drop.red.features <- c(
  "redFirstBlood", "redKills", "redDeaths", "redAssists",
  "redEliteMonsters", "redDragons", "redTotalGold",
  "redAvgLevel", "redTotalExperience", "redTotalMinionsKilled",
  "redGoldDiff", "redExperienceDiff", "redCSPerMin", "redGoldPerMin"
)
red.features <- red.features[, !(colnames(red.features) %in% drop.red.features)]
blueWins <- lol.sample$blueWins

lol <- cbind(blue.features, red.features, blueWins)

# plot correlation matrix
corrplot(cor(lol), tl.col = "black", diag = FALSE)
drop.more.features <- c(
  "blueWardsPlaced", "blueWardsDestroyed", "blueTowersDestroyed",
  "redWardsPlaced", "redWardsDestroyed", "redTowersDestroyed"
)

# dataset after dropping features
lol <- lol[, !(colnames(lol) %in% drop.more.features)]

# modify classification column
lol$blueWins[lol$blueWins == 1] <- "Blue"
lol$blueWins[lol$blueWins == 0] <- "Red"
lol$blueWins <- factor(lol$blueWins)

set.seed(100)
# random split to training and test set
train.index <- createDataPartition(lol$blueWins, p = 0.7, list = FALSE)
train <- lol[train.index, ]
test <- lol[-train.index, ]

###########################
### SHINY UI COMPONENTS ###
###########################

# header component
headerbar <- dashboardHeader(
  title = span(img(src = "logo.png", height = 35)),
  # custom CSS to change the navbar color
  tags$li(class = "dropdown", tags$style(".skin-blue .main-header .navbar {background-color: #111111;}")) 
)

# sidebar component
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction",
      tabName = "introduction", icon = icon("dashboard"),
      menuItem("Classification model", tabName = "classificationModel"),
      menuItem("Dataset", tabName = "dataset")
    ),
    menuItem("Decision Tree",
      tabName = "decisionTree", icon = icon("tree"),
      menuItem("Train", tabName = "decisionTreeTrain"),
      menuItem("Predict", tabName = "decisionTreePredict")
    ),
    menuItem("Random Forest",
      tabName = "randomForest", icon = icon("cubes"),
      menuItem("Train", tabName = "randomForestTrain"),
      menuItem("Predict", tabName = "randomForestPredict")
    ),
    menuItem("FAQ",
      tabName = "faq", icon = icon("question-circle")
    )
  )
)

############################
### SHINY BODY COMPONENT ###
############################
dashboardContent <-
  dashboardBody(
    tabItems(
      # customize the html for the introduction tab of model classifications
      tabItem(tabName = "classificationModel", fluidRow(fluidRow(HTML('
        <!DOCTYPE html>
          <html>
          <div style="margin-top:-50px">
              <img src="forest.png" style="max-height:100px;overflow:hidden;width:100%">
          </div>
          <div style="margin-left:10%;margin-right:15%;margin-top:4%;background:white;padding:40px ">
              <h2>
                  Introduction
              </h2>
              <br>
              <p style="font-size:16px">
              <p style="font-size:16px">The aim of this app is to explain how decision trees and random forests are applied in
                  classification problems. We will illustrate this through classifying the winner of a match in League of Legends, 
                  a popular Multiplayer Online Battle Arena (MOBA) video game. The app allows users to train their own decision trees
                  and random forests, assess the performance of their trained models, and predict the winner of a match.
              </p>
              <br>
              <h3>What is a Decision Tree?</h3>
              <br>
              <br>
              <p style="font-size:16px">Decision trees perform a classification on data in a hierarchical structure by observing
                  the key differentiating features on the classification data. The path is segregated into binary “yes”, “no”
                  decisions at each level leading to another question and ultimately the result. Each question is regarded as a
                  node, with the split becoming “branches” and the “leaves” representing the end of each point. </p>
              <br>
              <br>
              <iframe width="560" height="315" src="https://www.youtube.com/embed/7VeUPuFGJHk" title="YouTube video player"
                  frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
                  allowfullscreen></iframe>
              <br>
              <br>
              <h3>What is a Random Forest?</h3>
              <br>
              <br>
              <img src="randomforest.gif" width=500>
              <br>
              <br>
              <p style="font-size:16px">
                  Random forest is a modified approach to bagging (bootstrap sampling that averages the aggregate of individual
                  models), in which an algorithm inspects a random subset of features in the data at each split in the learning
                  process, rather than all features seen in bagging. This is done to avoid correlation between the trees. Using
                  multiple bootstrapped samples of the original dataset reduces variance, resulting in lower overfitting.
              </p>
              <br>
              <br>

              <iframe width="560" height="315" src="https://www.youtube.com/embed/J4Wdy0Wc_xQ" title="YouTube video player"
                  frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
                  allowfullscreen></iframe>
              <div align="center">
              </div>
              <br>
              </br>
          </div>

          </html>
        ')))
      ),

      # customize the html for the introduction of the dataset
      tabItem(tabName = "dataset", fluidRow(fluidRow(HTML('
        <!DOCTYPE html>
          <html>
          <div style="margin-top:-50px">
              <img src="lolbanner.png" style="max-height:100px;overflow:hidden;width:100%">
          </div>
          <div style="margin-left:10%;margin-right:15%;margin-top:4%;background:white;padding:20px ">
              <h2>
                  Dataset
              </h2>
              <br>
              <p style="font-size:16px">
              <p style="font-size:16px">
              The dataset comes from the first 10 minutes of 10,000 ranked games from League of Legends where 2 teams (blue and red) play against each other. 
              Players in these ranked games are roughly the same level. The dataset contains 38 features 
              (19 per team) containing information relating to score per minute (SPM), enemies killed, and rank.
              For this app, the initial 37 predictors were reduced to 7 and the classification will be if the 
              blue team wins.
              </p>
              <br>
                The 7 ultimate features, and their definitions, are below:
              <br>
              <p>
                <p style="font-size:16px">
                  <b>blueGoldDiff:</b> &nbsp;&nbsp;&nbsp;&nbsp;  Blue team gold difference compared to the enemy team
                </p>
                
                <p style="font-size:16px">
                  <b>blueCSPerMin:</b> &nbsp;&nbsp;&nbsp;&nbsp;  Blue team CS (minions) per minute
                </p>
                <p style="font-size:16px">
                  <b>blueTotalJungleMinionsKilled:</b> &nbsp;&nbsp;&nbsp;&nbsp;  Blue team total jungle monsters killed
                <p style="font-size:16px">
                  <b>redTotalJungleMinionsKilled:</b> &nbsp;&nbsp;&nbsp;&nbsp;  Red team total jungle monsters killed
                </p>
                <p style="font-size:16px">
                  <b>blueEliteMonsters:</b> &nbsp;&nbsp;&nbsp;&nbsp;  Number of elite monsters killed by the blue team (Dragons and Heralds)
                </p>
                <p style="font-size:16px">
                  <b>blueFirstBlood:</b> &nbsp;&nbsp;&nbsp;&nbsp;   First kill of the game. 1 if the blue team did the first kill, 0 otherwise
                </p>
                <p style="font-size:16px">
                  <b>redHeralds:</b> &nbsp;&nbsp;&nbsp;&nbsp;  Number of heralds killed by the red team
                </p>
              </p>

              </br>
              <iframe width="560" height="315" src="https://www.youtube.com/embed/0uyLRPmmYPk" title="YouTube video player"
                  frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
                  allowfullscreen></iframe>
              <br>
              <br>
              <br>
              <br>
              <br>
          </div>
          </html>
        ')))
      ),

      # customize the page for the decision tree tab
      tabItem(
        tabName = "decisionTreeTrain",
        fluidRow(
          box(
            fluidRow(
              # plot trained decision tree
              plotOutput("decisionTreeTrainPlot", height = 600)
            ),
            fluidRow(
              height = 400,
              # evenly split the width of the fluidRow
              column(
                6,
                h3("Training Results"),
                helpText(
                  "Training results measure how good the model was when it ran on the training data
                   set. Using the slider will determine the percentage of the dataset that will be 
                   trained. Positive results reflect the blue team winning. "
                ),
                # training accuracy, true positive, and true negative
                tagAppendAttributes(
                  textOutput("dt_train_scores"),
                  style = "white-space: pre-wrap; font-size: 15px;"
                ),
                br(),
                # training results table matches layout from presentation
                tableOutput("dt_train_table")
              ),
              column(
                6,
                h3("Test Results"),
                helpText(
                  "These are the measures of how good your model was when it was ran on the test data 
                  set. The test data percentage is the difference of the training slider."
                ),
                # test accuracy, true positive, and true negative
                tagAppendAttributes(
                  textOutput("dt_test_scores"),
                  style = "white-space: pre-wrap; font-size: 15px;"
                ),
                br(),
                # training results table matches layout from presentation
                tableOutput("dt_test_table")
              )
            )
          ),
          box(
            # The parameters for training the decision tree
            h3("Decision Tree"),
            helpText(
              "These controls are for setting the hyperparameter values",
              "which partly control the structure of the decision tree.",
              "The default values should create a fairly safe",
              "model but try changing them if you're feeling adventurous."
            ),
            br(),
            helpText(
              ""
            ),
            h4("Pick a Decision Tree"),
            helpText(
              ""
            ),
            radioButtons("custOpt", "", c("Optimized Tree", "Grow your own Tree!"), inline = TRUE, width = "100%"),
            br(),
            actionButton(
              inputId = "trainDtModel",
              label = "Train Model",
              class = "btn-primary",
              style = "color: #fff"
            ),
            br(),
            br(),
            h4("Split Size (%)"),
            helpText(
              "This is the percentage of the dataset that will be used to train the model."
            ),
            sliderInput(
              inputId = "splitDtSize",
              label = "", # label given in outer code
              min = 0, # two is the smallest that could be split
              max = 100, # chosen to not make the models too wild
              value = 70 # defaults to not having an artifical minimum
            ),
            br(),
            conditionalPanel(
              condition = "input.custOpt == 'Grow your own Tree!'",
              h4("Minimum Split"),
              helpText(
              "Specifies the minimum number of samples required to split
               an internal node.If the sample size of the node is smaller than this 
               value, the node will not split any further. This will become the 
               terminal node (leaf) of the tree."),
              sliderInput(
                inputId = "minSplit",
                label = NULL, # label given in outer code
                min = 2, # two is the smallest that could be split
                max = 10, # chosen to not make the models too wild
                value = 3 # defaults to not having an artifical minimum
              ),
              br(),
              h4("Minimum Bucket Size"),
              helpText(
              "The smallest number of samples allowed in a leaf node. The split will 
              not happen if the number of samples in the node is less than the specified 
              minimum bucket size."
              ),
              sliderInput(
                inputId = "minBucket",
                label = NULL, # label given in outer code
                min = 1, # can't have buckets of size zero
                max = 30, # rpart default is minbucket = 3*minsplit
                value = 4 # defaults to not having an artifical minimum
              ),
              br(),
              h4("Maximum Tree Depth"),
              helpText(
                "Control the maximum depth that the decision tree can reach. The deeper the tree, 
                the more splits it can achieve, capturing more information about the dataset."
              ),
              sliderInput(
                inputId = "maxDepth",
                label = NULL, # label given in outer code
                min = 2, # a min of 2 allows for at least one split
                max = 5, # rpart can't do 31+ depth on 32-bit machines
                value = 3 # chosen to not make the default too wild
              )
            )
          )
        )
      ),

      # customize the page for the random forest prediction tab
      tabItem(
        tabName = "decisionTreePredict",
        fluidRow(
          box(
            fluidRow(
              # plot trained decision tree
              plotOutput("decisionTreeTrainPlot_", height = 600)
            ),
            fluidRow(
              column(
                6,
                h3("Decision Tree Performance"),
                helpText(
                  "According to the model that you have trained, the accuracy rate is:"
                ),
                # test accuracy, true positive, and true negative
                tagAppendAttributes(
                  textOutput("dt_test_scores_"),
                  # allow linebreaks between scores, larger font here
                  style = "white-space: pre-wrap; font-size: 15px;"
                ),
                br(),
                # training results table matches layout from presentation
              ),
              column(
                6,
                h3("Prediction"),
                helpText("According to the input data, we predict that"),
                br(),
                conditionalPanel(
                  "output.dtPrediction == 'Blue'",
                  span(img(src = "blueteamwins.png", height = 80, )),
                ),
                conditionalPanel("output.dtPrediction == 'Red'", span(img(src = "redteamwins.png", height = 80))),
                span(textOutput("dtPrediction"), style = "color:#fff")
              )
            )
          ),
          box(
            h3("Predict which team would win!"),
            span(textOutput("impFeatures"), style = "color:#fff"),
            actionButton(
              inputId = "dtModelPredict",
              label = "Predict Winner",
              class = "btn-primary", 
              style = "color: #fff"
            ),
            br(),
            br(),
            # Because we want to change the input of parameters according to the model
            # our users have trained, so we need to dynamically change the UI input according
            # to the model they have trained.
            conditionalPanel(
              condition = "output.impFeatures.indexOf('blueFirstBlood') > -1",
              radioButtons("blueFirstBloodDt", "Blue First Blood", c(0, 1))
            ),
            conditionalPanel(
              condition = "output.impFeatures.indexOf('blueGoldDiff') > -1",
              sliderInput("blueGoldDiffDt", "Blue Gold Diff", -10000, 10000, 0)
            ),
            conditionalPanel(
              condition = "output.impFeatures.indexOf('blueCSPerMin') > -1",
              sliderInput("blueCSPerMinDt", "Blue CS Per Min", 0, 40, 8)
            ),
            conditionalPanel(
              condition = "output.impFeatures.indexOf('blueEliteMonsters') > -1",
              radioButtons("blueEliteMonstersDt", "Blue Elite Monsters", c(0, 1, 2))
            ),
            conditionalPanel(
              condition = "output.impFeatures.indexOf('blueTotalJungleMinionsKilled') > -1",
              sliderInput("blueTotalJungleMinionsKilledDt", "Blue Total Jungle Minions Killed", 0, 150, 50)
            ),
            conditionalPanel(
              condition = "output.impFeatures.indexOf('redTotalJungleMinionsKilled') > -1",
              sliderInput("redTotalJungleMinionsKilledDt", "Red Total Jungle Minions Killed", 0, 150, 50)
            ),
            conditionalPanel(
              condition = "output.impFeatures.indexOf('redHeralds') > -1",
              radioButtons("redHeraldsDt", "Red Heralds", c(0, 1))
            )
          )
        )
      ),

      # customize the page for the random forest training tab
      tabItem(
        tabName = "randomForestTrain",
        fluidRow(
          box(
            fluidRow(
              # plot variable importance from trained random forest
              plotOutput("rfPlot", height = 600)
            ),
            fluidRow(
              height = 400,
              column(
                6,
                h3("Training Results"),
                helpText(
                "Training results measure how good the model was when it ran on the training 
                data set. Using the slider will determine the percentage of the dataset that 
                will be trained. Positive results reflect the blue team winning. "
                ),
                # training accuracy, true positive, and true negative
                tagAppendAttributes(
                  textOutput("rf_train_scores"),
                  style = "white-space: pre-wrap; font-size: 15px;"
                ),
                br(),
                # training results table matches layout from presentation
                tableOutput("rf_train_table")
              ),
              column(
                6,
                h3("Test Results"),
                helpText(
                "These are the measures of how good your model was when it was ran on the test
                data set. The test data percentage is the difference of the training slider."
                ),
                # test accuracy, true positive, and true negative
                tagAppendAttributes(
                  textOutput("rf_test_scores"),
                  # allow linebreaks between scores, larger font here
                  style = "white-space: pre-wrap; font-size: 15px;"
                ),
                br(),
                # training results table matches layout from presentation
                tableOutput("rf_test_table")
              )
            )
          ),
          box(
            h3("Random Forest"),
            helpText(
              "These controls are for setting the hyperparameter values",
              "which partly control the structure of the random forest.",
              "The default values we've put in should create a fairly safe",
              "model but try changing them if you're feeling adventurous."
            ),
            br(),
            actionButton(
              inputId = "trainRfModel",
              label = "Train Model",
              class = "btn-primary",
              style = "color: #fff"
            ),
            br(),
            br(),
            h4("Split Size (%)"),
            helpText(
              "The percentage of the dataset that will be used to train the model."
            ),
            sliderInput(
              inputId = "splitRfSize",
              label = "", # label given in outer code
              min = 0, # two is the smallest that could be split
              max = 100, # chosen to not make the models too wild
              value = 70 # defaults to not having an artifical minimum
            ),
            h4("Number of Trees"),
            sliderInput(
              inputId = "ntree",
              label = NULL, # label given in outer code
              min = 100, # two is the smallest that could be split
              max = 500, # chosen to not make the models too wild
              value = 100, # defaults to not having an artifical minimum
              step = 100
            )
          )
        )
      ),

      # customize the page for the random forest prediction tab
      tabItem(
        tabName = "randomForestPredict",
        fluidRow(
          box(
            fluidRow(
              # plot variable importance from trained random forest
              plotOutput("rfPlot_", height = 600)
            ),
            fluidRow(
              column(
                6,
                h3("Random Forest Performance"),
                helpText(
                  "According to the model that you have trained, the accuracy rate is:"
                ),
                # test accuracy, true positive, and true negative
                tagAppendAttributes(
                  textOutput("rf_test_scores_"),
                  # allow linebreaks between scores, larger font here
                  style = "white-space: pre-wrap; font-size: 15px;"
                ),
                br(),
                # training results table matches layout from presentation
                tableOutput("rf_test_table_")
              ),
              column(
                6,
                h3("Prediction"),
                helpText("According to the input data, we predict that:"),
                br(),
                span(textOutput("rfPrediction"), style = "font-size:20px"),
              )
            )
          ),
          box(
            h3("Predict which team would win!"),
            br(),
            actionButton(
              inputId = "rfModelPredict",
              label = "Predict Winner",
              class = "btn-primary",
              style = "color: #fff"
            ),
            br(),
            br(),
            # components for user inputs
            radioButtons("blueFirstBloodRf", "Blue First Blood", c(0, 1)),
            sliderInput("blueGoldDiffRf", "Blue Gold Diff", -10000, 10000, 0),
            sliderInput("blueCSPerMinRf", "Blue CS Per Min", 0, 40, 8),
            radioButtons("blueEliteMonstersRf", "Blue Elite Monsters", c(0, 1, 2)),
            sliderInput("blueTotalJungleMinionsKilledRf", "Blue Total Jungle Minions Killed", 0, 150, 50),
            sliderInput("redTotalJungleMinionsKilledRf", "Red Total Jungle Minions Killed", 0, 150, 50),
            radioButtons("redHeraldsRf", "Red Heralds", c(0, 1))
          )
        )
      ),

      # customize the page for the FAQ page
      tabItem(tabName = "faq", fluidRow(fluidRow(HTML('
        <!DOCTYPE html>
          <html>
          <div style="margin-top:-50px">
          <img src="tf.png" style="max-height:100px;overflow:hidden;width:100%">
          </div>
          <div style="margin-left:10%;margin-right:15%;margin-top:4%;background:white;padding:20px ">
          <h2>
              FAQ
          </h2>
          <br>
          <h3>
              What is predicted accuracy?
          </h3>
          <br>
          <p style="font-size:16px">
              Accuracy score measures how many labels the model classified correctly out of the total number of predictions.
              If the accuracy score is higher in the training dataset then the model is more likely to be overfitted.
          </p>
          <br>
          <h3>
              What is true positive rate (TPR)?
          </h3>
          <br>
          <p style="font-size:16px">
              The probability that an event of blue team winning is predicted correctly.
          </p>
          <br>
          <h3>
              What is true negative rate (TNR)?
          </h3>
          <br>
          <p style="font-size:16px">
              The probability that an event of blue team losing is predicted correctly.
          </p>
          <br>
          <h3>
              Why can I not adjust all 7 features when using the trained Decision Tree to make predictions? 
          </h4>
          <br>
          <p style="font-size:16px">
              Only features that remain in the trained Decision Tree will be used to make predictions.

          </div>

          </html>')))
      )
    ),

    # customize the whole page css style
    tags$head(tags$style(HTML("
                      /* logo */
                      .skin-blue .main-header .logo {
                                            background-color: #111111;
                                            };
              #         /* whole page */
              #         .box {margin-top: 2px;margin-left: 0px; margin-right: 0px; margin-bottom:2px;padding:-10px};
              # div {padding: 0 !important;}
                              ")))
  )


#########################################
### CONSOLIDATING SHINY UI COMPONENTS ###
#########################################
ui <- dashboardPage(
  headerbar,
  sidebar,
  dashboardContent
)

####################
### SHINY SERVER ###
####################
server <- function(input, output, session) {

  ########################################
  # >>> decision tree server section <<< #
  ########################################
  
  # train decision tree when user clicks the button
  decisionTree <- eventReactive(
    eventExpr = input$trainDtModel,
    {
      train.test <- train.test.split(lol, input$splitDtSize / 100)
      train <- train.test[[1]]
      test <- train.test[[2]]

      if (input$custOpt == "Optimized Tree") {
        valueExpr <- createTree(train)
      } else {
        valueExpr <- createTree(train, input$minSplit, input$minBucket, input$maxDepth, optimise = FALSE)
      }
    }
  )

  # plot trained decision tree
  output$decisionTreeTrainPlot_ <- renderPlot(
    rpart.plot(decisionTree(), box.palette = "BuRd", roundint = FALSE)
  )
  output$decisionTreeTrainPlot <- renderPlot(
    rpart.plot(decisionTree(), box.palette = "BuRd", roundint = FALSE)
  )

  # extract important features from decision tree
  output$impFeatures <- renderText(get.dt.features(decisionTree()))

  # calculate peformance of trained decision tree when user clicks the button
  # then output the results as score and table for Shiny UI
  dt_train_results <- eventReactive(
    eventExpr = input$trainDtModel,
    valueExpr = useTree(decisionTree(), train)
  )
  dt_test_results <- eventReactive(
    eventExpr = input$trainDtModel,
    valueExpr = useTree(decisionTree(), test)
  )
  output$dt_train_scores <- renderText(
    paste(calcScores(dt_train_results()), collapse = "\n")
  )
  output$dt_test_scores <- renderText(
    paste(calcScores(dt_test_results()), collapse = "\n")
  )
  output$dt_train_table <- renderTable(
    resultsTable(dt_train_results()),
    align = "lccc", # left-align first column, centre rest
    striped = TRUE
  )
  output$dt_test_table <- renderTable(
    resultsTable(dt_test_results()),
    align = "lccc", # left-align first column, centre rest
    striped = TRUE
  )

  # additional score and table output for decision tree's predict section
  output$dt_test_scores_ <- renderText(
    paste(calcScores(dt_test_results()), collapse = "\n")
  )
  output$dt_test_table_ <- renderTable(
    resultsTable(dt_test_results()),
    align = "lccc", # left-align first column, centre rest
    striped = TRUE
  )

  # make predictions using decision tree trained by user
  # based on custom user inputs
  output$dtPrediction <- eventReactive(
    eventExpr = input$dtModelPredict,
    {
      valueExpr <- predict.winner(decisionTree(),
        data.frame(
          blueFirstBlood = c(as.integer(input$blueFirstBloodDt)),
          blueEliteMonsters = c(as.integer(input$blueEliteMonstersDt)),
          blueTotalJungleMinionsKilled = c(input$blueTotalJungleMinionsKilledDt),
          blueGoldDiff = c(input$blueGoldDiffDt),
          blueCSPerMin = c(input$blueCSPerMinDt),
          redHeralds = c(as.integer(input$redHeraldsDt)),
          redTotalJungleMinionsKilled = c(input$redTotalJungleMinionsKilledDt)
        ),
        model.type = "dt"
      )
    }
  )

  ########################################
  # >>> random forest server section <<< #
  ########################################

  # train random forest when user clicks the button
  randomForest <- eventReactive(
    eventExpr = input$trainRfModel,
    {
      train.test <- train.test.split(lol, input$splitRfSize / 100)
      train <- train.test[[1]]
      test <- train.test[[2]]

      valueExpr <- createRf(train, as.integer(input$ntree))
    }
  )

  # plot variable importance from trained random forest
  output$rfPlot_ <- renderPlot(
    plot(varImp(randomForest()), main = "Random Forest Variable Importance")
  )
  output$rfPlot <- renderPlot(
    plot(varImp(randomForest()), main = "Random Forest Variable Importance")
  )

  # calculate peformance of trained random forest when user clicks the button
  # then output the results as score and table for Shiny UI
  rf_train_results <- eventReactive(
    eventExpr = input$trainRfModel,
    valueExpr = useRf(randomForest(), train)
  )
  rf_test_results <- eventReactive(
    eventExpr = input$trainRfModel,
    valueExpr = useRf(randomForest(), test)
  )
  output$rf_train_scores <- renderText(
    paste(calcScores(rf_train_results()), collapse = "\n")
  )
  output$rf_test_scores <- renderText(
    paste(calcScores(rf_test_results()), collapse = "\n")
  )
  output$rf_train_table <- renderTable(
    resultsTable(rf_train_results()),
    align = "lccc", # left-align first column, centre rest
    striped = TRUE
  )
  output$rf_test_table <- renderTable(
    resultsTable(rf_test_results()),
    align = "lccc", # left-align first column, centre rest
    striped = TRUE
  )

  # make predictions using random forest trained by user
  # based on custom user inputs
  output$rfPrediction <- eventReactive(
    eventExpr = input$rfModelPredict,
    {
      valueExpr <- predict.winner(randomForest(),
        data.frame(
          blueFirstBlood = c(as.integer(input$blueFirstBloodRf)),
          blueEliteMonsters = c(as.integer(input$blueEliteMonstersRf)),
          blueTotalJungleMinionsKilled = c(input$blueTotalJungleMinionsKilledRf),
          blueGoldDiff = c(input$blueGoldDiffRf),
          blueCSPerMin = c(input$blueCSPerMinRf),
          redHeralds = c(as.integer(input$redHeraldsRf)),
          redTotalJungleMinionsKilled = c(input$redTotalJungleMinionsKilledRf)
        ),
        model.type = "rf"
      )
    }
  )

  # additional score and table output for random forest's predict section
  output$rf_test_scores_ <- renderText(
    paste(calcScores(rf_test_results()), collapse = "\n")
  )
  output$rf_test_table_ <- renderTable(
    resultsTable(rf_test_results()),
    align = "lccc", # left-align first column, centre rest
    striped = TRUE
  )
}

shinyApp(ui, server)