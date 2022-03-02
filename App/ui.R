library(shinydashboard)
# library(shinyjs)

# jsToggleFS <- 'shinyjs.toggleFullScreen = function() {
#     var element = document.documentElement,
#       enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
#       exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
#     if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
#       enterFS.call(element);
#     } else {
#       exitFS.call(document);
#     }
#   }'


# header

headerbar <- dashboardHeader(
  title = div(img(src = "download.png", height = 40), "LOL Diamond Rank Analytics", style = "background-color: #111111;display:inline-block "), # TODO solve broken picture
  titleWidth = 300,
  tags$li(class = "dropdown", tags$style(".skin-blue .main-header .navbar {background-color: #111111;}"))
)

# sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
    # menuItem("Model Selection",
    #   icon = icon("tree"), tabName = "modelSelection",
    
    menuItem("Decision Tree", tabName = "decisionTree",icon = icon('tree'),
               menuItem("Train", tabName = "decisionTreeTrain"),
               menuItem("Predict", tabName = "decisionTreePredict")),
    
    menuItem("Random Forest", tabName = "randomForest", icon= icon('cubes'),
               menuItem("Train", tabName = "randomForestTrain"),
               menuItem("Predict", tabName = "randomForestPredict"))
    # menuItem("Prediction", tabName = "prediction", icon = icon("clock")),
    # menuItem("Lexicon", tabName = "lexicon", icon = icon("table")),
    # menuItem("Introduction Video", tabName = "introductionVideo", icon = icon("video"))
  )
)


# dashboardBody
dashboardContent <-
  dashboardBody(
    # # Boxes need to be put in a row (or column)
    # fluidRow(
    #   box(plotOutput("plot1", height = 600)),

    #   box(
    #     title = "Controls",
    #     sliderInput("slider", "Number of observations:", 1, 100, 50)
    #   )
    # ),
    tabItems(
      tabItem(tabName = "overview"),
      tabItem(tabName = "modelSelection"),
      tabItem(tabName = "decisionTree",
      fluidRow(
        box(),
        box(
      h3("Decision Tree"),
            helpText(
                "These controls are for setting the hyperparameter values",
                "which partly control the structure of the decision tree.",
                "The default values we've put in should create a fairly safe",
                "tree but try changing them if you're feeling adventurous."
            ),
            br(),
            h4("Minimum Split"),
            helpText(
                "If at a given node N is below this value, that node cannot",
                "be split any further: it is a terminal node of the tree."
            ),
            sliderInput(
                inputId = "min_split",
                label = NULL,  # label given in outer code
                min = 2,       # two is the smallest that could be split
                max = 10,      # chosen to not make the models too wild
                value = 2      # defaults to not having an artifical minimum
            ),
            br(),
            h4("Minimum Bucket Size"),
            helpText(
                "If creating a given split would cause N₁ or N₂ to fall below",
                "this minimum, then that split isn't made part of the",
                "decision tree."
            ),
            sliderInput(
                inputId = "min_bucket",
                label = NULL,  # label given in outer code
                min = 1,       # can't have buckets of size zero
                max = 30,      # rpart default is minbucket = 3*minsplit
                value = 1      # defaults to not having an artifical minimum
            ),
            br(),
            h4("Maximum Tree Depth"),
            helpText(
                "Control the maximum depth that the decision tree can reach.",
                "Note that, depending on what features are being used and the",
                "values of the other parameters, you may end up with a tree",
                "much shallower than the maximum."
            ),
            sliderInput(
                inputId = "max_depth",
                label = NULL,  # label given in outer code
                min = 2,       # a min of 2 allows for at least one split
                max = 30,      # rpart can't do 31+ depth on 32-bit machines
                value = 5      # chosen to not make the default too wild
            ))
        )), # 
      tabItem(tabName = "randomForest",
      fluidRow(
        box(plotOutput("plot1", height = 600))
      )), # TODO number of trees; randomize
      ##############################
      # prediction interaction part
      ##############################
      tabItem(
        tabName = "prediction",
        fluidRow(
          box(plotOutput("plot1", height = 250)),
          box(
            title = "Controls",
            radioButtons("firstBlood", "First Blood",c("Blue", "Red")),
            radioButtons('herald', 'Herald', c('Blue', 'Red')),
            sliderInput("blueWardsPlaced", "Blue wards placed", 0, 60, 20),
            sliderInput("blueWardsDestroyed", "Red wards placed", 0, 120, 20),
            sliderInput("blueELiteMonsters", "Blue Elite Monsters", 0, 2, 1),
            sliderInput("blueTowersDestroyed", "Blue Towers Destroyed", 0, 1, 1),
            sliderInput('blueTotalJungleMinionsKilled', 'Blue Total Jungle Minions Killed', 0, 80, 20),
            sliderInput('blueTotalGold', 'Blue Total Gold', -10000, 10000, 0),
            sliderInput('blueMinionKillsPerMin', 'Blue Minion Kills Per Min', 10, 30, 20),
            sliderInput('redWardsPlaced', 'Red wards placed', 0, 60, 20),
            sliderInput('redWardsPlaced', 'Red wards placed', 0, 60, 20),
            sliderInput('redTowersDestroyed', 'Red Towers Destroyed', 0, 1, 1),
            sliderInput('redTotalJungleMinionsKilled', 'Red Total Jungle Minions Killed', 0, 80, 20),
            sliderInput('redTotalGold', 'Red Total Gold', -10000, 10000, 0)
            ),
           

          )
        ),
      tabItem(
        tabName = "lexicon",
        HTML('<!DOCTYPE html>
<html>
<head>
  <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
  <script type="application/shiny-singletons"></script>
  <script type="application/html-dependencies">jquery[3.6.0];shiny-css[1.7.1];shiny-javascript[1.7.1]</script>
  <head>
    <link href="shared/shiny.min.css" rel="stylesheet" />
    <script src="shared/shiny.min.js"></script>
    </head>
    <body>
        <h1>Lexicon</h1>
        <table>
        <tr>
            <th>Name</th>
            <th>Explanation</th>
          </tr>
          <tr>
            <td>Alfreds Futterkiste</td>
            <td>Maria Anders</td>
          </tr>
        </table>
        </body>>
</html>')
      ),
      tabItem(
        tabName = "introductionVideo",
        HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/0uyLRPmmYPk" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
      )
    ),
    tags$head(tags$style(HTML("
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #111111;
                              };
        /* whole page */
        .box {margin-top: 2px;margin-left: 0px; margin-right: 0px; margin-bottom:2px;padding:-10px};
div {padding: 0 !important;}
                              ")))
  )


# whole Ui
ui <- dashboardPage(
  # useShinyjs(),
  # extendShinyjs(text = jsToggleFS),
  headerbar,
  sidebar,
  dashboardContent
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)