library(shinydashboard)



# header

headerbar <-dashboardHeader(
  title = div(img(src = "download.png", height = 40), "LOL Diamond Rank Analytics", style = "background-color: #111111;display:inline-block "), # TODO solve broken picture
  titleWidth = 300,
  tags$li(class = "dropdown",tags$style(".skin-blue .main-header .navbar {background-color: #111111;}"))
)

# sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
    menuItem("Model Selection", icon = icon("tree"), tabName = "modelSelection",
             menuItem('    Decision Tree',tabName = "decisionTree"),
             menuItem('    Random Forest',tabName = "randomForest"),
             menuItem('    Random Forest',tabName = "random/Forest")
             
    ),
    menuItem("Prediction", tabName = "prediction", icon = icon("clock")),
    menuItem('Lexicon', tabName = "lexicon", icon = icon("table")),
    menuItem('Introduction Video', tabName = "introductionVideo", icon = icon("video"))
    
  )
)


# dashboardBody
dashboardContent = 
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
      tabItem(tabName = 'overview'),
      tabItem(tabName = 'modelSelection'),
      tabItem(tabName = 'decisionTree'),
      tabItem(tabName = 'randomForest'),
      tabItem(tabName = 'prediction',
      fluidRow(
          box(plotOutput("plot1", height = 250)),

          box(
            title = "Controls",
            sliderInput("slider", "Number of observations:", 1, 100, 50)
          )
        )),
      tabItem(tabName = 'lexicon',
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
      tabItem(tabName = 'introductionVideo',
             HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/0uyLRPmmYPk" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
      )
      
      ),
    
    
     tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #111111;
                              }
                              ')))
  )


# whole Ui
ui <- dashboardPage(
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
