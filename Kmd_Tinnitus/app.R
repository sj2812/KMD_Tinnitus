#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    pageWithSidebar(
        headerPanel('Tinnitus Analysis'),
        sidebarPanel(
            selectInput('Algorithm', 'CLustering Algorithm',
                        choices = list("Select an algorithm" = 0,"kmeans" = 1,"Heirarchichal" = 2,"Hkmeans" = 3,"PC kmeans" = 4,"Proclus" = 5,"Orclus" = 6),selected = 0),
            selectInput('visual','Type of visualization',choices = list("Select" = 0,"Decision Tree" = 1,"Radial Chart" = 2),selected = 0),
            numericInput('clusters', 'Cluster count',3,min = 2,max = 9),
            actionButton("update", "Show"),
            hr(),
            fluidRow(column(3, verbatimTextOutput("value")))
        ),
        mainPanel(
            plotOutput('plot1')
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  condition <- reactive(input$visual)
  alg <- reactive(input$Algorithm)
  
  selectedData <- reactive({
    clust(input$clusters)
  })
  
  view <- reactive(if( condition() == 1 ) 
                    DT(selectedData)
                  else 
                    radial(selectedData)
                    )
  

  
  clust <- reactive(
    if(alg() == "kmeans")
      
    )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
