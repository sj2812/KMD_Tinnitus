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
            selectInput('clust', 'CLustering Algorithm',choices = list("kmeans" = 1,"Heirarchichal" = 2,"Hkmeans" = 3),selected = 1),
            selectInput('visual','Type of visualization',choices = list("Cluster" = 1,"Decision Tree" = 2,"Radial Chart" = 3)),
            selectInput('clusters', 'Cluster count',choices = list("2" = 2,"4" = 4),selected = 1),
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
    output$value <- renderPrint({ input$select })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
