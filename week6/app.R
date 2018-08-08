library(shiny)
library(ggplot2)
library(markdown)
library(shinyWidgets)


dta <- read.csv("college4.csv",
                header = TRUE)

ui <- navbarPage(
  title = 'DataTable Options',
  tabPanel('Display length',  
    column(4,
           selectInput("types",
                       "types:",
                       c("All",
                         unique(as.character(dta$types))))
    ),
    
    column(4,
           selectInput("types",
                       "types:",
                       c("All",
                         unique(as.character(dta$types))))
    ),
  # Create a new row for the table.
    DT::dataTableOutput("table")
  )
)

server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- dta
    if (input$types != "All") {
      data <- data[data$types == input$types,]
    }
    data
  }))
  
}

shinyApp(ui = ui, server = server)