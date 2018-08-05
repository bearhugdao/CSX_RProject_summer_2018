library(shiny)
library(ggplot2)

dta <- read.csv(file = "???и????`?????p3.csv", fileEncoding = "big5")


#show?Xtest????
str(dta)
#418 obs. of  11 variables

head(dta)


choice.type <-
  c('?Ǯ?????', '?]?ߧO', '?Ǯ?', '???t')

choice.value <-
  c(
    '?j?ǥͿ????XR',
    '?j?ǥͿ????XI',
    '?j?ǥͿ????XA',
    '?j?ǥͿ????XS',
    '?j?ǥͿ????XE',
    '?j?ǥͿ????XC'
  )

ui <- navbarPage(
  "?j?ǱШ|????",
  tabPanel(
    "?I??????",
    tags$h1("?j?Ǭ??????Ƭd?߻P???s"),
    tags$p("²???y?z")
    #HTML("<img height=600 src=\"https://78.media.tumblr.com/aa70ed84a7cd2e7b83c36118dfb2c0e5/tumblr_p8xzq1U8ik1qhy6c9o1_500.gif\"/>")
  ),
  tabPanel(
    "???l????",
    tags$h1("?��R?P?e?{???Ҧ?????"),
    br(),
    fluidRow(column(
      8,
      tabPanel("Table",
               DT::dataTableOutput("data.raw"))
    ))
  ),
  
  tabPanel(
    "?D???ܶ?",
    tags$h1("?ݤ@?ݦۤv?P???쪺???Чa"),
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput('SV.input', 'type', c(choice.type, choice.value), selectize = TRUE)
      ),
      mainPanel(plotOutput("SV.plot"))
    ),
    
    tags$h1("?K?n"),
    verbatimTextOutput("summary")
    
  ),
  
  tabPanel(
    "PartA.",
    tags$h1("Box Plot"),
    sidebarLayout(
      sidebarPanel(
        selectInput('PA.type', 'type', choice.type, selectize = TRUE),
        selectInput('PA.value', 'Value', choice.value, selectize =
                      TRUE)
      ),
      mainPanel(plotOutput("PA.plot"))
    ),
    h1("T Test / ANOVA"),
    verbatimTextOutput("t.test.anova")
  ),
  
  # tabPanel("Summary"),
  
  navbarMenu("More",
             plotOutput("plot"))
)

server <- function(input, output, session) {
  output$SV.plot <- renderPlot({
    if( is.element(input$SV.input, choice.type) ){
      ggplot(data = dta, aes_string(x = input$SV.input)) +
        geom_bar() +
        labs(y = "count", x = input$SV.input)
    }
    else{
      ggplot(data = dta, aes_string(x = input$SV.input)) +
        geom_histogram() +
        labs(y = "count", x = input$SV.input)
    }
  })
  
  output$PA.plot <- renderPlot({
    ggplot(data = dta, aes_string(x = input$PA.type, y = input$PA.value)) +
      geom_boxplot() + coord_flip() +
      labs(y = input$PA.value, x = input$PA.type)
    
  })
  
  output$summary <- renderPrint({
    summary(dta)
  })
  
  output$t.test.anova <- renderPrint({
    type = unlist(dta[input$PA.type])
    value = unlist(dta[input$PA.value])
    if (length(levels(type)) == 2){
      t.test( value ~ type )
    } else {
      anova( lm(value ~ type) )
    }
  })
  
  output$data.raw <- DT::renderDataTable({
    DT::datatable(dta)
  })
  
  output$data.summary <- DT::renderDataTable({
    DT::datatable(summary(dta))
  })
  
}shinyApp(ui = ui, server = server)
