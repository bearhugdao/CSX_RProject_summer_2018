library(shiny)
library(ggplot2)

dta <- read.csv(file = '高教資料總表串聯3.csv', encoding = 'UTF-8')


#show出test資料
str(dta)
#418 obs. of  11 variables

head(dta)


choice.type <-
  c('學校類型', '設立別', '學校', '科系')

choice.value <-
  c(
    '大學生興趣碼R',
    '大學生興趣碼I',
    '大學生興趣碼A',
    '大學生興趣碼S',
    '大學生興趣碼E',
    '大學生興趣碼C'
  )

ui <- navbarPage(
  "大學教育資料",
  tabPanel(
    "背景介紹",
    tags$h1("大學相關資料查詢與研究"),
    tags$p("簡單描述")
    #HTML("<img height=600 src=\"https://78.media.tumblr.com/aa70ed84a7cd2e7b83c36118dfb2c0e5/tumblr_p8xzq1U8ik1qhy6c9o1_500.gif\"/>")
  ),
  tabPanel(
    "原始資料",
    tags$h1("分析與呈現的所有資料"),
    br(),
    fluidRow(column(
      8,
      tabPanel("Table",
               DT::dataTableOutput("data.raw"))
    ))
  ),
  
  tabPanel(
    "挑選變項",
    tags$h1("看一看自己感興趣的指標吧"),
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput('SV.input', 'type', c(choice.type, choice.value), selectize = TRUE)
      ),
      mainPanel(plotOutput("SV.plot"))
    ),
    
    tags$h1("摘要"),
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
