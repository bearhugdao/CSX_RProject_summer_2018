library(shiny)
library(ggplot2)

library(markdown)


dta <- read.csv("data/college.csv", header = TRUE)
choice.type <-
  c('學校類型', '設立別', '學類編碼', '學類所屬學群名稱','縣市名稱','體系別')

choice.value <-
  c(
    '學生數',
    '教師數',
    '師生比',
    '人數下滑率',
    '延畢率',
    '專任助理教授以上之比例',
    '正副教授比較助理教授及講師講課時數之倍數',
    '專業學分比',
    '總休學率',
    '退學率',
    '三年後就業薪資_100學年日間學士',
    '三年後就業率_100學年日間學士',
    '隔屆就業率成長幅度',
    '隔屆就業薪資變化',
    '隔年就業率成長幅度',
    '隔年就業薪資變化'
  )


navbarPage(
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
    "交叉比較.",
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
  
  tabPanel(
    "分布圖比較.", 
    titlePanel("看看感興趣的資料分佈"),
    
    numericInput('大考中心學類編號', '大考中心學群編號', 3, min = 1, max = 9),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        sliderInput("大考中心學類編號",
                    "大考中心學類編號",
                    min = 1,
                    max = 123,
                    value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot"),
        plotOutput("distPlot2")
        
      )
    )),
  
  
  navbarMenu("More",
             plotOutput("plot"))
)