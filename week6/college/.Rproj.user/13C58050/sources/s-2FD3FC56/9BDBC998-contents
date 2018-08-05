library(shiny)
library(ggplot2)


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


function(input, output, session) {
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
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    延畢人數    <- faithful[, 2] 
    a    <- input$大考中心學類編號
    b    <- input$延畢人數
    print(a)
    print(b)
    學類編號 <- seq(min(延畢人數), max(延畢人數), length.out = input$大考中心學類編號 + 1)
    
    # draw the histogram with the specified number of bins
    hist(延畢人數, breaks = 學類編號, col = 'darkgray', border = 'white')
  })
  
}

