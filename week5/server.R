library(shiny)
library(ggplot2)

dta <- read.csv(file = "data/college4.csv",
                  header = TRUE)


choice.type <-
  c('types', 
    'Set', 
    'Schoolgroupname', 
    'Schoolgroupnameandnumber', 
    'Countyname', 
    'System',
    'ProfessionalHollandcodefirstcode'
  )
choice.value <-
  c(
    'Teacherstudentratio',
    'Numberofstudents',
    'Numberofdelaypeople',
    'Decliningrate',
    'Delayrate',
    'Hitrate',
    'TotalSuspensionrate',
    'Dropoutrate',
    'Afterthreeyearsofemploymentsalary_100schoolyearbachelor',
    'Afterthreeyearsofemploymentrate_100academicyearbachelor'
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
    
    output$data.raw <- DT::renderDataTable({
        DT::datatable(dta)
    })
    
    output$data.summary <- DT::renderDataTable({
        DT::datatable(summary(dta))
    })
}