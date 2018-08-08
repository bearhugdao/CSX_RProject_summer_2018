,        
tabPanel("regression",
         
         sidebarLayout(
           sidebarPanel(
             h4("Select Predictor"),
             selectInput("var1", "Predictor:",
                         c("Number.of.students" = "Number.of.students",
                           "Number.of.teachers" = "Number.of.teachers"
                         )),
             
             selectInput("var2", "Predictor2:",
                         c(
                           "Total.Suspension.rate" = "Total.Suspension.rate",
                           "Dropout.rate" = "Dropout.rate"
                         )),
             selectInput("var3", "Predictor3:",
                         c(
                           "Three.year.employment.rate._100.academic.year.bachelor" = "Three.year.employment.rate._100.academic.year.bachelor",
                           "Three.years.after.employment.salary._100.school.year.bachelor" = "Three.years.after.employment.salary._100.school.year.bachelor"
                         ))
           ),
           
           mainPanel(
             
             tabsetPanel(
               tabPanel("Documentation", 
                        h3("Using the application"),
                        hr(),
                        p("The application has a 'Side bar Panel' and two tabs - 'Documentation' and 'Plot'. The 'Side bar panel' has two select boxes - 'Predictor' and 'Color'."),
                        p("The application uses the diamonds data set from the R datasets packages. The below table shows first few rows of the diamonds data"),
                        tableOutput("diamondsData"),
                        p("The 'Plot' tab shows the following things : "),
                        tags$ol(
                          tags$li("A plot of diamond price (in SGD) Vs the 'Predictor' variable selected in the Side Bar Panel. The plot uses the 'Color' variable from the side bar panel to color different data points"),
                          tags$li("Fits a Linear Regression between Price and the Predictor Variable. It also shows the coefficients of the fit"),
                          tags$li("Shows the R Squared value of the fit to show a mesaure of Explained variation to Total variation")
                        ),
                        
                        tags$b("NOTE:"),
                        
                        tags$li("Predictor Variable in Side bar panel has all the continous variables from the diamonds data set. If you change the 'Predictor' variable in the Side bar panel, the plot (Price Vs Predictor) will change and a new linear regression model will be built for Price Vs Predictor"),
                        tags$li("Color Variable in Side bar panel has all the categorical variables from the diamonds data set. If you change the 'Color' variable in the Side bar panel, the color of the points in plot will change based on your selection"),
                        tags$li("Go to Plot tab and start to play with the Predictor and Color. You might have to wait for a few seconds to see the output")
               ),
               tabPanel("Plot", 
                        textOutput("text0"),
                        plotOutput("distPlot"),
                        textOutput("text1"),
                        tableOutput("coeff"),
                        textOutput("text2")
               )
             )
           )
         )
         
         
         
         
         
         
         
         
         
         output$distPlot <- renderPlot({
           data("dta")
           #qplot(x=input$var1,y=input$var3.rate,data=dta);
           ggplot(data = dta,aes_string(x=input$var1, y=input$var3,color=input$var2)) + geom_point() + geom_smooth(method = "lm")
         })
         
         runRegression <- reactive({
           lm(as.formula(paste(paste("~","~",input$var1))),data=diamonds)
         })
         
         output$coeff <- renderTable({
           details<-summary(runRegression())$coefficients
           as.data.frame(details)
         },include.rownames=TRUE)
         
         output$text0 <- renderText({
           paste("Plot of Delay.rate","Vs",input$var1)
         })
         
         output$text1 <- renderText({
           paste("Linear Regression","~",input$var1)
         })
         
         output$text2 <- renderText({
           paste("R Squared Value",":",summary(runRegression())$r.squared)
         })
         
         output$dtaData <- renderTable(head(dta,3))
         
         
         
         
         
         
         ,        
         tabPanel("Examples of DataTables",
                  sidebarLayout(
                    sidebarPanel(
                      conditionalPanel(
                        'input.dataset === "dta"',
                        checkboxGroupInput("show_vars", "Columns:",
                                           names(dta), selected = names(dta))
                      )
                    ),
                    mainPanel(
                      tabsetPanel(
                        id = 'dataset',
                        tabPanel("college", DT::dataTableOutput("mytable1"))
                      )
                    )
                  )
         )
         
         
         
         
         
         dta2 = dta[sample(nrow(dta), 1000), ]
         output$mytable1 <- DT::renderDataTable({
           DT::datatable(dta[, input$show_vars, drop = FALSE])
         })