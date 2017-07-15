#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
PMT <- function(rate, nper,pv, fv=0, type=0){
        pmt = ifelse(rate!=0,
                     (rate*(fv+pv*(1+ rate)^nper))/((1+rate*type)*(1-(1+ rate)^nper)),
                     (-1*(fv+pv)/nper )
        ) 
        
        return(pmt)
}
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Car Purchase Calculator: Should I buy new or used?"),
   
   # Sidebar with a slider input for number of bins 

   
   sidebarLayout(
      sidebarPanel(
              numericInput("value",
                           "Loan Value ($)",
                           value = 30000,
                           min = 0,
                           max = 1000000,
                           step = 1000,
                           width = 100),
              
              numericInput("term",
                           "Loan Term (months)",
                           value = 60,
                           min = 0,
                           max = 84,
                           step = 12,
                           width = 100),
              
              numericInput("rate",
                          "Interest Rate: (%, Annual)",
                          min = 0,
                          max = 10,
                          step = 0.01,
                          value = 0.99,
                          width = 100),
              
              numericInput("odometer",
                           "Odometer (km)",
                           value = 0,
                           min = 0,
                           max = 500000,
                           step = 10000,
                           width = 100),
              
              numericInput("life",
                           "Lifetime km",
                           value = 200000,
                           min = 100000,
                           max = 500000,
                           step = 10000,
                           width = 100),
              
              numericInput("annual",
                           "Annual km",
                           value = 15000,
                           min = 0,
                           max = 200000,
                           step = 10000,
                           width = 100),    
              
              numericInput("fuelEconomy",
                           "Fuel Economy (L per 100 km)",
                           value = 7,
                           min = 0,
                           max = 25,
                           step = 0.1,
                           width = 100),
              
              numericInput("gasPrice",
                           "Gas ($ per L)",
                           value = 1.31,
                           min = 0,
                           max = 5,
                           step = 0.1,
                           width = 100)
              
      
      ),
      
      mainPanel(
              
              tabsetPanel(
                tabPanel("Summary", tags$p("Monthly Payment:"), 
                                    verbatimTextOutput("MonthlyPay"),
                                    tags$p("Borrowing Cost:"),
                                    verbatimTextOutput("BorrowingCost"),
                                    tags$p("Total Purchase Cost:"),
                                    verbatimTextOutput("TotalCostExFuel"),
                                    tags$p("Total Ownership Cost:"),
                                    verbatimTextOutput("TotalCostInFuel"),
                                    tags$p("Cost per km (inc. fuel):"),
                                    verbatimTextOutput("PerKMCost"),
                                    tags$p("Annual Cost: (inc. fuel)"),
                                    verbatimTextOutput("AnnualCost")
                ),
                tabPanel("Loan Table",  tableOutput("PaymentTable"))
                
                
              )
      )
   )

)
# Define server logic required to draw a histogram
server <- function(input, output) {

   output$MonthlyPay <- renderText({ 
           monthly <- PMT(input$rate/(100*12), input$term, input$value)
   })
  
   calcLoan <- reactive ({
           
           monthly <- PMT(input$rate/(100*12), input$term, input$value)
           payment <- data.frame(matrix(NA, nrow = input$term, ncol = 6))
           colnames(payment) <- c("Month", "Starting balance", "Payment", "Applied to Interest", "Applied to Principal", "Remaining Loan Balance")
           starting_balance <- input$value
           for (i in 1:input$term) {
                   
                   payment[i,1] <- i
                   payment[i,2] <- starting_balance
                   payment[i,3] <- monthly
                   payment[i,4] <- (-input$rate/(100*12)) * payment[i,2]
                   payment[i,5] <- payment[i,3] - payment[i,4]
                   payment[i,6] <- payment[i,2] +  payment[i,5]
                   
                   starting_balance <- payment[i,6]
           }  
        list (payment = payment)           
   })
   
   borrowCost <- reactive ({
    sum(calcLoan()$payment$'Applied to Interest')    
   })
   
   costPer <- reactive ({
           gasCost <- ((input$life-input$odometer) / 100 * input$fuelEconomy * input$gasPrice)  
           totalCost <- (gasCost +  (input$value - borrowCost())) 
           kmCost <- totalCost / (input$life-input$odometer)
           annualCost <- totalCost / (input$life / input$annual)
           
           list (gasCost = gasCost, totalCost = totalCost, kmCost = kmCost, annualCost = annualCost)
   })
   
   output$PaymentTable <- renderTable({
           calcLoan()$payment
   })
   
   
   output$BorrowingCost <- renderText({
         -borrowCost()      
   })
   
   output$TotalCostInFuel <- renderText({
          costPer()$totalCost
   })
   
   output$TotalCostExFuel <- renderText({
           costPer()$totalCost-costPer()$gasCost
   })
   
   output$PerKMCost <- renderText({
           costPer()$kmCost
   })
   
   output$AnnualCost <- renderText({
           costPer()$annualCost
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

