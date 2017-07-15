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
              selectInput("car", "Choose a car to buy:",
                          list(`Honda` = c("Civic LX", "Civic LX Honda Sensing", "Civic EX", "Civic EX-t", "Civic Touring", "2014 Civic LX"),
                               `Toyota` = c("Corolla LE", "Corolla LE ECO", "Prius", "2014 Camry Hybrid"),
                               `Hyundai` = c("Elantra SE", "Accent LE"),
                               `Custom`  = c("New", "Used")),
                          selected = 'Civic EX-t'
              ),
              numericInput("price",
                           "Loan Value ($)",
                           value = 29535,
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
                           value = 6.53,
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
                           width = 100),
              
              numericInput("insurance",
                           "Insurance (monthly)",
                           value = 100,
                           min = 0,
                           max = 300,
                           step = 10,
                           width = 100)
      
      ),
      
      mainPanel(
              
              tabsetPanel(
                tabPanel("Summary", tags$p("Monthly Payment:"), 
                                    verbatimTextOutput("MonthlyPay"),
                                    tags$p("Borrowing Cost:"),
                                    verbatimTextOutput("BorrowingCost"),
                                    tags$p("Total Purchase Cost:"),
                                    verbatimTextOutput("purchaseCost"),
                                    tags$p("Total Fuel Cost:"),
                                    verbatimTextOutput("fuelCost"),
                                    tags$p("Total Maintenace & Repair Cost:"),
                                    verbatimTextOutput("repairCost"),
                                    tags$p("Total Insurance Cost:"),
                                    verbatimTextOutput("insuranceCost"),
                                    tags$p("Total Ownership Cost:"),
                                    verbatimTextOutput("ownershipCost"),
                                    tags$p("Annual Ownership Cost"),
                                    verbatimTextOutput("AnnualCost"),
                                    tags$p("Total Cost per km:"),
                                    verbatimTextOutput("PerKMCost")                       
                                        

                ),
                tabPanel("Loan Table",  tableOutput("PaymentTable"))
                
                
              )
      )
   )

)
# Define server logic required to draw a histogram
server <- function(input, output, session) {

   observe({  
           if (input$car == "Civic LX") {
                   updateNumericInput(session, "price", value = 24383.17)
                   updateNumericInput(session, "fuelEconomy", value = 6.92)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 0.99)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 1.49)        
                   }
                   if (input$term == 84) {
                           updateNumericInput(session, "rate", value = 1.99)        
                   }
        
           }
           
           if (input$car == "Civic LX Honda Sensing") {
                   updateNumericInput(session, "price", value = 25503)
                   updateNumericInput(session, "fuelEconomy", value = 6.92)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 0.99)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 1.49)        
                   }
                   if (input$term == 84) {
                           updateNumericInput(session, "rate", value = 1.99)        
                   }
                   
           }
           if (input$car == "Civic EX") {
                   updateNumericInput(session, "price", value = 27967)
                   updateNumericInput(session, "fuelEconomy", value = 6.92)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 0.99)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 1.49)        
                   }
                   if (input$term == 84) {
                           updateNumericInput(session, "rate", value = 1.99)        
                   }
                   
           }
           if (input$car == "Civic EX-t") {
                   updateNumericInput(session, "price", value = 29535)
                   updateNumericInput(session, "fuelEconomy", value = 6.53)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 0.99)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 1.49)        
                   }
                   if (input$term == 84) {
                           updateNumericInput(session, "rate", value = 1.99)        
                   }
                   
           }
           
           if (input$car == "Civic Touring") {
                   updateNumericInput(session, "price", value = 31999)
                   updateNumericInput(session, "fuelEconomy", value = 6.53)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 0.99)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 1.49)        
                   }
                   if (input$term == 84) {
                           updateNumericInput(session, "rate", value = 1.99)        
                   }
                   
           }
           
           if (input$car == "2014 Civic LX") {
                   updateNumericInput(session, "price", value = 17920)
                   updateNumericInput(session, "fuelEconomy", value = 7.35)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 5)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 6.24)        
                   }

           }
           
           if (input$car == "Corolla LE") {
                   updateNumericInput(session, "price", value = 24122)
                   updateNumericInput(session, "fuelEconomy", value = 7.59)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 0.49)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 0.99)        
                   }
                   if (input$term == 84) {
                           updateNumericInput(session, "rate", value = 0.99)        
                   }
           }
           
           if (input$car == "Corolla LE ECO") {
                   updateNumericInput(session, "price", value = 24682)
                   updateNumericInput(session, "fuelEconomy", value = 6.92)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 0.49)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 0.99)        
                   }
                   if (input$term == 84) {
                           updateNumericInput(session, "rate", value = 0.99)        
                   }
           }
           
           if (input$car == "Prius") {
                   updateNumericInput(session, "price", value = 36377)
                   updateNumericInput(session, "fuelEconomy", value = 4.2)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 1.99)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 2.49)        
                   }
                   if (input$term == 84) {
                           updateNumericInput(session, "rate", value = 2.99)        
                   }
           }
           
           if (input$car == "2014 Camry Hybrid") {
                   updateNumericInput(session, "price", value = 17640)
                   updateNumericInput(session, "fuelEconomy", value = 6.03)
                   updateNumericInput(session, "odometer", value = 26000)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 2.99)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 5.24)        
                   }
                   if (input$term == 84) {
                           updateNumericInput(session, "rate", value = 6)     
                         
                   }
           }
           
           if (input$car == "Elantra SE") {
                   updateNumericInput(session, "price", value = 27248.48)
                   updateNumericInput(session, "fuelEconomy", value = 7.13)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 0)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 0)        
                   }
                   if (input$term == 84) {
                           updateNumericInput(session, "rate", value = 0.99)        
                   }
           }
           
           if (input$car == "Accent LE") {
                   updateNumericInput(session, "price", value = 19632.48)
                   updateNumericInput(session, "fuelEconomy", value = 7.59)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 0)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 0)        
                   }
                   if (input$term == 84) {
                           updateNumericInput(session, "rate", value = 0)        
                   }
           }
           

           if (input$car == "Used") {
             updateNumericInput(session, "rate", value = 6)
             updateNumericInput(session, "odometer", value = 50000)
             updateNumericInput(session, "fuelEconomy", value = 8)
           }
           
           
   })
   output$MonthlyPay <- renderText({ 
           monthly <- PMT(input$rate/(100*12), input$term, input$price)
   })
  
   calcLoan <- reactive ({
           
           monthly <- PMT(input$rate/(100*12), input$term, input$price)
           payment <- data.frame(matrix(NA, nrow = input$term, ncol = 6))
           colnames(payment) <- c("Month", "Starting balance", "Payment", "Applied to Interest", "Applied to Principal", "Remaining Loan Balance")
           starting_balance <- input$price

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
   
   calcRepair <- reactive ({
           odometer <- input$odometer
           if (odometer >= 100000) {
                   repairCost <-  (input$life - odometer) * 0.07
           } else if ((odometer < 100000) & (odometer > 45000)) {
                   repairCost <- (100000 - odometer) * 0.035 + (input$life - 100000) * 0.07
           } else if (odometer <= 45000) {
                   repairCost <- (45000 - odometer) * 0.01 + (100000-45000) * 0.035 + (input$life - 100000) * 0.07
           }
           list (repairCost = repairCost)
   })    
   

   borrowCost <- reactive ({
    sum(calcLoan()$payment$'Applied to Interest')    
   })
   
   costPer <- reactive ({
           gasCost <- ((input$life-input$odometer) / 100 * input$fuelEconomy * input$gasPrice)  
           monthsUsed <- ((input$life - input$odometer) / input$annual) * 12
           insuranceCost <- input$insurance * monthsUsed
           
           totalCost <- (gasCost +  (input$price - borrowCost())) + calcRepair()$repairCost + insuranceCost
           kmCost <- totalCost / (input$life-input$odometer)
           annualCost <- totalCost / (input$life / input$annual)

           list (gasCost = gasCost, totalCost = totalCost, kmCost = kmCost, annualCost = annualCost, insuranceCost = insuranceCost)
   })
   
  
   output$PaymentTable <- renderTable({
           calcLoan()$payment
   })
   
   
   output$BorrowingCost <- renderText({
         -borrowCost()      
   })
   
   output$ownershipCost <- renderText({
          costPer()$totalCost 
   })
   
   output$purchaseCost <- renderText({
           costPer()$totalCost - costPer()$gasCost
   })
   
   output$fuelCost <- renderText({
           costPer()$gasCost
   })
   
   output$repairCost <- renderText({
           calcRepair()$repairCost
   })
   
   output$insuranceCost <- renderText({
           costPer()$insuranceCost
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

