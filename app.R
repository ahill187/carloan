#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
PMT <- function(rate, nper,pv, fv=0, type=0){
        pmt = ifelse(rate!=0,
                     (rate*(fv+pv*(1+ rate)^nper))/((1+rate*type)*(1-(1+ rate)^nper)),
                     (-1*(fv+pv)/nper )
        ) 
        
        return(pmt)
}
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
   
   # Application title
   titlePanel("Canadian Car Purchase Calculator: Is it better to buy new or used?"),
   
   # Sidebar with a slider input for number of bins 

   
   sidebarLayout(
      sidebarPanel(
              selectInput("car", "Choose a car:",
                          list(`Honda` = c("2017 Civic LX", "2017 Civic LX Honda Sensing", "2017 Civic EX", "2017 Civic EX-t", "2017 Civic Touring", "2014 Civic LX"),
                               `Toyota` = c("2017 Corolla LE", "2017 Corolla LE ECO", "2017 Prius", "2017 Camry LE", "2017 Camry Hybrid LE", "2014 Camry Hybrid"),
                               `Nissan` = c("2017 Sentra SL", "2017 Altima 2.5 SV"),
                               `Hyundai` = c("2017 Elantra SE", "2017 Accent LE"),
                               `Custom Vehicle`  = c("New", "Used")),
                          selected = '2017 Civic EX-t'
              ),
              numericInput("price",
                           "Vehicle Price ($)",
                           value = 29535,
                           min = 0,
                           max = 1000000,
                           step = 1000,
                           width = 200),
              
              numericInput("downpayment",
                           "Downpayment ($)",
                           value = 0,
                           min = 0,
                           max = 100000,
                           step = 1000,
                           width = 200),
              
              numericInput("term",
                           "Loan Term (months)",
                           value = 60,
                           min = 0,
                           max = 84,
                           step = 12,
                           width = 200),

              numericInput("rate",
                          "Interest Rate: (%, Annual)",
                          min = 0,
                          max = 10,
                          step = 0.01,
                          value = 0.99,
                          width = 200),
              
              numericInput("odometer",
                           "Current Odometer (km)",
                           value = 0,
                           min = 0,
                           max = 500000,
                           step = 10000,
                           width = 200),
              
              numericInput("life",
                           "Driving Until? (km)",
                           value = 200000,
                           min = 100000,
                           max = 500000,
                           step = 10000,
                           width = 200),
              
              numericInput("annual",
                           "Annual km",
                           value = 15000,
                           min = 0,
                           max = 200000,
                           step = 10000,
                           width = 200),    
              
              numericInput("fuelEconomy",
                           "Fuel Economy (L/100 km)",
                           value = 6.53,
                           min = 0,
                           max = 25,
                           step = 0.1,
                           width = 200),
              
              numericInput("gasPrice",
                           "Gas ($/L)",
                           value = 1.31,
                           min = 0,
                           max = 5,
                           step = 0.1,
                           width = 200),
              
              numericInput("insurance",
                           "Insurance (monthly)",
                           value = 200,
                           min = 0,
                           max = 500,
                           step = 10,
                           width = 200),
              
              checkboxInput("includeRepair",
                            "Include Maintenance Costs",
                            value = TRUE)
      
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
                tabPanel("Affordability", numericInput("income",
                                                       "Gross Monthly Income ($)",
                                                       value = 3000,
                                                       min = 0,
                                                       max = 10000,
                                                       step = 100,
                                                       width = 200),
                         numericInput("rent",
                                      "Monthly Rent / Mortgage ($)",
                                      value = 1100,
                                      min = 0,
                                      max = 10000,
                                      step = 100,
                                      width = 200),
                         
                         numericInput("otherDebt",
                                      "Other Monthly Debt Payments ($)",
                                      value = 0,
                                      min = 0,
                                      max = 10000,
                                      step = 100,
                                      width = 200),
                         
                         
                          textOutput("affordability")
                         
                         
                         ),
                
                
                tabPanel("Car vs. Transit", numericInput("transit",
                                                 "Annual Public Transit ($)",
                                                 value = 0,
                                                 min = 0,
                                                 max = 20000,
                                                 step = 100,
                                                 width = 200),
                         
                         numericInput("carsharing",
                                      "Annual Car-Sharing ($)",
                                      value = 0,
                                      min = 0,
                                      max = 20000,
                                      step = 100,
                                      width = 200),
                         
                         numericInput("convinience",
                                      "Vale of lost convinience ($)",
                                      value = 0,
                                      min = 0,
                                      max = 100000,
                                      step = 100,
                                      width = 200),
                         textOutput("vsTransit")),
                
                
                tabPanel("Loan Table",  tableOutput("PaymentTable")),
                tabPanel("Methodology", includeMarkdown("method.Rmd")),
                tabPanel("Disclaimer",  includeMarkdown("disclaimer.Rmd"))
                
              )
      )
   )

)
# Define server logic required to draw a histogram
server <- function(input, output, session) {

   observe({  
           if (input$car == "2017 Civic LX") {
                   updateNumericInput(session, "odometer", value = 0)
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
           
           if (input$car == "2017 Civic LX Honda Sensing") {
                   updateNumericInput(session, "odometer", value = 0)
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
           if (input$car == "2017 Civic EX") {
                   updateNumericInput(session, "odometer", value = 0)
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
           if (input$car == "2017 Civic EX-t") {
                   updateNumericInput(session, "odometer", value = 0)
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
           
           if (input$car == "2017 Civic Touring") {
                   updateNumericInput(session, "odometer", value = 0)
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
                   updateNumericInput(session, "odometer", value = 63000)
                   updateNumericInput(session, "fuelEconomy", value = 7.35)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 5)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 6.24)        
                   }

           }
           
           if (input$car == "2017 Corolla LE") {
                   updateNumericInput(session, "odometer", value = 0)
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
           
           if (input$car == "2017 Corolla LE ECO") {
                   updateNumericInput(session, "odometer", value = 0)
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
           
           if (input$car == "2017 Prius") {
                   updateNumericInput(session, "odometer", value = 0)
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
           
           if (input$car == "2017 Camry LE") {
                   updateNumericInput(session, "price", value = 28751)
                   updateNumericInput(session, "fuelEconomy", value = 8.71)
                   updateNumericInput(session, "odometer", value = 0)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 0.49)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 0.49)        
                   }
                   if (input$term == 84) {
                           updateNumericInput(session, "rate", value = 0.99)     
                           
                   }
           } 
           
           if (input$car == "2017 Camry Hybrid LE") {
                   updateNumericInput(session, "price", value = 37068.80)
                   updateNumericInput(session, "fuelEconomy", value = 5.88)
                   updateNumericInput(session, "odometer", value = 0)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 0.49)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 0.49)        
                   }
                   if (input$term == 84) {
                           updateNumericInput(session, "rate", value = 0.99)     
                           
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

           if (input$car == "2017 Sentra SL") {
                   updateNumericInput(session, "odometer", value = 0)
                   updateNumericInput(session, "price", value = 29045)
                   updateNumericInput(session, "fuelEconomy", value = 7.35)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 0)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 0)        
                   }
                   if (input$term == 84) {
                           updateNumericInput(session, "rate", value = 0.49)        
                   }
           }
           
           if (input$car == "2017 Altima 2.5 SV") {
                   updateNumericInput(session, "odometer", value = 0)
                   updateNumericInput(session, "price", value = 32800)
                   updateNumericInput(session, "fuelEconomy", value = 7.59)
                   if (input$term == 60) {
                           updateNumericInput(session, "rate", value = 0)        
                   }
                   if (input$term == 72) {
                           updateNumericInput(session, "rate", value = 0)        
                   }
                   if (input$term == 84) {
                           updateNumericInput(session, "rate", value = 0.49)        
                   }
           }

           if (input$car == "2017 Elantra SE") {
                   updateNumericInput(session, "odometer", value = 0)
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
           
           if (input$car == "2017 Accent LE") {
                   updateNumericInput(session, "odometer", value = 0)
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
           monthly <- PMT(input$rate/(100*12), input$term, input$price - input$downpayment)
   })
  
   calcLoan <- reactive ({
           
           monthly <- PMT(input$rate/(100*12), input$term, input$price - input$downpayment)
           payment <- data.frame(matrix(NA, nrow = input$term, ncol = 6))
           colnames(payment) <- c("Month", "Starting balance", "Payment", "Applied to Interest", "Applied to Principal", "Remaining Loan Balance")
           starting_balance <- input$price - input$downpayment

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
           list (repairCost = (input$includeRepair == 'TRUE')*repairCost)
   })    
   

   borrowCost <- reactive ({
    sum(calcLoan()$payment$'Applied to Interest')    
   })
   
   costPer <- reactive ({
           gasCost <- ((input$life-input$odometer) / 100 * input$fuelEconomy * input$gasPrice)  
           monthsUsed <- ((input$life - input$odometer) / input$annual) * 12
           insuranceCost <- input$insurance * monthsUsed
           purchaseCost <- (input$price - borrowCost())
           totalCost <- purchaseCost + gasCost + calcRepair()$repairCost + insuranceCost
           kmCost <- totalCost / (input$life-input$odometer)
           annualCost <- totalCost / (input$life / input$annual)
           monthlyCost <- annualCost / 12
           carBudget <- 0.36*input$income - (input$rent + input$otherDebt) 
           doIafford <- (carBudget > monthlyCost) 
           
           if (doIafford) { 
                   affordMessage <- "You can afford this car :-)"
           } else {
                   affordMessage <- "You cannot afford this car :-("
           }
  
           
           list (purchaseCost = purchaseCost, gasCost = gasCost, totalCost = totalCost, kmCost = kmCost, annualCost = annualCost, insuranceCost = insuranceCost, affordMessage = affordMessage)
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
           costPer()$purchaseCost
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
   

   output$affordability <- renderText({
           costPer()$affordMessage
   }) 
 
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)

