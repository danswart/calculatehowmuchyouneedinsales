library(shiny)
library(scales)
library(grid)

ui <- fluidPage(
    titlePanel("I Need HOW MUCH in Sales!?!"),
    
    sidebarLayout(
        sidebarPanel(
            h4("(Make changes to the sample numbers, press 'Calculate', see the effect on Sales, save a pdf)"),
            numericInput("labor_percentage", "Labor Percentage:", value = 33, min = 0, max = 100, step = 1),
            numericInput("other_percentage", "Other Expenses Percentage:", value = 33, min = 0, max = 100, step = 1),
            numericInput("owner_salary", "Annual Owner Salary:", value = 150000, step = 1000),
            numericInput("owner_retirement", "Annual Retirement Plan Contribution:", value = 15000, step = 1000),
            numericInput("owner_health_insurance", "Owner's Annual Health Insurance Premiums:", value = 18000, step = 1000),
            numericInput("net_profit", "Desired Net Profit Before Taxes:", value = 50000, step = 1000),
            actionButton("calculate", "Calculate"),
            downloadButton("downloadPDF", "Download PDF")
        ),
        
        mainPanel(
            h4("Approximate Sales Needed to Support Profit & Cost Structure Specified:"),
            verbatimTextOutput("gross_revenue"),
            br(),
            h4("Calculation Details:"),
            verbatimTextOutput("calculation_details")
        )
    )
)

server <- function(input, output) {
    calculation_results <- reactiveValues(text = NULL, gross_revenue = NULL)
    
    observeEvent(input$calculate, {
        labor_percentage <- input$labor_percentage / 100
        other_percentage <- input$other_percentage / 100
        owner_salary <- input$owner_salary
        owner_retirement <- input$owner_retirement
        owner_health_insurance <- input$owner_health_insurance
        net_profit_pre_tax <- input$net_profit
        
        gross_revenue <- (owner_salary + owner_retirement + 
                              owner_health_insurance + net_profit_pre_tax) / (1 - labor_percentage - other_percentage)
        
        calculation_results$gross_revenue <- paste("$", comma(gross_revenue, big.mark = ","), sep = "")
        calculation_results$text <- paste0("Owner's Annual Salary: $", comma(owner_salary, big.mark = ","), "\n",
                                           
                                           "Owner's Annual Retirement Plan Contribution: $", comma(owner_retirement, big.mark = ","), "\n",
                                           
                                           "Owner's Annual Health Insurance Premiums: $", comma(owner_health_insurance, big.mark = ","), "\n",
                                           "Net Profit before Taxes: $", comma(net_profit_pre_tax, big.mark = ","), "\n",
                                           "Labor Cost Percentage: ", labor_percentage * 100, "%\n",
                                           "Other Cost Percentage: ", other_percentage * 100, "%\n",
                                           "\n",
                                           "Gross Revenue Calculation:\n",
                                           "-------------------------\n",
                                           "Gross Revenue Needed = (Owner's Annual Salary + Owner's Annual Retirement Plan Contribution + Owner's Annual Health Insurance Premiums + Net Profit before Taxes) / (1 - Labor Cost Percentage - Other Cost Percentage)\n",
                                           "Gross Revenue Needed = (", comma(owner_salary, big.mark = ","), " + ", comma(owner_retirement, big.mark = ","), " + ", comma(owner_health_insurance, big.mark = ","), " + ", comma(net_profit_pre_tax, big.mark = ","), ") / (1 - ", labor_percentage * 100, "% - ", other_percentage * 100, "%)")
        
        output$gross_revenue <- renderText({
            calculation_results$gross_revenue
        })
        
        output$calculation_details <- renderText({
            calculation_results$text
        })
    })
    
    output$downloadPDF <- downloadHandler(
        filename = function() {
            paste("calculation-details", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
            pdf(file, width = 8, height = 11)
            grid.text(paste("Approximate Sales Needed to Support Profit & Cost Structure Specified:", calculation_results$gross_revenue), x = 0.5, y = 0.7, gp = gpar(fontsize = 12))
            grid.text(calculation_results$text, x = 0.5, y = 0.3, gp = gpar(fontsize = 12))
            grid.text(paste("Shiny App Created by Dan Swart on", Sys.Date()), x = 0.5, y = 0.1, gp = gpar(fontsize = 12))
            dev.off()
        }
    )
}

shinyApp(ui, server)
