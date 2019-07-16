
# Survival sample size

rm(list = ls())

library(shiny)
ui = shinyUI(fluidPage(
  titlePanel("Survival Analysis Sample Size"),
  sidebarPanel(
    numericInput("l1", "l1:",1),
    numericInput("l2", "l2:", 2),
    numericInput("alpha", "significant level:", 0.05,min = 0, max = 1),
    numericInput("power", "power:", 0.8,min = 0, max = 1),
    numericInput("totaltime","total time:",3),
    numericInput("accrualtime", "accrual time:",1),
    numericInput("delta", "delta:",0.1),
    selectInput("choice", label = h3("Select test type"),choices = c("equality","equivalence","inferiority/superiority")
    )),
  mainPanel(fluidRow(column(9, verbatimTextOutput("text_calc"))),tags$head(tags$style("#text_calc{color:red; font-size:20px; font-style:italic;}")))))

server = shinyServer(function(input, output,session){
  output$text_calc = renderText({
    
    l1 = input$l1
    l2 = input$l2
    alpha = input$alpha
    power = input$power
    totaltime = input$totaltime
    accrualtime = input$accrualtime
    choice = input$choice
    delta = input$delta
    paste("The result is =", if (choice=="equality"){((qnorm(alpha/2)+qnorm(1-power))^2)*(((l1^2)/(1+1/(exp(l1*totaltime)-exp(l1*(accrualtime-totaltime)))/l1*accrualtime))+((l2^2)/(1+(1/exp(l2*totaltime)-exp(l2*(accrualtime-totaltime)))/l2*accrualtime)))/((l1-l2)^2)}
          else if(choice=="equivalence"){((qnorm(alpha)+qnorm((1-power)/2))^2)*(((l1^2)/(1+(1/exp(l1*totaltime)-exp(l1*(accrualtime-totaltime)))/(l1*accrualtime)))+((l2^2)/(1+(1/exp(l2*totaltime)-exp(l2*(accrualtime-totaltime)))/l2*accrualtime)))/((delta-abs(l1-l2))^2)} 
          else{((qnorm(alpha)+qnorm(1-power))^2)*(((l1^2)/(1+(1/exp(l1*totaltime)-exp(l1*(accrualtime-totaltime)))/(l1*accrualtime)))+((l2^2)/(1+(1/exp(l2*totaltime)-exp(l2*(accrualtime-totaltime)))/l2*accrualtime)))/(((l2-l1)-delta)^2)})
  })
})
shinyApp(ui = ui, server = server)

