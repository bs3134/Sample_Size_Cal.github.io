# one sample continuous sample size calculation
rm(list = ls())
library(shiny)
ui = shinyUI(fluidPage(
  sidebarPanel(
    numericInput("mu1", "mu1:",0.5, min = 0, max = 1),
    numericInput("mu2", "mu2:", 0.5,min = 0, max = 1),
    numericInput("alpha", "significant level:", 0.05,min = 0, max = 1),
    numericInput("power", "power:", 0.8,min = 0, max = 1),
    numericInput("delta", "delta:", 0.5),
    numericInput("sigma","sigma=",0.5),
    selectInput("choice", label = h3("Select test type"),choices = c("equality","equivalence","inferiority/superiority")
    )),
    mainPanel(fluidRow(column(3, verbatimTextOutput("text_calc"))))
))
server = shinyServer(function(input, output,session){
  output$text_calc = renderText({
    mu1 = input$mu1
    mu2 = input$mu2
    alpha = input$alpha
    power = input$power
    delta = input$delta
    sigma = input$sigma
    choice = input$choice
    paste("The result is =", if (choice=="equality"){((qnorm(alpha/2)+qnorm(1-power))^2)*(sigma^2)/((mu1-mu2)^2)}
          else if (choice=="equivalence"){((qnorm(alpha)+qnorm((1-power)/2))^2)*(sigma^2)/(delta-abs(mu1-mu2))^2}
          else {((qnorm(alpha)+qnorm(1-power))^2)*(sigma^2)/(delta-mu1-mu2)^2})
  })
})
shinyApp(ui = ui, server = server)

