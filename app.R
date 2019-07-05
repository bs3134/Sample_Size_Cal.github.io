

rm(list = ls())
library(shiny)
ui = shinyUI(fluidPage(
    mainPanel(
    numericInput("p0", "p0:",0.5, min = 0, max = 1),
    numericInput("p1", "p1:", 0.5,min = 0, max = 1),
    numericInput("alpha", "significant level:", 0.05,min = 0, max = 1),
    numericInput("power", "power:", 0.8,min = 0, max = 1),
    textOutput("text_calc"))
    ))
server = shinyServer(function(input, output,session){
  output$text_calc <- renderText({
    p0 = input$p0
    p1 = input$p1
    alpha = input$alpha
    power = input$power
    paste("The result is =", ((qnorm(1-alpha)*sqrt(p0*(1-p0))+qnorm(power)*sqrt(p1*(1-p1)))/(p1-p0))^2)
  })
})

shinyApp(ui = ui, server = server)

