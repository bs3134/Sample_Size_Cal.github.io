# One-sample Prop.test

rm(list = ls())
library(shiny)
ui = shinyUI(fluidPage(
  mainPanel(
    numericInput("p0", "p0:",0.5, min = 0, max = 1),
    numericInput("p1", "p1:", 0.5,min = 0, max = 1),
    numericInput("alpha", "significant level:", 0.05,min = 0, max = 1),
    numericInput("power", "power:", 0.8,min = 0, max = 1),
    numericInput("delta", "delta:", 0.5),
    selectInput("choice", label = h3("Select test type"),choices = c("equality","equivalence","inferiority/superiority")
    ),
    textOutput("text_calc"))
))
server = shinyServer(function(input, output,session){
  output$text_calc = renderText({
    p0 = input$p0
    p1 = input$p1
    alpha = input$alpha
    power = input$power
    delta = input$delta
    choice = input$choice
    paste("The result is =", if (choice=="equality"){((qnorm(1-alpha)*sqrt(p0*(1-p0))+qnorm(power)*sqrt(p1*(1-p1)))/(p1-p0))^2}
     else if (choice=="equivalence"){(((qnorm(1-alpha)+qnorm(1-(1-power)/2))^2)*p1*(1-p1))/((delta-abs(p1-p0))^2)}
     else {(((qnorm(alpha)+qnorm(1-power))^2)*p1*(1-p1))/((delta+p1-p0)^2)})
  })
})

shinyApp(ui = ui, server = server)

