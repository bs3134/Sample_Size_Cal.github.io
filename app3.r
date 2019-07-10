# Two-sample prop.test

rm(list = ls())
library(shiny)
ui = shinyUI(fluidPage(
  mainPanel(
    numericInput("p0", "p0:",0.5, min = 0, max = 1),
    numericInput("p1", "p1:", 0.5,min = 0, max = 1),
    numericInput("alpha", "significant level:", 0.05,min = 0, max = 1),
    numericInput("power", "power:", 0.8,min = 0, max = 1),
    checkboxInput('check', label = 'if check then use one-sided test; if not, then use two-sided test', value = FALSE)
  ),
  textOutput("text_calc"))
)
server = shinyServer(function(input, output,session){
  output$text_calc = renderText({
    p0 = input$p0
    p1 = input$p1
    alpha = input$alpha
    power = input$power
    paste("The result is =", if (input$check){(2*((qnorm(alpha)+qnorm(1-power))^2)*((p0+p1)/2)*(1-(p0+p1)/2))/(p0-p1)^2}
          else{(2*((qnorm(alpha/2)+qnorm(1-power))^2)*((p0+p1)/2)*(1-(p0+p1)/2))/(p0-p1)^2} )    
  })
})

shinyApp(ui = ui, server = server)