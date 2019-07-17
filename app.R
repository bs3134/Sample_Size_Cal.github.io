# Proportion sample size calculation

rm(list = ls())
library(shiny)
ui = shinyUI(fluidPage(
  titlePanel("Proportion Test (Binary Sample Size Cal)"),
  sidebarPanel(
    numericInput("p0", "p0:",0.5, min = 0, max = 1),
    numericInput("p1", "p1:", 0.5,min = 0, max = 1),
    numericInput("alpha", "significant level:", 0.05,min = 0, max = 1),
    numericInput("power", "power:", 0.8,min = 0, max = 1),
    numericInput("delta", "delta:", 0.5),
    selectInput("choice1", label = h3("Select test type"),choices = c("equality","equivalence","inferiority/superiority")
    ),
    selectInput("choice2", label = h3("Select sample number"),choices = c("One-sample","Two-sample")),
    helpText("For test for equality method, delta is not required. Therefore type delta=0 when using test for equality")),
  mainPanel(fluidRow(column(9, verbatimTextOutput("text_calc"))),
            tags$head(tags$style("#text_calc{color:red; font-size:20px; font-style:italic;}")))    # change output size and color
))
server = shinyServer(function(input, output,session){
  output$text_calc = renderText({
    p0 = input$p0
    p1 = input$p1
    alpha = input$alpha
    power = input$power
    delta = input$delta
    choice1 = input$choice1
    choice2 = input$choice2
    paste("The result is =", if ((choice1=="equality")&(choice2=="One-sample")){((qnorm(1-alpha)*sqrt(p0*(1-p0))+qnorm(power)*sqrt(p1*(1-p1)))/(p1-p0))^2}
     else if ((choice1=="equivalence")&(choice2=="One-sample")){(((qnorm(1-alpha)+qnorm(1-(1-power)/2))^2)*p1*(1-p1))/((delta-abs(p1-p0))^2)}
     else if ((choice1=="inferiority/superiority")&(choice2=="One-sample")){(((qnorm(alpha)+qnorm(1-power))^2)*p1*(1-p1))/((delta+p1-p0)^2)}
     else if ((choice1=="equality")&(choice2=="Two-sample")){(2*((qnorm(alpha)+qnorm(1-power))^2)*((p0+p1)/2)*(1-(p0+p1)/2))/(p0-p1)^2}
     else if ((choice1=="equivalence")&(choice2=="Two-sample")){(((qnorm(alpha)+qnorm((1-power)/2))^2)*(p0*(1-p0)+p1*(1-p1)))/(delta-abs(p0-p1))^2}
     else {(((qnorm(alpha)+qnorm(1-power))^2)*(p0*(1-p0)+p1*(1-p1)))/(delta-(p0-p1))^2})
  })
})

shinyApp(ui = ui, server = server)

