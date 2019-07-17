#  continuous sample size calculation
rm(list = ls())
library(shiny)
ui = shinyUI(fluidPage(
  titlePanel("Continuous Sample Size Test"),
  sidebarPanel(
    numericInput("mu1", "mu1:",0.5, min = 0, max = 1),
    numericInput("mu2", "mu2:", 0.5,min = 0, max = 1),
    numericInput("alpha", "significant level:", 0.05,min = 0, max = 1),
    numericInput("power", "power:", 0.8,min = 0, max = 1),
    numericInput("delta", "delta:", 0.5),
    numericInput("sigma","sigma:",0.5),
    selectInput("choice1", label = h3("Select test type"),choices = c("equality","equivalence","inferiority/superiority")
    ),
    selectInput("choice2", label = h3("Select sample number"),choices = c("One-sample","Two-sample")),
    helpText("For test for equality method, delta is not required. Therefore type delta=0 when using test for equality")
  ),
    mainPanel(fluidRow(column(9, verbatimTextOutput("text_calc"))),
              tags$head(tags$style("#text_calc{color:red; font-size:20px; font-style:italic;}")))))

server = shinyServer(function(input, output,session){
  output$text_calc = renderText({
    mu1 = input$mu1
    mu2 = input$mu2
    alpha = input$alpha
    power = input$power
    delta = input$delta
    sigma = input$sigma
    choice1 = input$choice1
    choice2 = input$choice2
    paste("The result is =", if ((choice1=="equality")&(choice2=="One-sample")){((qnorm(alpha/2)+qnorm(1-power))^2)*(sigma^2)/((mu1-mu2)^2)}
          else if ((choice1=="equivalence")&(choice2=="One-sample")){((qnorm(alpha)+qnorm((1-power)/2))^2)*(sigma^2)/(delta-abs(mu1-mu2))^2}
          else if ((choice1=="inferiority/superiority")&(choice2=="One-sample")){((qnorm(alpha)+qnorm(1-power))^2)*(sigma^2)/(delta-mu1-mu2)^2}
          else if ((choice1=="equality")&(choice2=="Two-sample")){2*((qnorm(alpha/2)+qnorm(1-power))^2)*(sigma^2)/(mu1-mu2)^2}
          else if ((choice1=="equivalence")&(choice2=="Two-sample")){2*((qnorm(alpha)+qnorm((1-power)/2))^2)*(sigma^2)/(delta-abs(mu1-mu2))^2}
          else {2*((qnorm(alpha)+qnorm(1-power))^2)*(sigma^2)/(delta+mu1-mu2)^2})
  })
})
shinyApp(ui = ui, server = server)

