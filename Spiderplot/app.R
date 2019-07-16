rm(list = ls())

library(shiny)
library(ggplot2)
ui = shinyUI(fluidPage(
  titlePanel("Spider Plot"),
  sidebarPanel(
    fileInput("file1","Choose CSV File",multiple = FALSE)
    ),
  mainPanel(plotOutput("Spiderplot"))))

server = shinyServer(function(input, output,session){
  output$Spiderplot = renderPlot({
      req(input$file1) 
      file1 = input$file1 
      df = read.csv(file1$datapath)
      ggplot(df,aes(x=time,y=percentChange,group=name)) +
        geom_line(aes(color=treatment)) +
        geom_point(aes(shape=cstatus, color=treatment))+
        scale_shape_identity()
})})
shinyApp(ui = ui, server = server)



