rm(list = ls())
library(tidyverse)
library(shiny)
library(ggplot2)
ui = shinyUI(fluidPage(
  titlePanel("Spider Plot and Waterfall Plot"),
  sidebarPanel(
    fileInput("file1","Choose CSV File",multiple = FALSE),
    helpText("The input data must be csv and contains several columns: 'time', 'percentChange', 'name', 'cstatus' and 'treatment'. PercentChange column is the tumor size change data. 
             Cstatus column indicates if censored or not. Treatment column indicates different treatment groups. Name column is usually the id of patients.
             The name of columns in the input data must be exactly the same as mentioned above. 
             This plot code is made based on the 'sampleData' data in the 'tumgr' package. You can regard that database as an example."),
    selectInput("choice", label = h3("Select plot type"),choices = c("spider plot","waterfall plot")
    )
    ),
  mainPanel(plotOutput("Spiderplot"))))

server = shinyServer(function(input, output,session){
  output$Spiderplot = renderPlot({
      req(input$file1) 
      file1 = input$file1 
      choice = input$choice
      df = read.csv(file1$datapath)
      if (choice == "spider plot"){
      ggplot(df,aes(x=time,y=percentChange,group=name)) +
        geom_line(aes(color=treatment)) +
        geom_point(aes(shape=cstatus, color=treatment))+
        scale_shape_identity() }
      else {df = df%>%
        group_by(name)%>%
        mutate(patient=percentChange[which.max(abs(percentChange))])%>%
        select(name,patient,cstatus)
      o=order(df$patient,decreasing = TRUE)
      df=df[o,]
      df=distinct(df)  
      col = ifelse(df$cstatus == 0, "#BC5A42", "#009296")
      df=t(df[,-3])
        
      barplot(df["patient",], col=col, border=col, space=1,width=0.1,
                    cex.axis=1.2, cex.lab=1.4, ylim = c(-200,500))}
})})
shinyApp(ui = ui, server = server)



