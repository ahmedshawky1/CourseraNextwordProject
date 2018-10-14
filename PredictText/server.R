#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("./PrepareModel.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  observeEvent(input$predict,{
  
    a <-predictnext(input$inputtext)
    o="<font color='red'><h2><ul>";
    for (x in a) {
      if(!is.na(x))
       o = paste(o,'<li>',x,'</li>');
    }
    o = paste(o,'</ul></h2></font>');
    
    
  output$result <- renderUI ({ 
    HTML(o)
  
  })
  })
})
