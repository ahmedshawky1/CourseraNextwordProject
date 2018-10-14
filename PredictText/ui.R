library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predict Next Word"),
  helpText("This application will predict the next using 1 or 2 of the words you entered."),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(      
      helpText("Enter the sentence for which you want to predict the next word. Once filled out, click the Predict button below."),
      
      textInput("inputtext", "Sentence"),           
      br(),   
      actionButton("predict", "Predict Next Word"),      
      br()   
    )
    ,
    
    # Main panel
    mainPanel(
      
      h3("Predicted Words"),
      
      htmlOutput ("result")      
    )
  )
))