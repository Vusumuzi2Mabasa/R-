library(shiny)
library(data.table)
library(caret)




# Read in the SVM model
model <- readRDS("model.rds")

####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('TB Drug Toxicity Predictor'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Input parameters')),
    numericInput("MW", 
                 label = "MW", 
                 value = ""),
    numericInput("LogP", 
                 label = "LogP", 
                 value = ""),
    numericInput("NumHDonors", 
                 label = "NumHDonors", 
                 value = ""),
    numericInput("NumHAcceptors", 
                 label = "NumHAcceptors", 
                 value = ""),
    
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$img(src="pic2.png", height="200px", width="500px"),
    tags$video(src="images.mp4", type="video/mp4", id = "videoID",height="100px", width="500px"),
    verbatimTextOutput('contents'),
    tableOutput('tabledata'), # Prediction results table
    tags$text("*Active output is representative of non-toxic compounds and Inactive output is representative of
              toxic compounds")
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("MW",
               "LogP",
               "NumHDonors",
               "NumHAcceptors"),
      Value = as.character(c(input$MW,
                             input$LogP,
                             input$NumHDonors,
                             input$NumHAcceptors)),
      stringsAsFactors = T)
    
    Class <- 0
    df <- rbind(df, Class)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 10))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready to predict toxicity.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)



   
