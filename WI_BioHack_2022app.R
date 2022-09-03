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
  headerPanel('BioHack 2022: TB Drug Toxicity Predictor'),
  
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
    tags$img(src="Screenshot_20220902-224706_Office~2.jpg", height="200px", width="800px"),
    tags$img(src="images (6).jpeg",height="100px", width="800px"),
    verbatimTextOutput('contents'),
    tableOutput('tabledata'), # Prediction results table
    tags$text("*Active >>> Non-toxic  | Inactive >>> Toxic*")
    
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
    
    Output <- data.frame(Prediction=predict(model,test))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Prediction complete.") 
    } else {
      return("Server is ready for prediction.")
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



   
