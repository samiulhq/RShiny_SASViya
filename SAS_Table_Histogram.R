library(shiny)
library(stringr)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  titlePanel("SAS Tables Histogram Viewer"),
  
  selectInput("caslibname", "Select a CASLIB", choices = c("Loading CASLIB List")),
 
  selectInput("tablename", "Select a Table", choices = c("Select CASLILB first")),
 
  selectInput("colname", "Select a Column", choices = c("Select a table first")),
  # App title ----
  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output,session) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  library(swat)
  #library(getPass)
  #library(tidyr)
  
  Sys.setenv(CAS_CLIENT_SSL_CA_LIST='/opt/anaconda3/my_CAS_Viya4_cert.pem')   
  cassess<-CAS("cas.com,",5570,username="psfss",password="psfsf")
  
  
  
  list_caslibs<-cas.table.caslibInfo(cassess)
  
  updateSelectInput(session = session, inputId = "caslibname", choices = list_caslibs$CASLibInfo$Name[str_order(list_caslibs$CASLibInfo$Name)])
  
  
  
  
  
  
  
  observeEvent(input$caslibname,
  {
    currentCaslib <- req(input$caslibname)
    list_tables<-cas.table.tableInfo(cassess,caslib=currentCaslib)
    updateSelectInput(session = session, inputId = "tablename", choices = req(list_tables$TableInfo$Name[str_order(list_tables$TableInfo$Name)]))
    
  }
  )
  
  
  
  
  
  
  observeEvent(input$tablename,
               {
                 # Use this function to update the choices for the user.
                 # First argument is session, next the input to update,
                 # and third the new choices. Here, I'm filtering the
                 # previously made data.frame to based on the series column,
                 # and returning the choices column. 
                 # `drop=TRUE` makes it explicit that I want a vector returned.
                 
                 
                 table_info=cas.table.columnInfo(cassess,table=c(name=req(input$tablename),caslib=input$caslibname))
                 columns=table_info$ColumnInfo;
                 columns=columns[columns$Type=='double',]
                 updateSelectInput(session = session, inputId = "colname", choices = columns$Column[str_order(columns$Column)])
                
               })

  
  
  observeEvent(input$colname,{
    
    if(input$tablename!='Select CASLILB first'){
      cas_table <- defCasTable(cassess,caslib=input$caslibname,input$tablename)
      df<-to.casDataFrame((cas_table))
      
    }
    
    
  print(input$colname)
  if(input$colname!="Select a table first"){  
  output$distPlot <- renderPlot({
  
    x    <- df[,input$colname]
    bins <- seq(min(x, na.rm = TRUE), max(x,na.rm = TRUE), length.out = input$bins + 1)
    print(x)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = input$colname,
         main = paste("Histogram",input$tablename,".",input$colname), ylab="")
    
  })}
  
  }
  
  )
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
