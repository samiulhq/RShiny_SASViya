library(shiny)
library(stringr)
library(ggplot2)
library(swat)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  

  titlePanel("SAS Tables Histogram Viewer"),
  tags$img(
    src = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/10/SAS_logo_horiz.svg/2560px-SAS_logo_horiz.svg.png",
    width = "100px",
    height = "50px"
  ),
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
 

  
  
  Sys.setenv(CAS_CLIENT_SSL_CA_LIST='/opt/anaconda3/my_CAS_Viya4_cert.pem')   
  cassess<-CAS("mipistcas.eastus.cloudapp.azure.com",5570,username="sahaqu",password="sahaqu")
  
  
  
  list_caslibs<-cas.table.caslibInfo(cassess)
  updateSelectInput(session = session, inputId = "caslibname", choices = list_caslibs$CASLibInfo$Name[str_order(list_caslibs$CASLibInfo$Name)])
  
  observeEvent(input$caslibname,
  {
    currentCaslib <- req(input$caslibname)
    list_tables<-cas.table.tableInfo(cassess,caslib=currentCaslib)
    if(length(list_tables)==0)
    {
      updateSelectInput(session = session, inputId = "tablename", choices ="No table in this library")
      updateSelectInput(session = session, inputId = "colname", choices = "")
    }
    else
    {
    updateSelectInput(session = session, inputId = "tablename", choices = list_tables$TableInfo$Name[str_order(list_tables$TableInfo$Name)])
    }
  }
  )
  
  observeEvent(input$tablename,
               {
               
                 table_info=cas.table.columnInfo(cassess,table=c(name=req(input$tablename),caslib=input$caslibname))
                 columns=table_info$ColumnInfo;
                 columns=columns[columns$Type=='double',]
                 updateSelectInput(session = session, inputId = "colname", choices = columns$Column[str_order(columns$Column)])
                
               })

  
  
  observeEvent(input$colname,{
    
    if(input$tablename!='Select CASLILB first'){
      cas_table <- defCasTable(cassess,caslib=input$caslibname,input$tablename)
      df<-to.casDataFrame((cas_table[,input$colname]))
      
    }
    
    
  #print(input$colname)
  if(input$colname!="Select a table first" & input$tablename!="No table in this library" ){  
  output$distPlot <- renderPlot({
  
    #x    <- df[,input$colname]
    #x <-as.data.frame(x)
    x    <- to.r.data.frame(cas_table[,input$colname])
    bins <- seq(min(x, na.rm = TRUE), max(x,na.rm = TRUE), length.out = input$bins + 1)
    print(x)
    #hist(x, breaks = unique(bins), col = "#75AADB", border = "white",
     #    xlab = input$colname,
      #   main = paste("Histogram",input$tablename,".",input$colname), ylab="")
    
    ggplot(x, aes(.data[[input$colname]]),na.rm = TRUE) +
      geom_histogram(bins = input$bins,
                     fill = "steelblue3",
                     colour = "grey30") +
      xlab(input$colname) +
      theme_minimal()
    
    
  })}
  
  }
  
  )
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
