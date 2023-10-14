library(shiny)
library(sasr)
library(stringr)
library(swat)
#library(getPass)
#library(tidyr)

#get a sas compute sesssion
my_sas_session <- get_sas_session()

#cassess<-CAS("example.sas.com",5570,username="username",password="password")
list_caslibs<-cas.table.caslibInfo(cassess)
dataflag=1

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("SAS Tables Histogram Viewer"),
  

 
  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("caslibname", "Select a CASLIB", choices = c("Loading CASLIB List")),
      
      selectInput("tablename", "Select a Table", choices = c("Select CASLILB first")),
      
      selectInput("colname", "Select a Column", choices = c("Select a table first")),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
    checkboxInput("showdata",
                               label = "Show Data",
                               value = FALSE)
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      DT::dataTableOutput(outputId="demo_datatable",
                          width = "50%",
                          height = "auto"),
      actionButton("doMean", "Run PROC MEANS"),
      h4("Result:"),
      verbatimTextOutput("Log"),
      h4("Log:"),
      verbatimTextOutput("Result")
     
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output,session) {


  print(list_caslibs)
  updateSelectInput(session = session, inputId = "caslibname", choices = list_caslibs$CASLibInfo$Name[str_order(list_caslibs$CASLibInfo$Name)])
  
  
  observeEvent(input$caslibname,
  {
    currentCaslib <- input$caslibname
    list_tables<-cas.table.tableInfo(cassess,caslib=currentCaslib)
    updateSelectInput(session = session, inputId = "tablename", choices = list_tables$TableInfo$Name[str_order(list_tables$TableInfo$Name)])
    
  }
  )
  
  
  observeEvent(input$tablename,
               {
                   table_info=cas.table.columnInfo(cassess,table=c(name=req(input$tablename),caslib=input$caslibname))
                 columns=table_info$ColumnInfo;
                 columns=columns[columns$Type=='double',]
                 updateSelectInput(session = session, inputId = "colname", choices = columns$Column[str_order(columns$Column)])
                
               })

  
  observeEvent(input$tablename,{
    
    
    if(input$showdata==TRUE){
      flag=1;
      ct <- defCasTable(cassess,caslib=input$caslibname,input$tablename)
      ctdf<-to.casDataFrame(ct,obs = 1000)
      output$demo_datatable <- DT::renderDataTable({
        ctdf
      }, options = list(pageLength = 10))
      
    }
  })
  
  
  observeEvent(input$showdata,
               {
                 print(input$showdata)
                 if(input$showdata==TRUE){
                   flag=1;
                 ct <- defCasTable(cassess,caslib=input$caslibname,input$tablename)
                 ctdf<-to.casDataFrame(ct,obs = 1000)
                 output$demo_datatable <- DT::renderDataTable({
                   ctdf
                 }, options = list(pageLength = 10))
                 
                 }
                 else{
                   
                   ctdf<- NULL
                   output$demo_datatable <- DT::renderDataTable({
                     ctdf
                   }, options = list(pageLength = 10))
                 }
                 
               })
  
  
  
  observeEvent(input$colname,{
    
    if(input$tablename!='Select CASLILB first'){
      cas_table <- defCasTable(cassess,caslib=input$caslibname,input$tablename)
      df<-to.casDataFrame((cas_table[,input$colname]))
      
      
    }
    
    
  print(input$colname)
  if(input$colname!="Select a table first"){  
  output$distPlot <- renderPlot({
  
    x    <- df[,input$colname]
    bins <- seq(min(x, na.rm = TRUE), max(x,na.rm = TRUE), length.out = input$bins + 1)
    print(x)
    hist(x, breaks = unique(bins), col = "#75AADB", border = "white",
         xlab = input$colname,
         main = paste("Histogram",input$tablename,".",input$colname), ylab="")
    
  })}
  }
  )
  observeEvent(input$doMean, {
    
    sascode=paste0("cas;caslib _all_assing; proc freq data=",strsplit(input$caslibname, "\\(")[[1]][1],".",input$tablename,";run;")
    
    result <- run_sas(sascode)
    cat(result$LOG)
    cat(result$LST)
        #output$text1 <- renderHTML({(result$LOG)})
    output$Log <- renderText({ result$LOG })
    output$Result <- renderText({ result$LST })
    #output$text1 <- renderText({ gsub(pattern = "\\n", replacement = "<br/>", result$LOG) })
    
  })
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
