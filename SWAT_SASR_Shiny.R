### created by Samiul Haque
## Last Update November 27, 2023

library(shiny)
library(sasr)
library(stringr)
library(swat)
library(ggplot2)
library(hrbrthemes)

#get a sas compute sesssion
my_sas_session <- get_sas_session()


###test if the session is active################################################
er<-try({run_sas("")},silent=TRUE)

if(class(er)=='try-error'){ 
  if(any(grepl("package:sasr", search()))) detach("package:sasr")
  library(sasr)
  print('Restarting SAS Compute Session....')
  my_sas_session <- get_sas_session()
}
################################################################################


cassess<-CAS("www.examplesasviya.com",5570,username="",password="")
list_caslibs<-cas.table.caslibInfo(cassess)
dataflag=1
scatterflag=0;
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("SAS Data Viewer"),
  
  
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
                               value = FALSE),
    checkboxInput("showscatter",
                  label = "Scatterplot",
                  value = FALSE),
    uiOutput("xaxis"),
    uiOutput("yaxis"),
    actionButton("doMean", "Run PROC FREQ", class="btn btn-primary"),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      plotOutput(outputId = "scatterPlot"),
      DT::dataTableOutput(outputId="demo_datatable",
                          width = "50%",
                          height = "auto"),
     
      textOutput("logheader"),
      tags$head(tags$style("#logheader{color: blue;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
      )
      )
    ,
      verbatimTextOutput("Log"),
      textOutput("resultheader"),
    tags$head(tags$style("#resultheader{color: blue;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
    )
    ),
    
      verbatimTextOutput("Result"),
      htmlOutput("resulthtml")
      
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
                   table_info=cas.table.columnInfo(cassess,table=list(name=req(input$tablename),caslib=input$caslibname))
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
  
  observeEvent(input$doMean, {
    output$logheader<-renderText("Log:")
    output$resultheader<-renderText("Result:")
    sascode=paste0("cas;caslib _all_ assign; proc freq data=",strsplit(input$caslibname, "\\(")[[1]][1],".",input$tablename,";run;")
    cat(sascode)
    result <- run_sas(sascode,result="HTML")
    cat(result$LOG)
    cat(result$LST)
        #output$text1 <- renderHTML({(result$LOG)})
    output$Log <- renderText({ result$LOG })
    #output$Result <- renderText({ result$LST })
    #output$text1 <- renderText({ gsub(pattern = "\\n", replacement = "<br/>", result$LOG) })
    output$resulthtml<-renderUI(HTML(result$LST))
  })
  
  
  observeEvent(input$showscatter, {
    table_info=cas.table.columnInfo(cassess,table=list(name=req(input$tablename),caslib=input$caslibname))    
    columns=table_info$ColumnInfo;
    columns=columns[columns$Type=='double',]
    print(columns)
    print(input$showscatter)
    if(input$showscatter==TRUE){
      
      output$xaxis <- renderUI({
        selectInput("xlabel", "X:", choices = columns$Column[str_order(columns$Column)])
      })
        output$yaxis <- renderUI({
          selectInput("ylabel", "y:", choices = columns$Column[str_order(columns$Column)])
          
          
      })
      

      
      scatterflag=1
      }
    else{
      output$scatterPlot<-NULL
      output$xaxis<-NULL
      output$yaxis<-NULL
      scatterflag=0
    }
    
    
  })
  
  observeEvent(input$xlabel,{
    
     

    #  print(dtbl)
      output$scatterPlot <- renderPlot({
        cas_table <- defCasTable(cassess,caslib=input$caslibname,input$tablename)
        print('cas_table')
        print(input$xlabel)
        print(input$ylabel)
        dtbl    <- to.r.data.frame(cas_table[,list(input$xlabel,input$ylabel)])
    ggplot(dtbl, aes(x=.data[[input$xlabel]], y=.data[[input$ylabel]], na.rm = TRUE)) +
      geom_point(size=6) +
          theme_minimal()



      })

    }
  )
  
 
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

