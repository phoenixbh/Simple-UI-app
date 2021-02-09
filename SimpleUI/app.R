library(shiny)
library(shinybusy)
library(httr)
library(data.table)

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel(
    # "Receptiviti Basic User Interface"
    # Receptiviti Logo
    tags$a(href = 'http://www.receptiviti.com',
              img(src = 'receptiviti_logo.png',
                  title = "Receptiviti", height = "40px"))
    ),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
 

    # Sidebar panel for inputs ----
    sidebarPanel(width=3, position="left",
      
      # Horizontal line ----
      tags$hr(),
      
      # Api keys
      textInput("apikey","API Key",value="<Enter API Key Here>"),
      passwordInput("apisecret", "API Secret",placeholder="<Enter API Secret Here>"),
      
      tags$hr(),
      
      h4("File Upload"),
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),

      actionButton("process","Submit",icon("paper-plane")),
      
      # Horizontal line ----
      tags$hr(),

      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),

      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),

      # Horizontal line ----
      tags$hr()

    ),

    # Main panel for displaying outputs ----
    mainPanel(
      # Busy spinner
      add_busy_spinner(spin = "fading-circle",position="bottom-right"),
      
      # Instructions
      tags$b(h3("Basic Receptiviti User Interface")),
      tags$hr(),
      
      tags$b("1."), "To use this UI, first enter your API key and API secret in the panels to the left. If you don't yet have an API account with Receptiviti,
      you can sign up here: ",tags$a(href="https://dashboard.receptiviti.com/","https://dashboard.receptiviti.com/"),
      br(), "If you already have an account you can find your API keys at the same link after logging in.",
      br(), br(),
    
      tags$b("2."),"Choose a file in CSV format to process using the browse tool to the left. The file needs to have two columns: 
      the first containing a unique identifier for each language sample and the second containing the text to be analysed. 
      You can select whether or not the CSV file contains column names (i.e. a header), how the file is delimited (comma, semicolon, or tab separator), 
      and how quotes are represented (i.e. none, single or double quotes).",
      br(),br(),
      
      tags$b("3."),"Click on the 'Submit' button to begin processing the CSV file containing your language samples.",
      br(),br(),
      
      tags$b("4."),"Once you have uploaded a file, it will be processed through the Receptiviti API. Upon completion, 
      a button will appear below allowing you to download the results as a CSV file.",
      br(),br(),
      
      # Button
      uiOutput('button2'),
      br(),br(),
      "For information on how to interpret each of the output measures, please consult the documentation at",tags$a(href="https://dashboard.receptiviti.com/docs","https://dashboard.receptiviti.com/docs")
      

    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output) {

  # Check if file uploaded
  output$fileUploaded <- reactive({
    df <- ntext()
    return(!is.null(df))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  # Process data samples through API
  ntext <- eventReactive(input$process, {
    
    req(input$file1)
      
    # Load csv file
    df <- read.csv(input$file1$datapath,stringsAsFactors = F)
    
    if (input$header == T){
    df <- read.csv(input$file1$datapath,
                   sep = input$sep,
                   quote = input$quote,stringsAsFactors = F)
    } else {
      df <- read.csv(input$file1$datapath,
                     sep = input$sep,
                     quote = input$quote,stringsAsFactors = F, header = F)
    }
    
    # Fix column names
    if (ncol(df) == 2){
      names(df)[1:2] <- c("request_id","content")
    } else if (ncol(df)==1){
      df <- cbind(1:nrow(df),df)
      names(df)[1:2] <- c("request_id","content")
    }
    
    df$request_id <- as.character(df$request_id)  
    df$content  <- as.character(df$content)
    
    # Clean-up content
    df$content <- gsub("\n"," ",df$content)
    df$content <- gsub("&amp;","&",df$content)
    df$content <- gsub("&gt;",">",df$content)
    df$content <- gsub("&lt;","<",df$content)
    df$content <- gsub("\\[.*\\]","",df$content)
    df$content <- gsub("\xd5","'",df$content)
    df$content <- gsub("\xfc\xbe\x8c\x96\x90\xbc"," - ",df$content)
    df$content <- gsub("\xfc\xbe\x8c\x86\x90\xbc"," - ",df$content)
    df$content <- gsub("\xfc\xbe\x8c\xa6\x90\xbc",'\\"' ,df$content)
    df$content <- gsub("\xfc\xbe\x8c\xb6\x90\xbc",'\\" ' ,df$content)
    df$content <- gsub("\xfc\xbe\x8e\x96\x8c\xbc",'\\" ' ,df$content)
    df$content <- gsub("  ",' ' ,df$content)
    
    # Force encoding to UTF-8
    Encoding(df$content) <- "UTF-8"
    df$content <- iconv(df$content, "UTF-8", "UTF-8",sub='')
    
    # Grab API keys
    api_key <- as.character(input$apikey)
    api_secret <- as.character(input$apisecret)
    
    # Set counter for bulk scoring
    x <- floor(nrow(df)/1000)
    
    # Set error message flag
    error_msg <- "N"
    
    s <- lapply(1:(x+1),function(j){
      print(j/x)
      if (j != x+1){
        temp <- df[((j-1)*1000+1):(j*1000),]
      } else {
        temp <- df[((j-1)*1000+1):nrow(df),]
        
      }
      
      if (exists("temp")){
        # If more than one sample of text, convert into list format
        d <- lapply(1:nrow(temp),function(i){
          body=list(
            request_id=temp$request_id[i],
            content=temp$content[i],
            context=0,
            language='en'
          )
          return(body)
        })
        body <- do.call(list,d)
        
        # push to API
        
        # Dev API
        response = POST("https://api.receptiviti.com/v1/score/bulk",
                        authenticate(api_key, api_secret),
                        body=body,
                        add_headers('Content-Type'='application/json'),
                        encode='json')
        
        scores <- httr::content(response, as='parsed')
        
        # Check if API keys ok
        if (response$status_code == 200){
          scores <- scores$results
          
          return(scores)
          
        } else if (scores$message == "Unrecognized API key pair."){
          # If dodgy API keys, throw error
          error_msg <<- "Y"
          
        } else {
          
          # Otherwise provide HTTP error code
          showNotification(ui = paste0("HTTP ",response$status_code," Error"), type="error",id="0",duration=NULL)
          
        }
        
      } else {
        NULL
      }
    })

    if (error_msg == "N"){
      r <- lapply(1:length(s),function(j){
        print(j/length(s))
        scores <- s[[j]]
        
        # Convert to data frame
        d <- lapply(1:length(scores),function(i){
          temp <- scores[[i]]
          
          h <- as.data.frame(t(unlist(temp[1:4])))
          
          summary <- as.data.frame(t(unlist(temp$summary)))
          
          liwc <- as.data.frame(t(unlist(temp$dictionary_measures)))
          names(liwc) <- paste0("dictionary_measures.",names(liwc))
          
          recept <- as.data.frame(t(unlist(temp$receptiviti)))
          names(recept) <- paste0("receptiviti_measures.",names(recept))
          
          output <- as.data.frame(cbind(h,summary,liwc,recept),stringsasfactors=F)  
          return(output)
        })
        output <- as.data.frame(rbindlist(d,use.names=T,fill=T))
        return(output)
      })
      scores.df <- rbindlist(r,use.names=T,fill=T)
      
      
      # Activate download button
      output$button2 <- renderUI({
        downloadButton("downloadData", "Download")
      })
      
      
      return(scores.df)
    } else {
      
      # Throw an error if dodgy API keys
      showNotification(ui = "Invalid API or secret key. Please check your credentials.", type="error",id="0",duration=NULL)
      
    }
    
    
  })
  
  
  # Download results to csv file
  output$downloadData <- downloadHandler(
    filename = function() {
      "receptiviti.csv"
    },
    content = function(file) {
      write.csv(ntext(), file, row.names=F)
    },
    contentType="text/csv"
  )
  
}

# Create Shiny app ----
shinyApp(ui, server)
