#%%
library(shiny)
library(shinythemes)
library(lubridate)
library(dygraphs)
library(xts)
library(tidyverse)

#%%
bitcoin <-read.csv(file = 'BTC-USD.csv', stringsAsFactors = F)
bitcoin$Date <- ymd(bitcoin$Date)

#%%
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  # App title ----
  titlePanel(strong("Bitcoin Dashboard")),
  
  sidebarLayout(
    sidebarPanel(
      h4(strong("Bitcoin closing prices 2014-2022")),
      br(),
      selectInput('selectOutput', 
                  'Select output', 
                  choices = colnames(bitcoin)[c(2,3,4,5,7)]),
      dateRangeInput('selectDate', 
                     'Select date', 
                     start = min(bitcoin$Date), 
                     end = max(bitcoin$Date)),
      br(),
      br(),
      checkboxInput("logInput", "Plot y axis on log scale", value = FALSE),
      
    ),
    
    mainPanel(dygraphOutput("priceGraph", width = "100%", height = "800px"))
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  getData <- reactive({
    
    # get inputs 
    selectedOutput <- input$selectOutput
    startDate <- input$selectDate[1]
    endDate <- input$selectDate[2]
    
    
    # filter data
    data <- bitcoin %>%
      select(Date, selectedOutput) %>%
      filter(Date >= startDate & Date <= endDate)
    
    
    # formatting in case of market cap
    if(selectedOutput == "Volume"){
      data["Volume"] <- lapply(data["Volume"], FUN = function(x) x/1000000000)
    }
    
    
    # converting to logscale for bitcoin price
    if(input$logInput == TRUE & input$selectOutput != "Volume"){
      data[selectedOutput] = log(data[selectedOutput])
      
    }
    
    data
  })
  
  output$priceGraph <- renderDygraph({
    data <- getData()
    time_series <- xts(data, order.by = data$Date)
    dygraph(time_series)
  })
  
}




# Run the application 
shinyApp(ui = ui, server = server)