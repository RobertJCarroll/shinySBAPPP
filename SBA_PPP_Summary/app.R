# First draft shiny app. 

library(DT)
library(shiny)
library(tidyverse)

##Loan Ranges
#Create loan size ranges:
loan_range = data.frame(LoanRange=c("a $5-10 million","b $2-5 million","c $1-2 million","d $350,000-1 million","e $150,000-350,000"),
                        MinValue=c(5e6,2e6,1e6,3.5e5,1.5e5),MaxValue=c(10e6,5e6,2e6,1e6,3.5e5))
loan_range = loan_range %>% mutate(MeanValue=(MinValue+MaxValue)/2)


#Read it in
naics=readxl::read_xlsx("../SBA_PPP_Custom_Summary/all_data/naics.xlsx",col_names = c("NAICSCode","NAICSTitle"),skip=2,col_types=c("numeric","text","skip")) %>% select(NAICSCode, NAICSTitle)

##Load the 150k+ data
big_loans = read_csv("../SBA_PPP_Custom_Summary/all_data/PPP Data 150k plus 080820.csv",col_types="cc__cnnc___cn___")
big_values = left_join(inner_join(big_loans,loan_range),naics) %>% mutate(CustomLabel="",rowid=1:n())
rm(big_loans)

## Prep lists
states=unique(big_values$State)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Summary of SBA PPP Data"),
   
   # Sidebar with a select input for type of value used
   sidebarLayout(
      sidebarPanel(
        helpText("View state totals."),
        selectInput("groups", 
                       label = "Summarize by",
                       choices = c("State","NAICSTitle"),
                    selected = "State",
                    multiple = T),
        selectInput("ncode", 
                    label = "Filter by NAICS code(s)",
                    choices = c("<<ALL>>",naics$NAICSTitle),
                    multiple=T),
        selectInput("state", 
                       label = "Filter by State(s)",
                       choices = c("<<ALL>>",states),
                       multiple=T)
        
      ),
      
      # Show a plot of the map
      mainPanel(
        DTOutput('tbl') 
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$tbl <- renderDT(datatable({
    ncode=input$ncode
    if("<<ALL>>" %in% ncode) {
      ncode=naics$NAICSTitle
    }
    state=input$state
    if("<<ALL>>" %in% state) {
      state=states
    }
    #Create the totals used for % calculations
    #state_totals = big_values %>% group_by(State) %>% summarize(`StateTotalMean`=sum(MeanValue),`N Loans`=n())
    big_values %>% filter(NAICSTitle %in% ncode, State %in% state) %>% group_by(across(all_of(input$groups))) %>% 
      summarize(`Total Mean Value`=sum(MeanValue),`N Loans`=n()) %>% arrange(-`Total Mean Value`)
  }) %>% formatCurrency("Total Mean Value"),
  server = FALSE) 

}

# Run the application 
shinyApp(ui = ui, server = server)

