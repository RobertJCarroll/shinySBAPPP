# Customizable summary tool for SBA PPP Data

library(DT)
library(shiny)
library(tidyverse)
library(fuzzyjoin)


##Loan Ranges
#Create loan size ranges:
loan_range = data.frame(LoanRange=c("a $5-10 million","b $2-5 million","c $1-2 million","d $350,000-1 million","e $150,000-350,000"),
                      MinValue=c(5e6,2e6,1e6,3.5e5,1.5e5),MaxValue=c(10e6,5e6,2e6,1e6,3.5e5))
loan_range = loan_range %>% mutate(MeanValue=(MinValue+MaxValue)/2)


#Read it in
naics=readxl::read_xlsx("all_data/naics.xlsx",col_names = c("NAICSCode","NAICSTitle"),skip=2,col_types=c("numeric","text","skip")) %>% select(NAICSCode, NAICSTitle)

##Load the 150k+ data
big_loans = read_csv("all_data/PPP Data 150k plus 080820.csv",col_types="cc__cnnc___cn___")
big_values = left_join(inner_join(big_loans,loan_range),naics) %>% mutate(CustomLabel="",rowid=1:n())
rm(big_loans)

## Prep lists
states=unique(big_values$State)
base_classes=data.frame(priority=1:11,
                        regex=c("SCHOOL|EDUCATION|ACADEMY","SERVICE","PARISH","CHARIT","CHUR|CATHEDRAL|CONGREGATION","BISHOP|DIOCESE","CEMETER","MISSION","COMMUNITY","SEMINA",".*"),
                        CustomLabel=c("School(s)","Services","Parish","Charity","Parish","Bishop / Diocese","Cemetery","Mission","Community","Seminary","Other"))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Summary of SBA PPP Data"),
   
   # Sidebar with a select input for type of value used
   sidebarLayout(
      sidebarPanel(
        textInput(inputId = "text_filter",value = "CATHOLIC", label = "Custom text filter"),
        checkboxInput("just_rows","Just show rows? (Overrides the summarize option below)", 
                      value=FALSE),
        selectInput("groups", 
                    label = "Summarize by",
                    choices = c("State","NAICSTitle","CustomLabel"),
                    selected = NULL,
                    multiple = T),
        selectInput("ncode", 
                    label = "Filter by NAICS code(s)",
                    choices = naics$NAICSTitle,
                    selected = NULL,
                    multiple=T),
        selectInput("state", 
                    label = "Filter by State(s)",
                    choices = states,
                    selected = NULL,
                    multiple=T),
        
        checkboxInput("ignore_custom","Ignore custom labels? (Overrides the below options- helps speed without the text filter)", 
                      value=FALSE),
        
        selectInput("custom_filter", 
                    label = "Filter by custom class list",
                    choices = base_classes$CustomLabel,
                    selected = NULL,
                    multiple=T),
        
        DTOutput(outputId = "custom_classes")
        
      ),
      
      # Show a plot of the map
      mainPanel(
        DTOutput('tbl') 
      )
   )
)

# Define server logic 
server <- function(input, output, session) {
  my_classes=base_classes
  
  my_big_values=big_values
  
  currency_cols = c("Total Mean Value")
  
  output$tbl <- renderDT(datatable({
    ncode=input$ncode
    if(is.null(ncode)) {
      ncode=naics$NAICSTitle
    }
    state=input$state
    if(is.null(state)) {
      state=states
    }
    custom_filter=input$custom_filter
    if(is.null(custom_filter)) {
      custom_filter=my_classes$CustomLabel
    }
    
    
    # Assign custom labels and do all filters
    my_big_values = big_values %>% select(-CustomLabel) %>% filter(NAICSTitle %in% ncode, State %in% state, grepl(pattern = input$text_filter,BusinessName))
    if(input$ignore_custom) {
      my_big_values %>% mutate(CustomLabel="")
    }
    else {
      my_big_values = my_big_values %>% 
      regex_left_join(my_classes, by=c(BusinessName = "regex")) %>% group_by(rowid) %>% slice_min(order_by = priority, n = 1) %>% 
      ungroup() %>% select(-regex,-priority) %>% filter(CustomLabel %in% custom_filter)
    }
    
    if(input$just_rows) {
      currency_cols = c("MeanValue")
      my_big_values %>% select(BusinessName,MeanValue,State,NAICSTitle,CustomLabel, BusinessType,LoanRange, JobsReported)  %>% 
        arrange(-`MeanValue`)
    } else{
      currency_cols = c("Total Mean Value")
      my_big_values  %>% 
        group_by(across(all_of(input$groups))) %>% 
        summarize(`Total Mean Value`=sum(MeanValue),`N Loans`=n()) %>% arrange(-`Total Mean Value`)
    }
  }) %>% formatCurrency(columns=currency_cols),
  server = FALSE) 
  
  #Present the custom classes
  output$custom_classes <- renderDT(my_classes,
    server = TRUE, editable = "row", rownames=FALSE) 
  
  #On changes to the classes, update the ui selection box
  observeEvent(input$custom_classes_cell_edit, {
    my_classes <<- editData(my_classes, input$custom_classes_cell_edit,proxy = "custom_classes", rownames = FALSE)
    updateSelectInput(session, "custom_filter",
                     choices = my_classes$CustomLabel, selected = NULL)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

