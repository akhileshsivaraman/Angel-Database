#----- investor_search -----

#---- libraries ----
library(shiny)
library(bslib)
library(tidyverse)


#---- functions ----
source("functions/similarity_matrix.R")
source("functions/similar_investors.R")


#---- investor_search_UI ----
investor_search_UI <- function(id, angels_names = NULL){
  layout_sidebar(
    fillable = T,
    
    sidebar = sidebar(
      width = "25%",
      selectInput(NS(id, "selected_investor"),
                  label = "Select an investor",
                  choices = angels_names,
                  selectize = T,
                  multiple = F),
      
      numericInput(NS(id, "number_to_find"),
                   label = "How many similar investors would you like to find?",
                   value = 10,
                   min = 1, 
                   max = length(angels_names),
                   step = 1
                   ),
      
      actionButton(NS(id, "investor_search_button"),
                   label = "Search"),
      
      helpText("Select an investor and how many similar investors you would like to find.")
    ),
    
    h2("Search for investors similar to one of interest"),
    
    card(DT::dataTableOutput(NS(id, "similar_investors_table"), fill = T)),
    
    p("The more similar an investor is to the one you selected, the higher up in the table they will be listed. Note: similarity is measured as the number of sectors of interest shared between two investors.")
  )
}

#---- investor_search_server ----
investor_search_server <- function(id, angel_data){
  moduleServer(id, function(input, output, session){
    
    angel_dist_matrix <- similarity_matrix(angel_data)
    
    similar_investors_table_reactive <- eventReactive(input$investor_search_button,{
      similar_investors(selected_investor = input$selected_investor,
                        n = input$number_to_find,
                        angel_dist_matrix = angel_dist_matrix,
                        angel_data = angel_data)
    })
    
    output$similar_investors_table <- DT::renderDataTable({
      similar_investors_table_reactive()
    }, rownames = F, fillContainer = T)
  })
}


#---- investor_search_app ----
investor_search_app <- function(){
  ui <- page_fluid(
    investor_search_UI("investor_search", angels_names)
  )
  
  server <- function(input, output, session){
    investor_search_server("investor_search", angels)
  }
  
  shinyApp(ui, server)
}
