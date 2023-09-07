#----- select_sector -----

#---- libraries ----
library(shiny)
library(bslib)
library(tidyverse)


#---- functions ----
source("functions/common_investors.R")
source("functions/search_by_sector.R")


#---- select_sector_UI ----
select_sector_UI <- function(id, sector_names = NULL){
  layout_sidebar(
    fillable = T,

    sidebar = sidebar(
      width = "25%",
      selectInput(NS(id, "selected_sector"),
                  label = "Select a sector",
                  choices = sector_names,
                  selectize = T,
                  multiple = T,
                  selected = "Agnostic"),

      actionButton(NS(id, "button"),
                   label = "Find investors"),

      helpText("Select a sector to find investors interested in that sector. (Note: you can also select multiple sectors to find investors who meet more than one of your requirements)")
      ),

    h2("Search for investors by sector"),

    card(DT::dataTableOutput(NS(id, "investors_table")), fill = T)
  )
}


#---- select_sector_server ----
select_sector_server <- function(id, angel_data){
  moduleServer(id, function(input, output, session){

    investors <- eventReactive(input$button, {
      if (length(input$selected_sector) == 1){
        search_by_sector(input$selected_sector, angel_data) # check what the output is, if tibble do the li_link in ifs
      } else if (length(input$selected_sector) > 1){
        sector_tibbles <- map(input$selected_sector, search_by_sector, angel_data)
        common_investors(sector_tibbles, angel_data)
      }
    }, ignoreNULL = F)

    output$investors_table <- DT::renderDataTable({ # return a table
      investors()
    }, options = list(pageLength = 15), rownames = F, fillContainer = T)

  })
}


#---- select_sector_app ----
select_sector_app <- function(){
  ui <- page_fluid(
    select_sector_UI("select_sector", sector_names)
  )

  server <- function(input, output, session){
    select_sector_server("select_sector", angels)
  }

  shinyApp(ui, server)
}
