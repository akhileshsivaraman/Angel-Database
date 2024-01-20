#----- funds_table -----

#---- libraries ----
library(shiny)
library(bslib)
library(tidyverse)


#---- funds_table_UI ----
funds_table_UI <- function(id, sector_names = NULL){
  tagList(
    p("Browse through all the investment funds in the PACP database and filter by sector of interest using the drop down"),

    selectInput(NS(id, "selected_sector"),
                label = "Select a sector",
                choices = sector_names,
                selectize = T,
                multiple = F,
                selected = "AI"),

    actionButton(NS(id, "button"),
                 label = "Search"),

    card(DT::dataTableOutput(NS(id, "funds_data")), fill = T)
  )
}

#---- funds_table_server ----
funds_table_server <- function(id, funds_data){
  moduleServer(id, function(input, output, session){

    funds_selected_sector <- eventReactive(input$button, {
      x <- funds_data |>
        filter(Sector == input$selected_sector)
      y <- map(x$LinkedIn, li_link)
      x$LinkedIn <- y
      return(x)
    }, ignoreNULL = F)

    output$funds_data <- DT::renderDataTable({
      funds_selected_sector()
    }, options = list(pageLength = 25), rownames = F, fillContainer = T, escape = F)
  })
}

#---- funds_table_app -----
funds_table_app <- function(){
  ui <- page_fluid(
    funds_table_UI("funds_table", sector_names = funds_sector_names)
  )

  server <- function(input, output, session){
    funds_table_server("funds_table", funds_data = funds)
  }

  shinyApp(ui, server)
}

funds_table_app()
