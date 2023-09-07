#----- investor_table -----

#---- libraries ----
library(shiny)
library(bslib)
library(tidyverse)


#---- functions ----
source("functions/sectors_table.R")
source("functions/li_link.R")

#---- investor_table_UI ----
investor_table_UI <- function(id){
  tagList(
    p("Browse through all the angel investors in the PACP database"),
    card(DT::dataTableOutput(NS(id, "full_data")), fill = T)
  )
}


#---- investor_table_server ----
investor_table_server <- function(id, angel_data){
  moduleServer(id, function(input, output, session){
    output$full_data <- DT::renderDataTable({
      x <- sectors_table(angel_data)
      y <- map(x$LinkedIn, li_link)
      x$LinkedIn <- y
      return(x)
    }, options = list(pageLength = 50), rownames = F, fillContainer = T, escape = F)
  })
}


#---- investor_table_app ----
investor_table_app <- function(){
  ui <- page_fluid(
    investor_table_UI("investor_table")
  )

  server <- function(input, output, session){
    investor_table_server("investor_table", angels)
  }

  shinyApp(ui, server)
}

investor_table_app()
