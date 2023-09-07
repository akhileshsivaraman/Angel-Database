#----- add_investor.R -----

#---- libraries ----
library(shiny)
library(bslib)
library(tidyverse)
library(shinyjs)


#---- function ----
source("functions/add_investor.R")


#---- global ----
mandatory_fields <- c("investor_name", "investor_sectors")
label_mandatory <- function(label){
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}


#---- CSS ----
appCSS <- ".mandatory_star {color: red;}"


#---- add_investor_UI ----
add_investor_UI <- function(id, sector_names = NULL){
  tagList(
    useShinyjs(),
    inlineCSS(appCSS),
    
    h2("Add an angel investor to the database"),
    
    textInput(NS(id, "investor_name"),
              label = label_mandatory("Investor name"),
              placeholder = "Greg Tufnell"),
    
    textInput(NS(id, "investor_li"),
              label = "Investor's LinkedIn URL",
              placeholder = "https://www.linkedin.com/in/greg-tufnell-gt1/"),
    helpText("If the investor is not on LinkedIn or their profile is inactive, this can be left blank."),
    
    textInput(NS(id, "investor_email"),
              label = "Investor's email address",
              placeholder = "greg@pa-capitalpartners.com"),
    helpText("If the investor's email is unknown, this can be left blank."),
    
    textInput(NS(id, "investor_country"),
              label = "The country in which the investor lives",
              placeholder = "UK"),
    
    selectInput(NS(id, "investor_sectors"),
                label = label_mandatory("The investors' sectors of interest"),
                choices = sector_names,
                selectize = T,
                multiple = T),
    helpText("You can select multiple sectors!"),
    
    br(),
    
    div(
      actionButton(NS(id, "add_investor_button"),
                   label = "Add investor"),
      
      hidden(div(NS(id, "loading_spinner"),
                 icon("spinner"),
                 class = "fa-spin")
             ),
      
      uiOutput(NS(id, "error_message"))
    )
  )
}


#---- add_investor_server ----
add_investor_server <- function(id, angel_data, angels_gsheet){
  moduleServer(id, function(input, output, session){
    
    # ensure mandatory fields are filled in before being able to submit
    observe({
      mandatory_filled <- vapply(mandatory_fields,
                                 function(x){
                                   !is.null(input[[x]]) && input[[x]] != ""
                                 },
                                 logical(1))
      
      mandatory_filled <- all(mandatory_filled)
      
      toggleState(id = "add_investor_button", condition = mandatory_filled)
    })
    
    
    exists_already <- eventReactive(input$add_investor_button, {
      angels <- range_read("1AkQVxmEU95mQPDQ7KjS-ckjrzVnZvLKYRInk9dFBDik") |> #
        mutate(across(Aerospace:Web3, ~replace_na(., 0))) |>
        mutate(across(Aerospace:Web3 & where(is.numeric), as.logical)) |>
        filter(Name == input$investor_name)
    })
    
    
    output$error_message <- renderUI({
      if(nrow(exists_already()) > 0){
        div("This investor already exists in the database")  
      }
    })
    
    
    observeEvent(exists_already(), {
      if(nrow(exists_already()) == 0){
        add_investor(investor_name = input$investor_name,
                     investor_li = input$investor_li,
                     investor_email = input$investor_email,
                     investor_country = input$investor_country,
                     investor_sectors = input$investor_sectors,
                     angel_data = angel_data,
                     angels_gsheet = angels_gsheet)
      }
    })
    
    
    observeEvent(input$add_investor_button, {
      disable(id = "add_investor_button")
      show(id = "loading_spinner")
      delay(5000, {
        enable(id = "add_investor_button")
        hide(id = "loading_spinner")
      })
    })
  })
}


#---- add_investor_app ----
add_investor_app <- function(){
  ui <- page_fluid(
    add_investor_UI("add_investor", sector_names)
  )
  
  server <- function(input, output, session){
    add_investor_server("add_investor", angels, angels_gsheet)
  }
  
  shinyApp(ui, server)
}

add_investor_app()
