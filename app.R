#----- PACP Angel Database Manager -----

#---- libraries ----
library(shiny)
library(tidyverse)
library(bslib)
library(thematic)
library(googlesheets4)


#---- data ----
gs4_deauth()

angels_gsheet <- source("sheet_id.R")$value

angels <- range_read(angels_gsheet, sheet = "Angels")
funds <- range_read(angels_gsheet, sheet = "Funds")

angels_sector_names <- colnames(angels[5:ncol(angels)])
angels_names <- pull(angels, Name)

funds_sector_names <- unique(funds$Sector)

#---- modules ----
source("modules/investor_table.R")
source("modules/select_sector.R")
source("modules/investor_search.R")
source("modules/add_investor.R")
source("modules/funds_table.R")


#---- UI ----
pacp_logo <- tags$img(src = "logo.png", height = "80px")

ui <- page_navbar(
  useShinyjs(),

  nav_item(pacp_logo),

  theme = bs_theme(
    "navbar-bg" = "#ffffff",
    fg = "#ffffff",
    bg = "#0c2c54",
    primary = "#f9c647",
    secondary = "#f9c647",
    base_font = font_google("Jost")
  ),

  fillable = T,

  nav_panel(title = "Angel Investors",
            navset_tab(
              nav_panel(
                title = "Search by Sector",
                select_sector_UI("select_sector_module", sector_names = angels_sector_names)
              ),

              nav_panel(
                title = "All Angels",
                investor_table_UI("investor_table_module")
              ),

              nav_panel(
                title = "Similar Investor Search",
                investor_search_UI("investor_search_module", angels_names)
              ),

              nav_panel(
                title = "Add an Investor",
                add_investor_UI("add_investor_module", sector_names = angels_sector_names)
              )
            )
  ),

  nav_panel(title = "Investment Funds",
            funds_table_UI("funds_table_module", sector_names = funds_sector_names)
            )

)


#---- server ----
server <- function(input, output, session){
  select_sector_server("select_sector_module", angel_data = angels)

  investor_table_server("investor_table_module", angel_data = angels)

  investor_search_server("investor_search_module", angel_data = angels)

  add_investor_server("add_investor_module", angel_data = angels, angels_gsheet = angels_gsheet)

  funds_table_server("funds_table_module", funds_data = funds)
}


#---- create app ----
shinyApp(ui, server)
