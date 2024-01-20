#---- li_link ----
li_link <- function(URL){
  a <- tags$a(href = as.character(URL), "LinkedIn")
  paste(a)
}

# can we add an if statement so it shows blank if there is no li link in the table
