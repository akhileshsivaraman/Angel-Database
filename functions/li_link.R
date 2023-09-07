#---- li_link ----
li_link <- function(URL){
  a <- tags$a(href = as.character(URL), "LinkedIn")
  paste(a)
}
