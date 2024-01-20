#---- li_link ----
li_link <- function(URL){
  if (!is.na(URL)){
    a <- tags$a(href = as.character(URL), "LinkedIn")
    paste(a)
  } else if (is.na(URL)){
    a <- ""
    paste(a)
  }
}
