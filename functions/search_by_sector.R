#---- search_by_sector ----
search_by_sector <- function(sector, angel_data) { # use dropdown menu for sector input
  x <- angel_data |>
    filter(.data[[sector]] == TRUE) |> # take sector name and search for TRUE in that column
    select(Name, LinkedIn, Email, Country) # return names and details
  
  return(x)
}