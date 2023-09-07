#---- common_investors ----
common_investors <- function(sector_tibbles, angel_data){ # sector tibbles is a list of tibbles
  x <- intersect(sector_tibbles[[1]][[1]], sector_tibbles[[2]][[1]]) # take the first intersection in the list
  
  if (length(sector_tibbles) > 2){ # if the length of the list is > 2 (i.e. there are more than 2 sectors)
    for (i in 3:length(sector_tibbles)){ # iterate over the list beginning at item 3
      x <- intersect(x, sector_tibbles[[i]][[1]]) # find the intersect of the next item and the previously computed intersect
    }
  }
  
  y <- angel_data |>
    filter(Name %in% x) |>
    select(Name, LinkedIn, Email, Country)
  
  return(y) # returns table of investors
}