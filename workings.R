library(tidyverse)
library(DT)
library(googlesheets4)

#---- read and transform data ----
# angels <- read_csv("data/Angel Investor Database.csv") |>
#   mutate(across(Aerospace:Web3, ~replace_na(., 0))) |>
#   mutate(across(Aerospace:Web3 & where(is.numeric), as.logical))
# googlesheets4

angels <- range_read("1AkQVxmEU95mQPDQ7KjS-ckjrzVnZvLKYRInk9dFBDik") |>
  mutate(across(Aerospace:Web3, ~replace_na(., 0))) |>
  mutate(across(Aerospace:Web3 & where(is.numeric), as.logical))

angels_gsheet <- "1AkQVxmEU95mQPDQ7KjS-ckjrzVnZvLKYRInk9dFBDik"

sector_names <- colnames(angels[5:ncol(angels)])


#---- full data set ----
sectors_table <- function(angel_data){
  a <- angel_data[5:ncol(angel_data)] # take sector columns
  z <- angel_data[1:4] # take investor details columns
  y <- tibble() # create a new tibble to bind data to later and return
  
  for (i in 1:nrow(a)){ # loop over the rows of the angel_data tibble
    b <- map(a[i,], isTRUE) # run isTRUE on a row of the angel_data tibble
    investor_sectors <- c() # create an empty vector to store data
    
    for (j in 1:length(b)){ # loop over the list named b
      if (b[[j]] == TRUE){ # if the value of the element is TRUE
        investor_sectors <- c(investor_sectors, names(b)[j]) # take the name of the element and append it to the investor_sectors vector
      }
    }
    
    sectors <- paste(investor_sectors, collapse = ", ") # collapse the investor_sectors vector into a length of 1
    x <- cbind(z[i,], sectors) # bind the sectors to the investor's details
    
    y <- rbind(y, x) # add the constructed row to the tibble
  }
  
  return(y)
}


#---- search by sector ----
search_by_sector <- function(sector, angel_data) { # use dropdown menu for sector input
  x <- angel_data |>
    filter(.data[[sector]] == TRUE) |> # take sector name and search for TRUE in that column
    select(Name, LinkedIn, Email, Country) # return names and details
  
  return(x)
}

# search by multiple sectors
s <- c("B2B SaaS", "FinTech", "Tech") # vector of sectors
v <- map(s, search_by_sector, angel_data = angels) # iterate over vector of sectors with search_by_sector
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
u <- common_investors(v, angels)



#---- search for similar investors ----
# produce matrix
similarity_matrix <- function(angel_data){
  angel_data <- column_to_rownames(angel_data, "Name") # move names of investors to rownames so that they are saved in the dist matrix
  angel_data <- angel_data[4:ncol(angel_data)] # just keep the sectors
  
  angel_dist_matrix <- dist(angel_data) # create dist matrix
  
  return(angel_dist_matrix)
}
# identify similar investors
similar_investors <- function(selected_investor, angel_dist_matrix){
  angel_matrix <- as.matrix(angel_dist_matrix) # convert dist class matrix to matrix so it can be manipulated
  angel_matrix_tibble <- as_tibble(angel_matrix) # convert to tibble
  angel_matrix_tibble <- rownames_to_column(as.data.frame(angel_matrix), "Investors") # add names to tibble
  
  similar_investor_table <- angel_matrix_tibble |>
    arrange(.data[[selected_investor]]) |>
    select(Investors, all_of(selected_investor)) |>
    slice(c(2:6))
  
  return(similar_investor_table)
}

#---- add an angel ----
# investor_name <- "akhilesh"
# investor_li <- "URL"
# investor_email <- "email"
# investor_country <- "France"
# investor_sectors <- c("Aerospace", "Agnostic", "AgriTech")

add_investor <- function(investor_name, investor_li, investor_email, investor_country, investor_sectors, angel_data, angels_gsheet){
  # verify the angel investor doesn't already exist in the database
  exists_already <- angel_data |>
    filter(Name == investor_name)
  
  if(nrow(exists_already) > 0){
    print("This investor already exists in the database")
    
  } else { # add investor if they do not exist in the database
    a <- angel_data |>
      add_row(Name = investor_name,
              LinkedIn = investor_li,
              Email = investor_email,
              Country = investor_country) |>
      filter(Name == investor_name) |>
      mutate(across(investor_sectors, ~ 1),
             across(5:ncol(angel_data), ~replace_na(., 0)))
    
    angels_gsheet |>
      sheet_append(a)
  }
}

# may need to split the function up
# then write a function for the else
# and write the if statement in the module itself

investor_name <- "Simon Murdoch"
x <- add_investor(investor_name, investor_li, investor_email, investor_country, investor_sectors, angels, angels_gsheet)
