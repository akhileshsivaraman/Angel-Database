#---- sectors_table ----
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