#---- similarity_matrix ----
similarity_matrix <- function(angel_data){
  angel_data <- column_to_rownames(angel_data, "Name") # move names of investors to rownames so that they are saved in the dist matrix
  angel_data <- angel_data[4:ncol(angel_data)] # just keep the sectors
  
  angel_dist_matrix <- dist(angel_data) # create dist matrix
  
  return(angel_dist_matrix)
}