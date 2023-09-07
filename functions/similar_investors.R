#---- similar_investors ----
similar_investors <- function(selected_investor, n = 10, angel_dist_matrix, angel_data){
  n <- as.integer(n)
  
  angel_matrix <- as.matrix(angel_dist_matrix) # convert dist class matrix to matrix so it can be manipulated
  angel_matrix_tibble <- as_tibble(angel_matrix) # convert to tibble
  angel_matrix_tibble <- rownames_to_column(as.data.frame(angel_matrix), "Investors") # add names to tibble
  
  similar_investors_vector <- angel_matrix_tibble |>
    arrange(.data[[selected_investor]]) |>
    select(Investors, all_of(selected_investor)) |>
    slice(c(2:(n+1))) |>
    pull(Investors)
  
  similar_investors_table <- angel_data |>
    filter(Name %in% similar_investors_vector) |>
    select(1:4)
  
  return(similar_investors_table)
}