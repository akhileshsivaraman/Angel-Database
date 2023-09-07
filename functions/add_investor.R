#---- add_investor ----
# add_investor <- function(investor_name, investor_li, investor_email, investor_country, investor_sectors, angel_data, angels_gsheet){
#   # verify the angel investor doesn't already exist in the database
#   exists_already <- angel_data |>
#     filter(Name == investor_name)
#   
#   if(nrow(exists_already) > 0){
#     print("This investor already exists in the database")
#     
#   } else { # add investor if they do not exist in the database
#     a <- angel_data |>
#       add_row(Name = investor_name,
#               LinkedIn = investor_li,
#               Email = investor_email,
#               Country = investor_country) |>
#       filter(Name == investor_name) |>
#       mutate(across(investor_sectors, ~ 1),
#              across(5:ncol(angel_data), ~replace_na(., 0)))
#     
#     angels_gsheet |>
#       sheet_append(a)
#   }
# }

add_investor <- function(investor_name, investor_li, investor_email, investor_country, investor_sectors, angel_data, angels_gsheet){
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
