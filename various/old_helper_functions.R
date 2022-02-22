# Function useful for recoding all values of a column. The dataframe to join NEED to have just 1 column outside of keys!
# Condition specified as ex:c("a" = "b")
left_join_and_substitute <- function(df1, df2, old_name = T, by.cond = NULL) {
  
  if (is.null(by.cond)){ # If we have the same key on the two dfs
    # Find the name of the column with the new values and key for which to join
    new_values_var <- setdiff(names(df2), names(df1))
    old_name_var <- intersect(names(df2), names(df1))
    finaldf <- df1 %>%
      left_join(df2)
    
    # Join and substitute the value of the key with the values of the column of the joined df.
    # Drop the duplicate column.
    helper_left_join_and_substitute(finaldf, old_name, {{new_values_var}}, {{old_name_var}})
    
  } else {
    # Find the name of the column with the new values and key of df1 for which to join
    new_values_var <- setdiff(names(df2), by.cond[[1]])
    old_name_var <- names(by.cond)
    finaldf <- df1 %>%
      left_join(df2, by = by.cond)
    
    helper_left_join_and_substitute(finaldf, old_name, {{new_values_var}}, {{old_name_var}})
  }
}

helper_left_join_and_substitute <- function(df_temp, keep_name, new_var, old_var) {
  # Join and substitute the value of the key with the values of the column of the joined df.
  # Drop the duplicate column.
  if (keep_name){ #Keep the original variable name
    df_temp %>%
      mutate({{old_var}} := eval(parse(text = new_var)))%>%
      select(-new_var)
  } else { #Use df2 variable name
    df_temp %>%
      mutate({{new_var}} := eval(parse(text = new_var)))%>%
      select(-old_var)
  }
}

test1 <- tibble(x = 1:3, y = list(1:5, 1:10, 1:20), z = c(NA, "a", ""))
test2 <- tibble(a = c("a", "b", "c"), t = list(1:5, 1:10, 1:20))
test <- left_join_and_substitute(test1, test2, F, by = c("y" = "t"))
test





create_cell_attributes <- function(cstyles, astyles) {
  temp_df <- tribble(
    ~cell_name, ~cell_style,~label,~tags,~link,~placeholders,~tooltip,~shape,~x,~y,~width,~height,~as,
    #---------|------------|------|-----|-----|-------------|--------|------|--|--|------|-------|---|
           "0",     "empty",    "",   "",   "",           "",      "",    "","","",    "",     "", "",
           "1",     "start",    "",   "",   "",           "",      "",    "","","",    "",     "", ""
  )
}

test_fun <- function(...) {
  element_list <- list(...)
  temp_df <- create_cell_attributes()
  for (i in element_list) {
    print(deparse(substitute(element_list[i])))
    temp_df <- temp_df %>%
      add_row(as_tibble(element_list[i]))
  }
  print(temp_df)
  names(element_list)
}

test_fun(
  cella1 = list(cell_style = "orange", label = "cella_arancione", tags = "cell1", link = "",
              tooltip = "hey, this is the first tooltip", shape = "oval",
              x = "80", y = "120", width = "120", height = "60", as = "geometry"),
  
  list(cell_style = "yellow", label = "cella_gialla", tags = "cell2", link = "",
              tooltip = "Now is the second tooltip", shape = "oval",
              x = "320", y = "40", width = "120", height = "60", as = "geometry")
)

test <- c("a" = 1, "b" = "c")
c(test, ayeye" = 99)
