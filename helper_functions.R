# Function useful for recoding all values of a column. The dataframe to join NEED to have just 1 column outside of keys!
# Condition specified as ex:c("a" = "b")
left_join_and_substitute <- function(df1, df2, old_name = T, by.cond = NULL) {
  if (is.null(by.cond)){ # If we have the same key on the two dfs
    # Find the name of the column with the new values and key for which to join
    new_values_var <- setdiff(names(df2), names(df1))
    old_name_var <- intersect(names(df2), names(df1))
    finaldf <- df1 %>%
      dplyr::left_join(df2, by = "cell_name")
    
    # Join and substitute the value of the key with the values of the column of the joined df.
    # Drop the duplicate column.
    helper_left_join_and_substitute(finaldf, old_name, new_values_var, old_name_var)
    
  } else {
    # Find the name of the column with the new values and key of df1 for which to join
    new_values_var <- setdiff(names(df2), by.cond[[1]])
    old_name_var <- names(by.cond)
    finaldf <- df1 %>%
      dplyr::left_join(df2, by = by.cond)
    
    helper_left_join_and_substitute(finaldf, old_name, new_values_var, old_name_var)
  }
}
helper_left_join_and_substitute <- function(df_temp, keep_name, new_var, old_var) {
  # Join and substitute the value of the key with the values of the column of the joined df.
  # Drop the duplicate column.
  if (keep_name){ #Keep the original variable name
    df_temp[old_var] <- df_temp[new_var]
    df_temp[!names(df_temp) == new_var]
  } else { #Use df2 variable name
    df_temp[!names(df_temp) == old_var]
  }
}

# Create vector of ids for the pages
create_id_page <- function() {
  pokemon <- rcorpora::corpora("games/pokemon")$pokemon
  pokemon <- tolower(pokemon[pokemon$generation_id == "1",]$name)
  moods <- tolower(rcorpora::corpora("humans/moods")$moods)
  return(unique(ids::ids(200, moods, pokemon))[0:100])
}

# Create vector of ids for the objects/cells
create_id_cells <- function() {
  pokemon <- rcorpora::corpora("games/pokemon")$pokemon
  pokemon <- tolower(pokemon[pokemon$generation_id == "2",]$name)
  moods <- tolower(rcorpora::corpora("humans/moods")$moods)
  return(unique(ids::ids(200, moods, pokemon))[0:100])
}

# Coerce to a named vector
as.named.vector <- function(temp_df) {
  temp_vect <- as.character(as.vector(temp_df))
  names(temp_vect) <- names(temp_df)
  return(temp_vect)
}

# Call the creation of the df containing the styles of pages, take the default style (for now),
# exclude the name of the style and transform the output to named character vector
create_vector_param_page <- function() {
  temp_df <- create_page_styles_df() %>%
    dplyr::filter(name_style == "default") %>%
    dplyr::select(-name_style)
  return(as.named.vector(temp_df))
}

# original dataset have the style attribute divide in multple columns for easier access
# create the style attribute which is a combination of the columns divided by ";"
columns_to_style <- function(start_df) {
  
  # Drop final columns and retain the ones to combine. suppressWarnings otherwise one_of() throws a warning.
  temp_df <- start_df %>%
    dplyr::select(-c(name_style, style, parent,
                     any_of(c("vertex", "edge", "shape", "width", "height", "as", "relative"))))
  
  # Split the df in a list of row
  temp_list <- split(temp_df, seq(nrow(temp_df)))
  
  # check which styles we need to keep
  has_style <- start_df$style != ""
  
  # Combine the column values and names in a string. Put the strings in a vector.
  vect_temp <- c()
  for (i in seq_along(temp_list)) {
    # If row already have a style do not overwrite it with the other value from dfs
    # TODO overwrite style in case of value given by user directly in command (probably later with str_detect)
    if (has_style[[i]])  {
      
      vect_temp <- append(vect_temp, start_df$style[[i]])
      
    } else {
      
      temp_list[[i]] <- temp_list[[i]] %>%
        dplyr::select(which(temp_list[[i]] != ""))
      collapse_string = paste0(names(temp_list[[i]]), "=", temp_list[[i]], collapse = ";")
      
      if (collapse_string != "=") {
        vect_temp <- append(vect_temp, paste0(collapse_string, ";"))
      } else {
        vect_temp <- append(vect_temp, start_df$style[[i]])
      }
      
    }
  } 
  
  # Keep only the column we need and modify the style with the values from the vector created above.
  start_df <- suppressWarnings(start_df %>%
                                 dplyr::select(name_style, style, parent,
                                               any_of(c("vertex", "edge", "shape", "width", "height", "as", "relative"))) %>%
                                 dplyr::mutate(style = vect_temp))
}

# Create the tibble for the cells attributes. It is initialize with two empty cells as foundation(requirements?)
basic_cells <- function(cstyles, astyles) {
  temp_df <- tibble::tribble(
    ~cell_name, ~cell_style,~label,~tags,~link,~placeholders,~tooltip,~shape,~x,~y,~width,~height,~as,~level,
    #---------|------------|------|-----|-----|-------------|--------|------|--|--|------|-------|---|------|
    "0",     "empty",    "",   "",   "",           "",      "",    "","","",    "",     "", "",    NA,
    "1",     "start",    "",   "",   "",           "",      "",    "","","",    "",     "", "",    NA
  )
}

# Create the tibble for the arrows attributes. Initialize empty
basic_arrow_attributes <- function(cstyles, astyles) {
  temp_df <- tibble::tibble(label = character(),
                            tags = character(),
                            tooltip = character(),
                            arrow_style = character(),
                            source = character(),
                            target = character(),
                            width = character(),
                            relative = character(),
                            as = character())
}

#TODO comment functions below
populate_attrs_fd <- function(cells_attr, arrow_attr) {
  
  for (elem in cell_list) {
    
    if (!elem$input == "") {
      for (cell in elem$input[[1]]) {
        arrow_attr %<>%
          tibble::add_row(tibble::tibble_row(arrow_style = arrows_style, source = cell, target = elem$cell_name))
      }
    }
    if (!elem$output == "") {
      for (cell in elem$output[[1]]) {
        arrow_attr %<>%
          tibble::add_row(tibble::tibble_row(arrow_style = arrows_style, source = elem$cell_name, target = cell))
      }
    }
    
    elem[["output"]] <- NULL
    elem[["input"]] <- NULL
    cells_attr %<>%
      tibble::add_row(as_tibble(elem))
  }
  
  arrow_attr <- calc_tags_level0(cells_attr, arrow_attr)
  cells_attr <- calc_coordinates(cells_attr, direction)
  
  return(list(cells_attr, arrow_attr))
}

calc_tags_level0 <- function(cells_attr, arrow_attr) {
  
  cells_tag_level <- cells_attr %>%
    dplyr::select(cell_name, tags, level) %>%
    dplyr::rename(tag = tags)
  
  return(arrow_attr %>%
           dplyr::left_join(cells_tag_level, by = c("source" = "cell_name")) %>%
           dplyr::left_join(cells_tag_level, by = c("target" = "cell_name")) %>%
           dplyr::mutate(tags = paste(tag.x, tag.y, sep = " "), level0 = level.x*level.y == 0) %>%
           dplyr::select(-c(tag.x, tag.y, level.x, level.y)))
  
}

calc_coordinates <- function(cells_attr, direction) {
  
  cells_attr_not_empty <- cells_attr %>%
    filter(!cell_style %in% c("empty", "start"))
  
  if (direction == "TB") {
    cells_attr_not_empty %<>%
      mutate(y = if_else(level != 0, level * 100, 0), x = 0) %>%
      group_by(level) %>%
      mutate(x = if_else(level != 0, row_number() * 200 - 100 + lv_0_flag * 200, x),
             y = if_else(level != 0, y, row_number() * 100))
  } else if (direction == "LR") {
    #TODO check if work correctly TB, LR and RL
    cells_attr_not_empty %<>%
      mutate(x = if_else(level != 0, (level - 1) * 200, 0), y = 0) %>%
      group_by(level) %>%
      mutate(y = if_else(level != 0, row_number() * 100 + lv_0_flag * 100, x),
             x = if_else(level != 0, x, row_number() * 200))
  } else if (direction == "RL") {
    cells_attr_not_empty %<>%
      mutate(x = if_else(level != 0, (level - 1) * -200, 0), y = 0) %>%
      group_by(level) %>%
      mutate(y = if_else(level != 0, row_number() * -100 + lv_0_flag * -100, x),
             x = if_else(level != 0, x, row_number() * -200))
  }
  
  return(cells_attr %<>%
           filter(cell_style %in% c("empty", "start")) %>%
           rbind(cells_attr_not_empty) %>%
           select(-level))
}

