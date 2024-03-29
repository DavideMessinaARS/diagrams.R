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
  
  start_df %>%
    tibble::rowid_to_column("ID") %>%
    dplyr::mutate_all(dplyr::na_if, "") %>%
    tidyr::gather(key, value, any_of(c("html", "rounded", "whiteSpace", "fillColor",
                                       "strokeColor", "strokeWidth", "dashed")), factor_key = TRUE) %>%
    dplyr::filter(!is.na(value) | name_style %in% c("empty", "start")) %>%
    tidyr::unite(var, key:value, sep = "=", remove = F, na.rm = T) %>% 
    dplyr::select(-c(value)) %>% 
    tidyr::spread(key, var) %>%
    dplyr::mutate(dplyr::across(.cols = any_of(c("html", "rounded", "whiteSpace", "fillColor",
                                                 "strokeColor", "strokeWidth", "dashed")),
                                .fns = ~ ifelse(name_style %in% c("empty", "start"), NA, .x))) %>%
    dplyr::select(-c(ID)) %>%
    tidyr::unite(style, any_of(c("style", "html", "rounded", "whiteSpace", "fillColor",
                                 "strokeColor", "strokeWidth", "dashed")), sep = ";", na.rm = T)
  
}

# Create the tibble for the cells attributes. It is initialize with two empty cells as foundation(requirements?)
basic_cells <- function() {
  tibble::tribble(
    ~cell_name, ~cell_style,~label,~tags,~link,~placeholders,~tooltip,~shape,~x,~y,~width,~height,~as,~level,
    #---------|------------|------|-----|-----|-------------|--------|------|--|--|------|-------|---|------|
    "0",     "empty",    "",   "",   "",           "",      "",    "","","",    "",     "", "",    NA,
    "1",     "start",    "",   "",   "",           "",      "",    "","","",    "",     "", "",    NA
  )
}

# Create the tibble for the arrows attributes. Initialize empty
basic_arrow_attributes <- function() {
  tibble::tibble(label = character(),
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
populate_attrs_fd <- function(cell_list, direction) {
  
  cells_attr <- basic_cells()
  arrow_attr <- basic_arrow_attributes()
  
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
      tibble::add_row(tibble::as_tibble(elem))
  }
  
  arrow_attr <- calc_tags_level0(cells_attr, arrow_attr)
  cells_attr <- calc_coordinates(cells_attr, direction)
  
  return(list(cells_attr, arrow_attr))
}

populate_attrs_fd_roel <- function(path, direction) {
  
  cells_attr <- basic_cells()
  arrow_attr <- basic_arrow_attributes()
  
  temp_data <- readr::read_csv(path, show_col_types = FALSE)
  
  temp_data <- conception_to_roel(temp_data)
  
  temp_data %<>%
    dplyr::select(PROGRAM, FOLDER_VAR, FILE, TYPE) %>%
    dplyr::mutate(PROGRAM = stringr::str_extract(temp_data$PROGRAM, "^([^_]*_){2}[^_]*"),
                  level = as.integer(stringr::str_extract(temp_data$PROGRAM, "\\d+")),
                  level_datamodel = dplyr::if_else(TYPE == "OUTPUT", level * 2, 999),
                  level_step = (level * 2) - 1) %>%
    dplyr::group_by(FOLDER_VAR, FILE) %>%
    dplyr::mutate(level_datamodel = min(level_datamodel),
                  level_datamodel = dplyr::if_else(level_datamodel == 999, 0, level_datamodel)) %>%
    dplyr::ungroup()
  
  steps_cells <- temp_data %>%
    dplyr::transmute(cell_name = paste(PROGRAM, level, sep = "_"),
                     cell_style = steps_style,
                     label = paste("Step", PROGRAM, sep = "_"),
                     level = level_step) %>%
    dplyr::distinct()
  
  datamodel_cells <- temp_data %>%
    dplyr::transmute(cell_name = paste(FOLDER_VAR, FILE, sep = "_"),
                     cell_style = datamodels_style,
                     label = FILE,
                     level = level_datamodel) %>%
    dplyr::distinct()
  
  cells_attr %<>%
    dplyr::bind_rows(steps_cells, datamodel_cells)
  
  arrow_attr %<>%
    dplyr::bind_rows(
      temp_data %>%
        dplyr::transmute(
          arrow_style = arrows_style,
          source = dplyr::if_else(TYPE == "INPUT", paste(FOLDER_VAR, FILE, sep = "_"), paste(PROGRAM, level, sep = "_")),
          target = dplyr::if_else(TYPE == "INPUT", paste(PROGRAM, level, sep = "_"), paste(FOLDER_VAR, FILE, sep = "_"))
        )
    )
  
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
    dplyr::filter(!cell_style %in% c("empty", "start"))
  lv_0_flag <- 0 %in% cells_attr$level
  if (direction == "TB") {
    cells_attr_not_empty %<>%
      dplyr::mutate(y = dplyr::if_else(level != 0, level * 100, 0), x = 0) %>%
      dplyr::group_by(level) %>%
      dplyr::mutate(x = dplyr::if_else(level != 0, dplyr::row_number() * 200 - 100 + lv_0_flag * 200, x),
                    y = dplyr::if_else(level != 0, y, dplyr::row_number() * 100))
  } else if (direction == "LR") {
    #TODO check if work correctly TB, LR and RL
    cells_attr_not_empty %<>%
      dplyr::mutate(x = dplyr::if_else(level != 0, (level - 1) * 200, 0), y = 0) %>%
      dplyr::group_by(level) %>%
      dplyr::mutate(y = dplyr::if_else(level != 0, dplyr::row_number() * 100 + lv_0_flag * 100, x),
                    x = dplyr::if_else(level != 0, x, dplyr::row_number() * 200))
  } else if (direction == "RL") {
    cells_attr_not_empty %<>%
      dplyr::mutate(x = dplyr::if_else(level != 0, (level - 1) * -200, 0), y = 0) %>%
      dplyr::group_by(level) %>%
      dplyr::mutate(y = dplyr::if_else(level != 0, dplyr::row_number() * -100 + lv_0_flag * -100, x),
                    x = dplyr::if_else(level != 0, x, dplyr::row_number() * -200))
  }
  
  return(cells_attr %<>%
           dplyr::filter(cell_style %in% c("empty", "start")) %>%
           rbind(cells_attr_not_empty) %>%
           dplyr::select(-level))
}

createCell <- function(cell_name, cell_style = "", label = "", tags = "", link = "", x = "", y = "", level = 1, input = "", output = "") {
  
  if (link != "") link <- paste0('data:action/json,{"actions":[{"open": "', link, '"}]}')
  
  return(list(cell_name = cell_name, cell_style = cell_style, label = label, tags = tags,
              link = link, x = x, y = y, level = level, input = list(input), output = list(output)))
  
}
