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

# Coerce to a named vector
as.named.vector <- function(temp_df) {
  
  temp_vect <- as.character(as.vector(temp_df))
  names(temp_vect) <- names(temp_df)
  temp_vect <- temp_vect[temp_vect != ""]
  return(temp_vect)
  
}

# Call the creation of the df containing the styles of pages, take the default style (for now),
# exclude the name of the style and transform the output to named character vector
create_vector_param_page <- function() {
  
  create_page_styles_df() %>%
    dplyr::filter(name_style == "default") %>%
    dplyr::select(-name_style) %>% 
    unlist()
  
}

# original dataset have the style attribute divide in multple columns for easier access
# create the style attribute which is a combination of the columns divided by ";"
columns_to_style <- function(start_df) {
  
  col_vect <- c("html", "rounded", "whiteSpace", "fillColor", "strokeColor", "strokeWidth", "dashed")
  
  start_df %>%
    tibble::rowid_to_column("ID") %>%
    dplyr::mutate_all(dplyr::na_if, "") %>%
    tidyr::gather(key, value, any_of(col_vect), factor_key = TRUE) %>%
    dplyr::filter(!is.na(value) | name_style %in% c("empty", "start")) %>%
    tidyr::unite(var, key:value, sep = "=", remove = F, na.rm = T) %>% 
    dplyr::select(-c(value)) %>% 
    tidyr::spread(key, var) %>%
    dplyr::mutate(dplyr::across(.cols = any_of(col_vect),
                                .fns = ~ ifelse(name_style %in% c("empty", "start"), NA, .x))) %>%
    dplyr::select(-c(ID)) %>%
    tidyr::unite(style, any_of(c("style", col_vect)), sep = ";", na.rm = T)
  
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
  
  create_ids_gen1 <<- create_unique_ids(1)
  
  cells_attr <- basic_cells()
  arrow_attr <- basic_arrow_attributes()
  
  temp_data <- readr::read_csv2(path,
                                locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
                                show_col_types = FALSE)
  
  temp_data %<>%
    dplyr::select(PROGRAM, FOLDER_VAR, FILE, TYPE) %>%
    dplyr::mutate(PROGRAM = stringr::str_extract(temp_data$PROGRAM, "(?<=_)(\\d|_|a|b)*(?=_)"),
                  level = as.integer(stringr::str_extract(temp_data$PROGRAM, "\\d+")),
                  level_datamodel = dplyr::if_else(TYPE == "OUTPUT", level * 2, 999),
                  level_step = (level * 2) - 1) %>%
    dplyr::group_by(FOLDER_VAR, FILE) %>%
    dplyr::mutate(level_datamodel = min(level_datamodel),
                  level_datamodel = dplyr::if_else(level_datamodel == 999, 0, level_datamodel),
                  cell_name = create_ids_gen1(1)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(PROGRAM) %>%
    dplyr::mutate(step_name = create_ids_gen1(1)) %>%
    dplyr::ungroup() 
  
  steps_cells <- temp_data %>%
    # dplyr::transmute(cell_name = paste(PROGRAM, level, sep = "_"),
    dplyr::transmute(cell_name = step_name,
                     cell_style = steps_style,
                     label = paste("Step", PROGRAM, sep = "_"),
                     level = level_step) %>%
    dplyr::distinct()
  
  datamodel_cells <- temp_data %>%
    # dplyr::transmute(cell_name = paste(FOLDER_VAR, FILE, sep = "_"),
    dplyr::transmute(cell_name,
                     cell_style = datamodels_style,
                     label = FILE,
                     level = level_datamodel) %>%
    dplyr::distinct()
  
  lookup_table <- dplyr::tibble(cell_name = c("0", "1"), new_ids = create_ids_gen1(2))
  
  cells_attr %<>%
    left_join_and_substitute(lookup_table) %>%
    dplyr::bind_rows(steps_cells, datamodel_cells)
  
  arrow_attr %<>%
    dplyr::bind_rows(
      temp_data %>%
        dplyr::transmute(
          arrow_style = arrows_style,
          source = dplyr::if_else(TYPE == "INPUT", cell_name, step_name),
          target = dplyr::if_else(TYPE == "INPUT", step_name, cell_name)
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
           tidyr::unite(tags, tag.x, tag.y, sep = " ", na.rm = T) %>%
           mutate_all(na_if, "") %>%
           dplyr::mutate(level0 = level.x * level.y == 0) %>%
           dplyr::select(-c(level.x, level.y)))
  
}

calc_coordinates <- function(cells_attr, direction) {
  
  cells_attr_not_empty <- cells_attr %>%
    dplyr::filter(!cell_style %in% c("empty", "start")) %>%
    dplyr::group_by(level) %>%
    dplyr::mutate(row_level = dplyr::row_number()) %>%
    dplyr::ungroup()
  
  if (direction == "TB") {
    cells_attr_not_empty %<>%
      dplyr::mutate(x = dplyr::if_else(level != 0, row_level * 200 + 100, 0),
                    y = dplyr::if_else(level != 0, level * 100, row_level * 100))
  } else if (direction == "LR") {
    #TODO check if work correctly TB, LR and RL
    cells_attr_not_empty %<>%
      dplyr::mutate(y = dplyr::if_else(level != 0, row_level * 100 + 100, 0),
                    x = dplyr::if_else(level != 0, (level - 1) * 200, row_level * 200 - 200))
  } else if (direction == "RL") {
    cells_attr_not_empty %<>%
      dplyr::mutate(y = dplyr::if_else(level != 0, row_level * -100 - 100, 0),
                    x = dplyr::if_else(level != 0, (level - 1) * -200, row_level * -200 + 200))
  } else if (direction == "BT") {
    cells_attr_not_empty %<>%
      dplyr::mutate(x = dplyr::if_else(level != 0, (row_level * 200 + 100) * -1, 0),
                    y = dplyr::if_else(level != 0, level * -100, row_level * -100))
  }
  
  cells_attr_not_empty %<>%
    dplyr::mutate(across(c(x, y), as.character))
  
  return(cells_attr %<>%
           dplyr::filter(cell_style %in% c("empty", "start")) %>%
           dplyr::bind_rows(cells_attr_not_empty) %>%
           dplyr::select(-c(level, row_level)))
}

createCell <- function(cell_name, cell_style = "", label = "", tags = "", link = "", x = "", y = "", level = 1, input = "", output = "") {
  
  if (link != "") link <- paste0('data:action/json,{"actions":[{"open": "', link, '"}]}')
  
  return(list(cell_name = cell_name, cell_style = cell_style, label = label, tags = tags,
              link = link, x = x, y = y, level = level, input = list(input), output = list(output)))
  
}

xml_add_child_with_attrs <- function(start_xml, node, xml_attribute) {
  
  for (xml_at in xml_attribute) {
    xml_set_attrs(dummy_child <- xml_add_child(start_xml, node), xml_at)
  }
  
  return(dummy_child)
}

# Call the creation of the df containing the styles of cells and arrows,
create_arrow_cell_attrs_tbl <- function(arrow_attr, cells_attr, direction) {
  
  # Create the stiles df for both cells and arrows. Collapse the column to create the variable style
  # if (direction == "TB") {
  #   outX <- inX <- "0.5"
  #   outY <- "1"
  #   inY <- "0"
  # } else if (direction == "LR") {
  #   outX <- "1"
  #   inX <- "0"
  #   outY <- inY <- "0.5"
  # } else if (direction == "RL") {
  #   outX <- "0"
  #   inX <- "1"
  #   outY <- inY <- "0.5"
  # }
  
  cell_styles <- create_cell_styles_df() %>%
    columns_to_style() %>%
    dplyr::mutate(across(parent:vertex,
                         ~ dplyr::recode(.x,
                                         "0" = cells_attr$cell_name[1],
                                         "1" = cells_attr$cell_name[2])))
  
  
  #TODO add option for other direction in addition to TB
  
  arrow_styles <- create_arrow_styles_df()  %>%
    dplyr::mutate(across(parent:edge,
                         ~ dplyr::recode(.x,
                                         "0" = cells_attr$cell_name[1],
                                         "1" = cells_attr$cell_name[2])))
  
  # arrow_styles <- create_arrow_styles_df() %>%
  #   mutate(exitX = outX, exitY = outY, entryX = inX, entryY = inY) %>%
  #   columns_to_style()
  # 
  # arrow_styles <- create_arrow_styles_df() %>%
  #   mutate(exitX = "1", exitY = "0.5", entryX = "0", entryY = "0.5") %>%
  #   columns_to_style()
  
  # Substitute the style name from the user-defined cells/arrows with the variable associated with them
  # tmp0 <- arrow_attr %>%
  #   filter(level0) %>%
  #   left_join(arrow_styles, by = c("arrow_style" = "name_style")) %>%
  #   mutate(width = coalesce(width.x, width.y),
  #          relative = coalesce(relative.x, relative.y),
  #          as = coalesce(as.x, as.y))  %>%
  #   select(-c(arrow_style, width.x, width.y, relative.x, relative.y, as.x, as.y, level0))
  # 
  # tmp1 <- arrow_attr %>%
  #   filter(!level0)  %>%
  #   left_join(arrow_styles, by = c("arrow_style" = "name_style")) %>%
  #   mutate(width = coalesce(width.x, width.y),
  #          relative = coalesce(relative.x, relative.y),
  #          as = coalesce(as.x, as.y))  %>%
  #   select(-c(arrow_style, width.x, width.y, relative.x, relative.y, as.x, as.y, level0))
  
  arrow_attr %<>%
    left_join(arrow_styles, by = c("arrow_style" = "name_style")) %>%
    mutate(width = coalesce(width.x, width.y),
           relative = coalesce(relative.x, relative.y),
           as = coalesce(as.x, as.y)) %>%
    select(-c(arrow_style, width.x, width.y, relative.x, relative.y, as.x, as.y, level0))
  
  cells_attr <- cells_attr %>%
    left_join(cell_styles, by = c("cell_style" = "name_style")) %>%
    mutate(shape = coalesce(shape.x, shape.y),
           width = coalesce(width.x, width.y),
           height = coalesce(height.x, height.y),
           as = coalesce(as.x, as.y)) %>%
    select(-c(cell_style, shape.x, shape.y, width.x, width.y, height.x, height.y, as.x, as.y))
  
  # Create vectors of ids for both cells and arrows 
  # id_cell <- create_ids_gen1(nrow(cells_attr))
  # id_arrows <- create_ids_gen1(nrow(arrow_attr))
  
  # Create a tbl. Take the user-defined cell name and link the cell-id to them
  # tbl_name_to_id <- cells_attr %>%
  #   select(cell_name) %>%
  #   mutate(id = id_cells)
  
  # Substitute the variable with the user-defined cell name with corresponding ids
  # cells_attr <- cells_attr %>%
  #   left_join_and_substitute(tbl_name_to_id, old_name = T, by.cond = c("parent" = "cell_name")) %>%
  #   left_join_and_substitute(tbl_name_to_id, old_name = F)
  
  cells_attr <- cells_attr %>%
    rename(id = cell_name)
  
  # Substitute the value, not the name, of the variables "source" and "target" with corresponding ids
  # Add the ids for the arrows
  # arrow_attr <- arrow_attr %>%
  #   left_join_and_substitute(tbl_name_to_id, old_name = T, by.cond = c("parent" = "cell_name")) %>%
  #   left_join_and_substitute(tbl_name_to_id, old_name = T, by.cond = c("source" = "cell_name")) %>%
  #   left_join_and_substitute(tbl_name_to_id, old_name = T, by.cond = c("target" = "cell_name")) %>%
  #   mutate(id = id_arrows)
  
  arrow_attr <- arrow_attr %>%
    mutate(id = create_ids_gen1(dplyr::n()))
  
  # Combine the two tbl
  cell_arrows_attrs_tbl <- bind_rows(cells_attr, arrow_attr)
  
  return(cell_arrows_attrs_tbl)
  
}

create_arrow_cell_attrs_list <- function(tbl_row) {
  
  tbl_row %<>% 
    mutate(
      across(everything(), ~tidyr::replace_na(.x, ""))
    ) %>%
    purrr::transpose()
  
  return(lapply(tbl_row, as.named.vector))
}