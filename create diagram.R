library(xml2)
library(tibble)
library(dplyr)


##%######################################################%##
#                                                          #
####                auxiliary functions                 ####
#                                                          #
##%######################################################%##

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

# Call the creation of the df containing the styles of pages,
# take the default style (for now), exclude the name of the style
# transform the output to named character vector
create_vector_param_page <- function() {
  temp_df <- create_page_styles_df() %>%
    filter(name_style == "default") %>%
    select(-name_style)
  temp_vect <- as.character(as.vector(temp_df))
  names(temp_vect) <- names(temp_df)
  return(temp_vect)
}

# Call the creation of the df containing the styles of cells and arrows,
create_arrow_cell_attrs_tbl <- function() {
  
  # Create the stiles df for both cells and arrows. Collapse the column to create the variable style
  cell_styles <- create_cell_styles_df() %>%
    columns_to_style()
  arrow_styles <- create_arrow_styles_df() %>%
    columns_to_style()
  
  # Substitute the style name from the user-defined cells/arrows with the variable associated with them
  object_arrow_attributes <- object_arrow_attributes %>%
    left_join(arrow_styles, by = c("arrow_style" = "name_style")) %>%
    select(-arrow_style )
  object_cell_attributes <- object_cell_attributes %>%
    left_join(cell_styles, by = c("cell_style" = "name_style")) %>%
    select(-cell_style )
  
  # Create vectors of ids for both cells and arrows 
  id_cell <- create_id_cells()
  nrow_obj_cell = nrow(object_cell_attributes)
  id_cells <- id_cell[1:nrow_obj_cell]
  id_arrows <- id_cell[(nrow_obj_cell+1):(nrow_obj_cell+nrow(object_arrow_attributes))]
  
  # Create a tbl. Take the user-defined cell name and link the cell-id to them
  tbl_name_to_id <- object_cell_attributes %>%
    select(cell_name) %>%
    mutate(id = id_cells)
  
  # Substitute the variable with the user-defined cell name with corresponding ids
  object_cell_attributes <- object_cell_attributes %>%
    left_join_and_substitute(tbl_name_to_id, old_name = F)
  
  # Substitute the value, not the name, of the variables "source" and "target" with corresponding ids
  # Add the ids for the arrows
  object_arrow_attributes <- object_arrow_attributes %>%
    left_join_and_substitute(tbl_name_to_id, old_name = T, by.cond = c("source" = "cell_name")) %>%
    left_join_and_substitute(tbl_name_to_id, old_name = T, by.cond = c("target" = "cell_name")) %>%
    mutate(id = id_arrows)
  
  # Combine the two tbl
  cell_arrows_attrs_tbl <- bind_rows(object_cell_attributes, object_arrow_attributes)
  
  return(cell_arrows_attrs_tbl)
  
}

create_arrow_cell_attrs_list <- function(tbl_row) {
  tbl_row %>%
    select(which(tbl_row != ""))
  temp_vect <- as.character(as.vector(tbl_row))
  names(temp_vect) <- names(tbl_row)
  return(temp_vect)
}

# Create new document and root. Number of pages is a variable defined in the parameters.
# TODO support for deciding styles
# TODO support for different style for each page
create_xml_container <- function(pages){
  test_xml <- xml_new_root("mxfile", host="Electron", modified="2021-02-12T20:24:28.529Z",
                           agent="5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) draw.io/14.1.8 Chrome/87.0.4280.88 Electron/11.1.1 Safari/537.36",
                           etag="fHMIuIajccZ_3DrzuGlE", version="14.1.8", type="device", pages = as.character(1))
  id_page <- create_id_page()
  vect_vars_diagram <- c("id", "name")
  
  vector_param_page <- create_vector_param_page()
  
  for (page in 1:pages) {
    
    xml_add_child(test_xml, "diagram")
    vect_values_diagram <- c(id_page[page], paste0("Page-", page))
    names(vect_values_diagram) <- vect_vars_diagram
    xml_set_attrs(xml_children(test_xml)[length(xml_children(test_xml))], vect_values_diagram)
    
    xml_add_child(xml_children(test_xml), "mxGraphModel")
    xml_set_attrs(xml_children(xml_children(test_xml))[length(xml_children(xml_children(test_xml)))], vector_param_page)
    xml_add_child(xml_children(xml_children(test_xml)), "root")
    
    arrow_cell_attrs_tbl <- create_arrow_cell_attrs_tbl()
    arrow_cell_attrs_rows <- split(arrow_cell_attrs_tbl, seq(nrow(arrow_cell_attrs_tbl)))
    
    # TODO divide object and cell attributes, maybe inside create_arrow_cell_attrs_tbl()
    
    for (object in arrow_cell_attrs_rows) {
      arrow_cell_attrs_list <- create_arrow_cell_attrs_list(object)
      xml_add_child(xml_children(xml_children(xml_children(test_xml))), "object")
      xml_set_attrs(xml_children(xml_children(xml_children(xml_children(test_xml))))[length(xml_children(xml_children(xml_children(xml_children(test_xml)))))], arrow_cell_attrs_list)
      xml_add_child(xml_children(xml_children(xml_children(xml_children(test_xml)))), "mxCell")
      xml_set_attrs(xml_children(xml_children(xml_children(xml_children(xml_children(test_xml)))))[length(xml_children(xml_children(xml_children(xml_children(xml_children(test_xml))))))], arrow_cell_attrs_list)
      
    }
  }
  return(test_xml)
}

# Create the tibble for the cells attributes. It is initialize with two empty cells as foundation(requirements?)
create_cell_attributes <- function(cstyles, astyles) {
  temp_df <- tribble(
    ~cell_name, ~destinations, ~cell_style,~label,~tags,~link,~placeholders,~tooltip,~shape,
    #---------|--------------|------------|------|-----|-----|-------------|--------|------|
        "base",            "",     "empty",    "",   "",   "",           "",      "",    "",
       "start",            "",     "start",    "",   "",   "",           "",      "",    "",
  )
}

# Create the tibble for the arrows attributes. Initialize empty
create_arrow_attributes <- function(cstyles, astyles) {
  temp_df <- tibble(label=character(),
                    tags=character(),
                    tooltip=character(),
                    arrow_style=character(),
                    source=character(),
                    target=character())
}

##%######################################################%##
#                                                          #
####                     parameters                     ####
#                                                          #
##%######################################################%##

# create styles and default values in multiple datasets (some parameter might be eliminated altogether, for example arrow width)
create_page_styles_df <- function() {
  temp_df <- tribble(
    ~name_style,   ~dx,  ~dy,~grid,~gridSize,~guides,~tooltips,~connect,~arrows,~fold,~page,~pageScale,~pageWidth,~pageHeight,~math,~shadow,
    #----------|------|-----|-----|---------|-------|---------|--------|-------|-----|-----|----------|----------|-----------|-----|-------
    "default"  ,"1086","846",  "1",     "10",    "1",      "1",     "1",    "1",  "1",  "1",       "1",     "850",     "1100",  "0",    "0"
  )
}

create_arrow_styles_df <- function() {
  temp_df <- tribble(
       ~name_style,~style,~parent,~edge,~html,~verticalAlign,~startArrow,~startFill,~endArrow,~startSize,~exitX,~exitY,~exitDx,~exitDy,~entryX,~entryY,~entryDx,~entryDy,           ~edgeStyle,    ~elbow,~curved,
    #-------------|------|-------|-----|-----|--------------|-----------|----------|---------|----------|------|------|-------|-------|-------|-------|--------|--------|---------------------|----------|-------
    "circle arrow",    "",    "1",  "1",  "1",      "bottom",     "oval",         1,  "block",         8,     1,   0.5,      0,      0,      0,    0.5,       0,       0,"orthogonalEdgeStyle","vertical",      1
  )
}

create_cell_styles_df <- function() {
  temp_df <- tribble(
    ~name_style,~style,~parent,~vertex,~html,~rounded,~whiteSpace,~fillColor,~strokeColor,~strokeWidth,~dashed,
    #----------|------|-------|-------|-----|--------|-----------|----------|------------|------------|-------
        "empty",    "",     "",     "",   "",      "",         "",        "",          "",          "",     "",
        "start",    "",    "0",     "",   "",      "",         "",        "",          "",          "",     "",
       "orange",    "",    "1",    "1",  "1",     "0",     "wrap", "#ffcc99",   "#36393d",          "",     "",
       "yellow",    "",    "1",    "1",  "1",     "0",     "wrap", "#ffff88",   "#36393d",          "",     ""
  )
}

# original dataset have the style attribute divide in multple columns for easier access
# create the style attribute which is a combination of the columns divided by ";"
columns_to_style <- function(start_df) {
  # Drop final columns and retain the ones to combine. suppressWarnings otherwise one_of() throws a warning.
  suppressWarnings(temp_df <- start_df %>% select(-c(name_style, style, parent, one_of("vertex", "edge"))))
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
        select(which(temp_list[[i]] != ""))
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
                                 select(name_style, style, parent, one_of("vertex", "edge")) %>%
                                 mutate(style = vect_temp))
}

object_cell_attributes <- create_cell_attributes() %>%
  add_row(cell_name = "1cella", cell_style = "orange", label = "cella_arancione", tags = "cell1", link = "",
          tooltip = "hey, this is the first tooltip", shape = "oval") %>%
  add_row(cell_name = "2cella", cell_style = "yellow", label = "cella_gialla", tags = "cell2", link = "",
          tooltip = "Now is the second tooltip", shape = "oval")

object_arrow_attributes <- create_arrow_attributes() %>%
  add_row(tags = "1tag 2tag", arrow_style = "circle arrow", source = "1cella", target = "2cella")

##%######################################################%##
#                                                          #
####                   script to run                    ####
#                                                          #
##%######################################################%##

# test_xml <- create_xml_container(2)

# TODO apply the attribute in the datasets to the xml document



# create a dataset with nodes/arrows relations (or two distinct dfs)
# create a second dataset for the order of "layers" or create the one with relationships map already ordered

# create a dataset containing links, tags, tooltip and other attributes (placeholders, shape, ...)

# write_xml(test_xml, "test r.xml")
