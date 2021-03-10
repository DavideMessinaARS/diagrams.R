library(xml2)
library(tibble)
library(dplyr)
library(magrittr)


##%######################################################%##
#                                                          #
####                auxiliary functions                 ####
#                                                          #
##%######################################################%##

# Call the creation of the df containing the styles of cells and arrows,
create_arrow_cell_attrs_tbl <- function(object_arrow_attributes, object_cell_attributes, direction) {
  
  # Create the stiles df for both cells and arrows. Collapse the column to create the variable style
  if (direction == "TB") {
    outX <- inX <- "0.5"
    outY <- "1"
    inY <- "0"
  } else if (direction == "LR") {
    outX <- "1"
    inX <- "0"
    outY <- inY <- "0.5"
  } else if (direction == "RL") {
    outX <- "0"
    inX <- "1"
    outY <- inY <- "0.5"
  }
  
  cell_styles <- create_cell_styles_df() %>%
    columns_to_style()
  arrow_styles <- create_arrow_styles_df() %>%
    mutate(exitX = outX, exitY = outY, entryX = inX, entryY = inY) %>%
    columns_to_style()
  
  # Substitute the style name from the user-defined cells/arrows with the variable associated with them
  object_arrow_attributes <- object_arrow_attributes %>%
    left_join(arrow_styles, by = c("arrow_style" = "name_style")) %>%
    mutate(width = coalesce(width.x, width.y),
           relative = coalesce(relative.x, relative.y),
           as = coalesce(as.x, as.y))  %>%
    select(-c(arrow_style, width.x, width.y, relative.x, relative.y, as.x, as.y))
  object_cell_attributes <- object_cell_attributes %>%
    left_join(cell_styles, by = c("cell_style" = "name_style")) %>%
    mutate(shape = coalesce(shape.x, shape.y),
           width = coalesce(width.x, width.y),
           height = coalesce(height.x, height.y),
           as = coalesce(as.x, as.y)) %>%
    select(-c(cell_style, shape.x, shape.y, width.x, width.y, height.x, height.y, as.x, as.y))

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
  # object_cell_attributes <- object_cell_attributes %>%
  #   left_join_and_substitute(tbl_name_to_id, old_name = T, by.cond = c("parent" = "cell_name")) %>%
  #   left_join_and_substitute(tbl_name_to_id, old_name = F)
  
  object_cell_attributes <- object_cell_attributes %>%
    rename(id = cell_name)
  
  # Substitute the value, not the name, of the variables "source" and "target" with corresponding ids
  # Add the ids for the arrows
  # object_arrow_attributes <- object_arrow_attributes %>%
  #   left_join_and_substitute(tbl_name_to_id, old_name = T, by.cond = c("parent" = "cell_name")) %>%
  #   left_join_and_substitute(tbl_name_to_id, old_name = T, by.cond = c("source" = "cell_name")) %>%
  #   left_join_and_substitute(tbl_name_to_id, old_name = T, by.cond = c("target" = "cell_name")) %>%
  #   mutate(id = id_arrows)
  
  object_arrow_attributes <- object_arrow_attributes %>%
    mutate(id = id_arrows)
  
  # Combine the two tbl
  cell_arrows_attrs_tbl <- bind_rows(object_cell_attributes, object_arrow_attributes)
  
  return(cell_arrows_attrs_tbl)
  
}

create_arrow_cell_attrs_list <- function(tbl_row) {
  tbl_row %>%
    select(which(tbl_row != ""))
  return(as.named.vector(tbl_row))
}

# Create new document and root. Number of pages is a variable defined in the parameters.
# TODO support for deciding styles
# TODO support for different style for each page
create_diagram <- function(cell_list, pages, arrows_style, direction){
  object_cell_attributes <- create_cell_attributes()
  object_arrow_attributes <- create_arrow_attributes()
  for (elem in cell_list) {
    if (elem$input != "") {
      for (cell in elem$input[[1]]) {
        object_arrow_attributes <- object_arrow_attributes %>%
          add_row(as_tibble(list(arrow_style = arrows_style, source = cell, target = elem$cell_name)))
      }
    }
    if (elem$output != "") {
      for (cell in elem$output[[1]]) {
        object_arrow_attributes <- object_arrow_attributes %>%
          add_row(as_tibble(list(arrow_style = arrows_style, source = elem$cell_name, target = cell)))
      }
    }
    
    elem[["output"]] <- NULL
    elem[["input"]] <- NULL
    object_cell_attributes <- object_cell_attributes %>%
      dplyr::add_row(as_tibble(elem))
    
    # name_cell <- names(elements_list)
    # if (name_cell[i] != "") {
    #   object_cell_attributes <- object_cell_attributes %>%
    #     add_row(as_tibble(c(elements_list[[i]], "cell_name" = name_cell[i])))
    # } else {
    #   object_arrow_attributes <- object_arrow_attributes %>%
    #     add_row(as_tibble(elements_list[[i]]))
    # }
  }
  
  object_cell_tags <- object_cell_attributes %>%
    dplyr::select(cell_name, tags) %>%
    dplyr::rename(tag = tags)
  
  object_arrow_attributes <- object_arrow_attributes %>%
    dplyr::left_join(object_cell_tags, by = c("source" = "cell_name")) %>%
    dplyr::left_join(object_cell_tags, by = c("target" = "cell_name")) %>%
    dplyr::mutate(tags = paste(tag.x, tag.y, sep = " ")) %>%
    dplyr::select(-c(tag.x, tag.y))
  
  part2_object_cell_attributes <- object_cell_attributes %>%
    filter(!cell_style %in% c("empty", "start"))
  if (direction == "TB") {
    part2_object_cell_attributes <- part2_object_cell_attributes %>%
      mutate(y = level * 100) %>%
      group_by(level) %>%
      mutate(x = row_number() * 200 - 100)
  } else if (direction == "LR") {
    part2_object_cell_attributes <- part2_object_cell_attributes %>%
      mutate(x = level * 200 - 100) %>%
      group_by(level) %>%
      mutate(y = row_number() * 100)
  } else if (direction == "RL") {
    part2_object_cell_attributes <- part2_object_cell_attributes %>%
      mutate(x = level * -200 + 100) %>%
      group_by(level) %>%
      mutate(y = row_number() * 100)
  }
  
  object_cell_attributes <- object_cell_attributes %>%
    filter(cell_style %in% c("empty", "start")) %>%
    rbind(part2_object_cell_attributes) %>%
    select(-level)
  
  
  test_xml <- xml_new_root("mxfile", host="Electron", modified="2021-02-12T20:24:28.529Z",
                           agent="5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) draw.io/14.1.8 Chrome/87.0.4280.88 Electron/11.1.1 Safari/537.36",
                           etag="fHMIuIajccZ_3DrzuGlE", version="14.1.8", type="device", pages = as.character(1))
  id_page <- create_id_page()
  vect_vars_diagram <- c("id", "name")
  
  vector_param_page <- create_vector_param_page()
  
  # TODO need fix for page > 1
  for (page in 1:pages) {
    
    xml_add_child(test_xml, "diagram")
    vect_values_diagram <- c(id_page[page], paste0("Page-", page))
    names(vect_values_diagram) <- vect_vars_diagram
    xml_set_attrs(xml_children(test_xml)[length(xml_children(test_xml))], vect_values_diagram)
    
    xml_add_child(xml_children(test_xml), "mxGraphModel")
    xml_set_attrs(xml_children(xml_children(test_xml))[length(xml_children(xml_children(test_xml)))], vector_param_page)
    xml_add_child(xml_children(xml_children(test_xml)), "root")

    arrow_cell_attrs_tbl <- create_arrow_cell_attrs_tbl(object_arrow_attributes, object_cell_attributes, direction)

    object_attrs_tbl <- arrow_cell_attrs_tbl %>%
      select(any_of(c("label", "tags", "link", "placeholders", "tooltip", "shape", "id")))
    mxGeometry_attrs_tbl <- arrow_cell_attrs_tbl %>%
      select(any_of(c("x", "y", "width", "height", "relative", "as")))
    mxCell_attrs_tbl <- arrow_cell_attrs_tbl %>%
      select(-any_of(c("label", "tags", "link", "placeholders", "tooltip", "shape", "id",
                       "x", "y", "width", "height", "relative", "as")))
    
    for (i in 1:nrow(object_attrs_tbl)) {
      object_attrs_named_row_filtered <- object_attrs_tbl[i,] %>%
        select(which(object_attrs_tbl[i,] != "" | is.null(object_attrs_tbl[i,])))
      object_attrs_named_vector <- create_arrow_cell_attrs_list(object_attrs_named_row_filtered)
      mxCell_attrs_named_row_filtered <- mxCell_attrs_tbl[i,] %>%
        select(which(mxCell_attrs_tbl[i,] != "" | is.null(mxCell_attrs_tbl[i,])))
      mxCell_attrs_named_vector <- create_arrow_cell_attrs_list(mxCell_attrs_named_row_filtered)
      mxGeometry_attrs_named_row_filtered <- mxGeometry_attrs_tbl[i,] %>%
        select(which(mxGeometry_attrs_tbl[i,] != "" | is.null(mxGeometry_attrs_tbl[i,])))
      mxGeometry_attrs_named_vector <- create_arrow_cell_attrs_list(mxGeometry_attrs_named_row_filtered)
      
      if (i %in% c(1,2)) {
        combined_attributes <- c(object_attrs_named_vector, mxCell_attrs_named_vector)
        xml_add_child(xml_children(xml_children(xml_children(test_xml))), "mxCell")
        xml_set_attrs(xml_children(xml_children(xml_children(xml_children(test_xml))))[length(xml_children(xml_children(xml_children(xml_children(test_xml)))))], combined_attributes)
        next
      }
      
      xml_add_child(xml_children(xml_children(xml_children(test_xml))), "object")
      xml_set_attrs(xml_children(xml_children(xml_children(xml_children(test_xml))))[length(xml_children(xml_children(xml_children(xml_children(test_xml)))))], object_attrs_named_vector)
      xml_add_child(xml_children(xml_children(xml_children(xml_children(test_xml))))[length(xml_children(xml_children(xml_children(xml_children(test_xml)))))], "mxCell")
      xml_set_attrs(xml_children(xml_children(xml_children(xml_children(xml_children(test_xml)))))[length(xml_children(xml_children(xml_children(xml_children(xml_children(test_xml))))))], mxCell_attrs_named_vector)
      xml_add_child(xml_children(xml_children(xml_children(xml_children(xml_children(test_xml)))))[length(xml_children(xml_children(xml_children(xml_children(xml_children(test_xml))))))], "mxGeometry")
      xml_set_attrs(xml_children(xml_children(xml_children(xml_children(xml_children(xml_children(test_xml))))))[length(xml_children(xml_children(xml_children(xml_children(xml_children(xml_children(test_xml)))))))], mxGeometry_attrs_named_vector)
      
    }
  }
  return(test_xml)
}

# Create the tibble for the cells attributes. It is initialize with two empty cells as foundation(requirements?)
create_cell_attributes <- function(cstyles, astyles) {
  temp_df <- tibble::tribble(
    ~cell_name, ~cell_style,~label,~tags,~link,~placeholders,~tooltip,~shape,~x,~y,~width,~height,~as,~level,
    #---------|------------|------|-----|-----|-------------|--------|------|--|--|------|-------|---|------|
           "0",     "empty",    "",   "",   "",           "",      "",    "","","",    "",     "", "",    NA,
           "1",     "start",    "",   "",   "",           "",      "",    "","","",    "",     "", "",    NA
  )
}

# Create the tibble for the arrows attributes. Initialize empty
create_arrow_attributes <- function(cstyles, astyles) {
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


##%######################################################%##
#                                                          #
####                     parameters                     ####
#                                                          #
##%######################################################%##

# create styles and default values in multiple datasets (some parameter might be eliminated altogether, for example arrow width)
create_page_styles_df <- function() {
  temp_df <- tibble::tribble(
    ~name_style,   ~dx,  ~dy,~grid,~gridSize,~guides,~tooltips,~connect,~arrows,~fold,~page,~pageScale,~pageWidth,~pageHeight,~math,~shadow,
    #----------|------|-----|-----|---------|-------|---------|--------|-------|-----|-----|----------|----------|-----------|-----|-------
    "default"  ,"1086","846",  "1",     "10",    "1",      "1",     "1",    "1",  "1",  "1",       "1",     "850",     "1100",  "0",    "0"
  )
}

create_arrow_styles_df <- function() {
  temp_df <- tibble::tribble(
       ~name_style,~style,~parent,~edge,~html,~verticalAlign,~startArrow,~startFill,~endArrow,~startSize,~exitX,~exitY,~exitDx,~exitDy,~entryX,~entryY,~entryDx,~entryDy,           ~edgeStyle,    ~elbow,~curved,~width,~relative,      ~as,
    #-------------|------|-------|-----|-----|--------------|-----------|----------|---------|----------|------|------|-------|-------|-------|-------|--------|--------|---------------------|----------|-------|------|---------|----------
    "circle arrow",    "",    "1",  "1",  "1",      "bottom",     "oval",         1,  "block",         8,     1,   0.5,      0,      0,      0,    0.5,       0,       0,"orthogonalEdgeStyle","vertical",      1,  "60",      "1","geometry"
  )
  return(temp_df)
}

create_cell_styles_df <- function() {
  temp_df <- tibble::tribble(
    ~name_style,~style,~parent,~vertex,~html,~rounded,~whiteSpace,~fillColor,~strokeColor,~strokeWidth,~dashed,~shape,~width,~height,       ~as,
    #----------|------|-------|-------|-----|--------|-----------|----------|------------|------------|-------|------|------|-------|-----------
        "empty",    "",     "",     "",   "",      "",         "",        "",          "",          "",     "",    "",    "",     "",        "",
        "start",    "",    "0",     "",   "",      "",         "",        "",          "",          "",     "",    "",    "",     "",        "",
       "orange",    "",    "1",    "1",  "1",     "0",     "wrap", "#ffcc99",   "#36393d",          "",     "","oval", "120",   "60","geometry",
       "yellow",    "",    "1",    "1",  "1",     "0",     "wrap", "#ffff88",   "#36393d",          "",     "","oval", "120",   "60","geometry"
  )
  return(temp_df)
}

# original dataset have the style attribute divide in multple columns for easier access
# create the style attribute which is a combination of the columns divided by ";"
columns_to_style <- function(start_df) {
  
  # Drop final columns and retain the ones to combine. suppressWarnings otherwise one_of() throws a warning.
  temp_df <- start_df %>% select(-c(name_style, style, parent, any_of(c("vertex", "edge", "shape", "width", "height", "as", "relative"))))
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
                                 select(name_style, style, parent, one_of(c("vertex", "edge", "shape", "width", "height", "as", "relative"))) %>%
                                 mutate(style = vect_temp))
}

##%######################################################%##
#                                                          #
####                   script to run                    ####
#                                                          #
##%######################################################%##

# TODO add a default standard in case not specified style or a style_name

# test_xml <- create_diagram(
#   1,
#   cella1 = list(cell_style = "orange", label = "cella_arancione", tags = "cell1", link = "", x = "80", y = "120"),
#   cella2 = list(cell_style = "yellow", label = "cella_gialla", tags = "cell2", link = "", x = "320", y = "40"),
#   list(tags = "1tag 2tag", arrow_style = "circle arrow", source = "cella1", target = "cella2")
#   )

createCell <- function(cell_name, cell_style = "", label = "", tags = "", link = "", x = "", y = "", level = 1, input = "", output = "") {
  return(list(cell_name = cell_name, cell_style = cell_style, label = label, tags = tags,
              link = link, x = x, y = y, level = level, input = list(input), output = list(output)))
}

cella1 <- createCell("cella1", cell_style = "orange", label = "cella_arancione1", tags = "cell1 test", link = "", level = 1)
cella2 <- createCell("cella2", cell_style = "orange", label = "cella_arancione2", tags = "cell2", link = "", level = 1)
cella3 <- createCell("cella3", cell_style = "yellow", label = "cella_gialla", tags = "cell3", link = "",
                     level = 2, input = c("cella1", "cella2"), output = c("cella4", "cella5"))
cella4 <- createCell("cella4", cell_style = "orange", label = "cella_arancione3", tags = "cell4", link = "", level = 3)
cella5 <- createCell("cella5", cell_style = "orange", label = "cella_arancione4", tags = "cell5 aaa", link = "", level = 3)

cell_list <- list(cella1, cella2, cella3, cella4, cella5)
pages <- 1
arrows_style <- "circle arrow"

test_xml <- create_diagram(cell_list, pages, arrows_style, direction = "TB")

# TODO apply the attribute in the datasets to the xml document

# create a dataset with nodes/arrows relations (or two distinct dfs)
# create a second dataset for the order of "layers" or create the one with relationships map already ordered

# create a dataset containing links, tags, tooltip and other attributes (placeholders, shape, ...)

write_xml(test_xml, "test r.xml")
