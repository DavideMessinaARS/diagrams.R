# TODO support for deciding styles
# TODO support for different style for each page
create_diagram <- function(path, pages = 1, arrows_style, steps_style, datamodels_style, direction){
  
  # Create pages attributes
  vector_param_page <- create_vector_param_page(pages)
  vector_param_mxGraphModel <- create_vector_param_mxGraphModel()
  
  # Create object, mxGeometry and mxCell attributes
  cell_arr <- populate_attrs_fd_roel(path, direction)
  cells_attr <- cell_arr[[1]]
  arrow_attr <- cell_arr[[2]]
  
  arrow_cell_attrs_tbl <- create_arrow_cell_attrs_tbl(arrow_attr, cells_attr, direction)
  
  object_attributes <- c("label", "tags", "link", "placeholders", "tooltip", "shape", "id")
  mxGeometry_attributes <- c("x", "y", "width", "height", "relative", "as")
  
  object_attrs_tbl <- arrow_cell_attrs_tbl %>%
    select(any_of(object_attributes))
  mxGeometry_attrs_tbl <- arrow_cell_attrs_tbl %>%
    select(any_of(mxGeometry_attributes))
  mxCell_attrs_tbl <- arrow_cell_attrs_tbl %>%
    select(-any_of(c(object_attributes, mxGeometry_attributes)))
  
  combined_attributes_1_2 <- create_arrow_cell_attrs_list(dplyr::bind_cols(object_attrs_tbl[1:2,], mxCell_attrs_tbl[1:2,]))
  mx_cell_1_2_child <- xml_add_child_with_attrs(root_child, "mxCell", combined_attributes_1_2)
  
  object_attrs_named_vector <- create_arrow_cell_attrs_list(tail(object_attrs_tbl, -2))
  mxCell_attrs_named_vector <- create_arrow_cell_attrs_list(tail(mxCell_attrs_tbl, -2))
  mxGeometry_attrs_named_vector <- create_arrow_cell_attrs_list(tail(mxGeometry_attrs_tbl, -2))
  
  # Create new root
  test_xml <- create_new_drawIOR_root()
  
  # TODO need fix for page > 1(xml_new_root before or after cycle?)
  for (page in 1:pages) {
    
    diagram_child <- xml_add_child(test_xml, "diagram")
    xml_set_attrs(diagram_child, vector_param_page[[page]])
    mxGraphModel_child <- xml_add_child(diagram_child, "mxGraphModel")
    xml_set_attrs(mxGraphModel_child, vector_param_mxGraphModel)
    root_child <- xml_add_child(mxGraphModel_child, "root")

    for (i in seq_along(object_attrs_named_vector)) {
      object_child <- xml_add_child_with_attrs(root_child, "object", object_attrs_named_vector[i])
      mxCell_child <- xml_add_child_with_attrs(object_child, "mxCell", mxCell_attrs_named_vector[i])
      mxGeometry_child <- xml_add_child_with_attrs(mxCell_child, "mxGeometry", mxGeometry_attrs_named_vector[i])
      
    }
  }
  return(test_xml)
}

##%######################################################%##
#                                                          #
####                   script to run                    ####
#                                                          #
##%######################################################%##

# TODO add a default standard in case not specified style or a style_name

# cella1 <- createCell("cella1", cell_style = "orange", label = "cella_arancione1", tags = "cell1 test", link = "", level = 0)
# cella2 <- createCell("cella2", cell_style = "orange", label = "cella_arancione2", tags = "cell2", link = "", level = 0)
# cella2a <- createCell("cella2a", cell_style = "orange", label = "cella_arancione2a", tags = "cell2", link = "", level = 1)
# cella3 <- createCell("cella3", cell_style = "yellow", label = "cella_gialla", tags = "cell3", link = "",
#                      level = 2, input = c("cella1", "cella2", "cella2a"), output = c("cella4", "cella5"))
# cella4 <- createCell("cella4", cell_style = "orange", label = "cella_arancione3", tags = "cell4", link = "", level = 3)
# cella5 <- createCell("cella5", cell_style = "orange", label = "cella_arancione4", tags = "cell5 aaa", link = "", level = 3)
# 
# cell_list <- list(cella1, cella2, cella2a, cella3, cella4, cella5)
# pages <- 1
# arrows_style <- "circle arrow"
# 
# test_xml <- create_diagram(cell_list, pages, arrows_style, direction = "TB")
# 
# write_xml(test_xml, "test r.xml")
