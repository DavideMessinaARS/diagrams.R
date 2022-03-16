# TODO support for deciding styles
# TODO support for different style for each page
create_diagram <- function(path, pages = 1, arrows_style, steps_style, datamodels_style, direction){
  
  # Create pages attributes
  vector_param_page <- create_vector_param_page(pages)
  vector_param_mxGraphModel <- create_vector_param_mxGraphModel()
  
  # Create object, mxGeometry and mxCell attributes
  create_ids_gen1 <- create_unique_ids(1)
  cell_arr <- populate_attrs_fd_roel(path, direction)
  cells_attr <- cell_arr[[1]]
  arrow_attr <- cell_arr[[2]]
  
  arrow_cell_attrs_tbl <- create_arrow_cell_attrs_tbl(arrow_attr, cells_attr, direction)
  
  test_xml <- populate_xml(vector_param_page, vector_param_mxGraphModel, arrow_cell_attrs_tbl)
  return(test_xml)
}

