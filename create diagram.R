
##%######################################################%##
#                                                          #
####                auxiliary functions                 ####
#                                                          #
##%######################################################%##

# Create new document and root. Number of pages is a variable defined in the parameters.
# TODO support for deciding styles
# TODO support for different style for each page
create_diagram <- function(path, pages = 1, arrows_style, steps_style, datamodels_style, direction){
  
  # cell_list, pages = 1, arrows_style, direction
  # cell_arr <- populate_attrs_fd(cell_list, direction)
  
  cell_arr <- populate_attrs_fd_roel(path, direction)
  cells_attr <- cell_arr[[1]]
  arrow_attr <- cell_arr[[2]]
  
  create_ids_gen2 <- create_unique_ids(2)
  id_page <- create_ids_gen2(pages)
  
  # TODO open to other styles
  vector_param_page <- create_vector_param_page()
  
  my_options <- options(digits.secs = 3)  
  test_xml <- xml_new_root("mxfile", host = "Electron", modified = Sys.time() %>% format('%Y-%m-%dT%H:%M:%OSZ'),
                           agent = "5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) draw.io/14.1.8 Chrome/87.0.4280.88 Electron/11.1.1 Safari/537.36",
                           etag = "fHMIuIajccZ_3DrzuGlE", version = "14.1.8", type = "device", pages = as.character(1))
  options(my_options)
  
  # TODO need fix for page > 1
  # TODO clean child usage
  for (page in 1:pages) {
    
    
    
    # my_options <- options(digits.secs = 3)  
    # test_xml_2 <- xml_new_root("mxfile", host = "Electron", modified = Sys.time() %>% format('%Y-%m-%dT%H:%M:%OSZ'),
    #                            agent = "5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) draw.io/14.1.8 Chrome/87.0.4280.88 Electron/11.1.1 Safari/537.36",
    #                            etag = "fHMIuIajccZ_3DrzuGlE", version = "14.1.8", type = "device", pages = as.character(1))
    # options(my_options)
    # diagram_child <- xml_add_child(test_xml_2, "diagram")
    # xml_set_attrs(diagram_child, c("id" = id_page[page], "name" = paste0("Page-", page)))
    # mxGraphModel_child <- xml_add_child(diagram_child, "mxGraphModel")
    # xml_set_attrs(mxGraphModel_child, vector_param_page)
    # xml_add_child(mxGraphModel_child, "root")
    
  
    my_options <- options(digits.secs = 3)  
    test_xml <- xml_new_root("mxfile", host = "Electron", modified = Sys.time() %>% format('%Y-%m-%dT%H:%M:%OSZ'),
                             agent = "5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) draw.io/14.1.8 Chrome/87.0.4280.88 Electron/11.1.1 Safari/537.36",
                             etag = "fHMIuIajccZ_3DrzuGlE", version = "14.1.8", type = "device", pages = as.character(1))
    options(my_options)
    diagram_child <- xml_add_child(test_xml, "diagram")
    xml_set_attrs(diagram_child, c("id" = id_page[page], "name" = paste0("Page-", page)))
    mxGraphModel_child <- xml_add_child(diagram_child, "mxGraphModel")
    xml_set_attrs(mxGraphModel_child, vector_param_page)
    root_child <- xml_add_child(mxGraphModel_child, "root")
    
    # (test_xml_2)
    # (test_xml)

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
    
    object_attrs_tbl <- tail(object_attrs_tbl, -2)
    mxCell_attrs_tbl <- tail(mxCell_attrs_tbl, -2)
    mxGeometry_attrs_tbl <- tail(mxGeometry_attrs_tbl, -2)
    
    object_attrs_named_vector <- create_arrow_cell_attrs_list(object_attrs_tbl)
    mxCell_attrs_named_vector <- create_arrow_cell_attrs_list(mxCell_attrs_tbl)
    mxGeometry_attrs_named_vector <- create_arrow_cell_attrs_list(mxGeometry_attrs_tbl)
    
    for (i in seq_along(object_attrs_named_vector)) {
      object_child <- xml_add_child_with_attrs(root_child, "object", object_attrs_named_vector[i])
      mxCell_child <- xml_add_child_with_attrs(object_child, "mxCell", mxCell_attrs_named_vector[i])
      mxGeometry_child <- xml_add_child_with_attrs(mxCell_child, "mxGeometry", mxGeometry_attrs_named_vector[i])
      
    }
    
    # object_child <- xml_add_child_with_attrs(root_child, "object", object_attrs_named_vector)
    # mxCell_child <- xml_add_child_with_attrs(object_child, "mxCell", mxCell_attrs_named_vector)
    # mxGeometry_child <- xml_add_child_with_attrs(mxCell_child, "mxGeometry", mxGeometry_attrs_named_vector)
    
    
    # for (i in 1:nrow(object_attrs_tbl)) {
    #   # object_attrs_named_row_filtered <- object_attrs_tbl[i,] %>%
    #   #   select(which(object_attrs_tbl[i,] != "" | is.null(object_attrs_tbl[i,])))
    #   object_attrs_named_vector <- create_arrow_cell_attrs_list(object_attrs_tbl)
    #   # mxCell_attrs_named_row_filtered <- mxCell_attrs_tbl[i,] %>%
    #   #   select(which(mxCell_attrs_tbl[i,] != "" | is.null(mxCell_attrs_tbl[i,])))
    #   mxCell_attrs_named_vector <- create_arrow_cell_attrs_list(mxCell_attrs_tbl)
    #   # mxGeometry_attrs_named_row_filtered <- mxGeometry_attrs_tbl[i,] %>%
    #   #   select(which(mxGeometry_attrs_tbl[i,] != "" | is.null(mxGeometry_attrs_tbl[i,])))
    #   mxGeometry_attrs_named_vector <- create_arrow_cell_attrs_list(mxGeometry_attrs_tbl)
    #   
    #   if (i %in% c(1,2)) {
    #     combined_attributes <- c(object_attrs_named_vector, mxCell_attrs_named_vector)
    #     xml_add_child(xml_children(xml_children(xml_children(test_xml))), "mxCell")
    #     xml_set_attrs(xml_children(xml_children(xml_children(xml_children(test_xml))))[length(xml_children(xml_children(xml_children(xml_children(test_xml)))))], combined_attributes)
    #     next
    #   }
    #   
    #   xml_add_child(xml_children(xml_children(xml_children(test_xml))), "object")
    #   xml_set_attrs(xml_children(xml_children(xml_children(xml_children(test_xml))))[length(xml_children(xml_children(xml_children(xml_children(test_xml)))))], object_attrs_named_vector)
    #   xml_add_child(xml_children(xml_children(xml_children(xml_children(test_xml))))[length(xml_children(xml_children(xml_children(xml_children(test_xml)))))], "mxCell")
    #   xml_set_attrs(xml_children(xml_children(xml_children(xml_children(xml_children(test_xml)))))[length(xml_children(xml_children(xml_children(xml_children(xml_children(test_xml))))))], mxCell_attrs_named_vector)
    #   xml_add_child(xml_children(xml_children(xml_children(xml_children(xml_children(test_xml)))))[length(xml_children(xml_children(xml_children(xml_children(xml_children(test_xml))))))], "mxGeometry")
    #   xml_set_attrs(xml_children(xml_children(xml_children(xml_children(xml_children(xml_children(test_xml))))))[length(xml_children(xml_children(xml_children(xml_children(xml_children(xml_children(test_xml)))))))], mxGeometry_attrs_named_vector)
    #   
    # }
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
