check_diagram <- function(path_to_dataset, path_to_diagram, direction = "TB"){
  
  doc <- xml2::read_xml(path_to_diagram)
  
  pages <- doc %>% xml2::xml_attr("pages")
  
  # data.frame(host = doc %>% xml2::xml_attr("host"),
  #            modified = doc %>% xml2::xml_attr("modified"),
  #            agent = doc %>% xml2::xml_attr("agent"),
  #            etag = doc %>% xml2::xml_attr("etag"),
  #            version = doc %>% xml2::xml_attr("version"),
  #            type = doc %>% xml2::xml_attr("type"),
  #            pages = doc %>% xml2::xml_attr("pages"))
  
  diagram_nodes <- xml2::xml_find_all(doc, ".//diagram")
  
  vector_param_page <- data.frame(id = diagram_nodes %>% xml2::xml_attr("id"),
                                  name = diagram_nodes %>% xml2::xml_attr("name"))
  
  mxGraphModel_nodes <- xml2::xml_find_all(doc, ".//mxGraphModel")
  
  vector_param_mxGraphModel <- data.frame(dx = mxGraphModel_nodes %>% xml2::xml_attr("dx"),
                                          dy = mxGraphModel_nodes %>% xml2::xml_attr("dy"),
                                          grid = mxGraphModel_nodes %>% xml2::xml_attr("grid"),
                                          gridSize = mxGraphModel_nodes %>% xml2::xml_attr("gridSize"),
                                          guides = mxGraphModel_nodes %>% xml2::xml_attr("guides"),
                                          tooltips = mxGraphModel_nodes %>% xml2::xml_attr("tooltips"),
                                          connect = mxGraphModel_nodes %>% xml2::xml_attr("connect"),
                                          arrows = mxGraphModel_nodes %>% xml2::xml_attr("arrows"),
                                          fold = mxGraphModel_nodes %>% xml2::xml_attr("fold"),
                                          page = mxGraphModel_nodes %>% xml2::xml_attr("page"),
                                          pageScale = mxGraphModel_nodes %>% xml2::xml_attr("pageScale"),
                                          pageWidth = mxGraphModel_nodes %>% xml2::xml_attr("pageWidth"),
                                          pageHeight = mxGraphModel_nodes %>% xml2::xml_attr("pageHeight"),
                                          math = mxGraphModel_nodes %>% xml2::xml_attr("math"),
                                          shadow = mxGraphModel_nodes %>% xml2::xml_attr("shadow"))
  
  mxCell.nodes <- xml2::xml_find_all(doc, ".//mxCell")
  
  vector_param_mxCell <- data.frame(id = mxCell.nodes %>% xml2::xml_attr("id"),
                                    label = mxCell.nodes %>% xml2::xml_attr("label"),
                                    style = mxCell.nodes %>% xml2::xml_attr("style"),
                                    parent = mxCell.nodes %>% xml2::xml_attr("parent"),
                                    vertex = mxCell.nodes %>% xml2::xml_attr("vertex"),
                                    source = mxCell.nodes %>% xml2::xml_attr("source"),
                                    target = mxCell.nodes %>% xml2::xml_attr("target"),
                                    edge = mxCell.nodes %>% xml2::xml_attr("edge"),
                                    html = mxCell.nodes %>% xml2::xml_attr("html"),
                                    verticalAlign = mxCell.nodes %>% xml2::xml_attr("verticalAlign"),
                                    startArrow = mxCell.nodes %>% xml2::xml_attr("startArrow"),
                                    startFill = mxCell.nodes %>% xml2::xml_attr("startFill"),
                                    endArrow = mxCell.nodes %>% xml2::xml_attr("endArrow"),
                                    startSize = mxCell.nodes %>% xml2::xml_attr("startSize"),
                                    exitX = mxCell.nodes %>% xml2::xml_attr("exitX"),
                                    exitY = mxCell.nodes %>% xml2::xml_attr("exitY"),
                                    exitDx = mxCell.nodes %>% xml2::xml_attr("exitDx"),
                                    exitDy = mxCell.nodes %>% xml2::xml_attr("exitDy"),
                                    entryX = mxCell.nodes %>% xml2::xml_attr("entryX"),
                                    entryY = mxCell.nodes %>% xml2::xml_attr("entryY"),
                                    entryDx = mxCell.nodes %>% xml2::xml_attr("entryDx"),
                                    entryDy = mxCell.nodes %>% xml2::xml_attr("entryDy"),
                                    edgeStyle = mxCell.nodes %>% xml2::xml_attr("edgeStyle"),
                                    elbow = mxCell.nodes %>% xml2::xml_attr("elbow"),
                                    curved = mxCell.nodes %>% xml2::xml_attr("curved"))
  
  object.nodes <- xml2::xml_find_all(doc, ".//object")
  
  vector_param_object <- data.frame(label = object.nodes %>% xml2::xml_attr("label"),
                                    tags = object.nodes %>% xml2::xml_attr("tags"),
                                    link = object.nodes %>% xml2::xml_attr("link"),
                                    placeholders = object.nodes %>% xml2::xml_attr("placeholders"),
                                    tooltip = object.nodes %>% xml2::xml_attr("tooltip"),
                                    shape = object.nodes %>% xml2::xml_attr("shape"),
                                    id = object.nodes %>% xml2::xml_attr("id")) %>%
    add_row(id = vector_param_mxCell[1:2, "id"], label = vector_param_mxCell[1:2, "label"], .before = 1)
  
  index_without_obect <- which(!is.na(vector_param_mxCell$id))[-c(1, 2)]
  
  for (i in index_without_obect) {
    vector_param_object %<>% add_row(id = vector_param_mxCell[i, "id"], .after = i - 1)
  }
  
  vector_param_mxCell %<>% select(-c(id, label))
  
  mxGeometry.nodes <- xml2::xml_find_all(doc, ".//mxGeometry")
  
  vector_param_mxGeometry <- data.frame(x = mxGeometry.nodes %>% xml2::xml_attr("x"),
                                        y = mxGeometry.nodes %>% xml2::xml_attr("y"),
                                        width = mxGeometry.nodes %>% xml2::xml_attr("width"),
                                        height = mxGeometry.nodes %>% xml2::xml_attr("height"),
                                        relative = mxGeometry.nodes %>% xml2::xml_attr("relative"),
                                        as = mxGeometry.nodes %>% xml2::xml_attr("as")) %>%
    add_row(.before = 1) %>%
    add_row(.before = 1)
  
  arrow_cell_recreated_attrs_tbl <- bind_cols(vector_param_mxCell, vector_param_object, vector_param_mxGeometry) 
  
  cols_to_keep <- c("source", "target")
  
  arrow_cell_attrs_tbl <- qs::qdeserialize(qs::base91_decode(arrow_cell_recreated_attrs_tbl[2, "label"]))
  
  
  
  
  
  test1 <- arrow_cell_attrs_tbl %>% dplyr::select(cols_to_keep)
  test2 <- arrow_cell_recreated_attrs_tbl %>% dplyr::select(cols_to_keep)
  
  # check 1 degree of separation
  
  old_not_new <- dplyr::anti_join(test1, test2, by = c("source", "target"))
  
  new_not_old <- dplyr::anti_join(test2, test1, by = c("source", "target"))
  
  if (nrow(old_not_new) == 0) {
    return(message("Your diagram already contains all the links defined in the input file"))
  }
  
  source_to_remove <- c()
  target_to_remove <- c()
  
  test2 %<>% filter(!is.na(source)) %>% add_count(source, target, name = "card_1")
  test3 <- test2 %>% inner_join(test2 %>% dplyr::rename(target_2 = target, target = source, card_2 = card_1), by = "target") %>%
    distinct()
  test4 <- test3 %>% left_join(old_not_new %>% dplyr::rename(target_3 = target), by = "source")
  test5 <- test4 %>% filter(target_2 == target_3) %>% select(-target_3)
  
  test6 <- test5 %>% filter(card_1 == 1) %>% select(source, target)
  test7 <- test5 %>% filter(card_2 == 1) %>% select(target, target_2) %>%
    dplyr::rename(source = target, target = target_2)
  
  test6_7 <- bind_rows(test6, test7) %>% mutate(flag_remove = T)
  
  test8 <- test5 %>% filter(card_1 > 1) %>% select(source, target)
  test9 <- test5 %>% filter(card_2 > 1) %>% select(target, target_2) %>%
    dplyr::rename(source = target, target = target_2)
  
  test8_9 <- bind_rows(test8, test9) %>% mutate(flag_color = T)
  
  # TODO add option to change color
  
  color <- "#ff0000"
  pattern_fill <- "fillColor[^;]*"
  pattern_stroke <- "strokeColor[^;]*"
  fill_color <- paste0("fillColor=", color)
  stroke_color <- paste0("strokeColor=", color)
  fill_color_semic <- paste0(";", fill_color)
  stroke_color_semic <- paste0(";", stroke_color)
  
  arrow_cell_recreated_attrs_tbl %<>%
    left_join(test6_7, by = c("source", "target")) %>%
    left_join(test8_9, by = c("source", "target")) %>%
    filter(is.na(flag_remove)) %>%
    select(-flag_remove) %>%
    mutate(style = case_when(
      is.na(flag_color) ~ style,
      grepl(style, pattern = pattern_fill) & grepl(style, pattern = pattern_stroke) ~ sub(sub(style, pattern = pattern_stroke, replacement = stroke_color), pattern = pattern_fill, replacement = fill_color),
      grepl(style, pattern = pattern_fill) ~ paste0(sub(style, pattern = pattern_fill, replacement = fill_color), stroke_color_semic),
      grepl(style, pattern = pattern_stroke) ~ paste0(sub(style, pattern = pattern_stroke, replacement = stroke_color), fill_color_semic),
      TRUE                      ~ paste0(style, fill_color_semic, stroke_color_semic)
    )) %>%
    filter(-flag_color) 
  
  vector_param_page <- create_arrow_cell_attrs_list(vector_param_page)
  vector_param_mxGraphModel <- create_arrow_cell_attrs_list(vector_param_mxGraphModel)[[1]]
  
  create_ids_gen1 <- create_unique_ids(1, arrow_cell_attrs_tbl$id)
  
  arrow_cell_attrs_tbl %<>%
    left_join(old_not_new %>% mutate(flag = T), by = c("source", "target")) %>%
    filter(flag) %>%
    select(-flag) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(id = create_ids_gen1(1))
  
  arrow_cell_recreated_attrs_tbl %<>% bind_rows(arrow_cell_attrs_tbl)
  test_xml <- populate_xml(vector_param_page, vector_param_mxGraphModel, arrow_cell_recreated_attrs_tbl)
  xml2::write_xml(test_xml, path_to_diagram)
  
  message_to_return <- if_else(nrow(test8_9) > 0, "Some links have been corrected, open the diagram to check", "All links have been corrected")
  
  return(message(message_to_return))
}