library(xml2)
library(tibble)
library(dplyr)


##%######################################################%##
#                                                          #
####                auxiliary functions                 ####
#                                                          #
##%######################################################%##

# Create vector of ids for the pages
create_id_page <- function() {
  pokemon <- rcorpora::corpora("games/pokemon")$pokemon
  pokemon <- tolower(pokemon[pokemon$generation_id == "1",]$name)
  moods <- tolower(rcorpora::corpora("humans/moods")$moods)
  return(unique(ids::ids(200, moods, pokemon))[0:100])
}

# Call the creation of the df containing the styles of pages,
# take the default (for now), exclude the name of the style
# transform the output to named character vector
create_vector_param_page <- function() {
  temp_df <- create_page_styles_df() %>%
    filter(name_style == "default") %>%
    select(-name_style)
  temp_vect <- as.character(as.vector(temp_df))
  names(temp_vect) <- names(temp_df)
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
  
  df_tags2 <- create_vector_param_page()
  
  for (page in 1:pages) {
    
    xml_add_child(test_xml, "diagram")
    vect_values_diagram <- c(id_page[page], paste0("Page-", page))
    names(vect_values_diagram) <- vect_vars_diagram
    xml_set_attrs(xml_children(test_xml)[length(xml_children(test_xml))], vect_values_diagram)
    
    xml_add_child(xml_children(test_xml), "mxGraphModel")
    xml_set_attrs(xml_children(xml_children(test_xml))[length(xml_children(xml_children(test_xml)))], df_tags2)
    xml_add_child(xml_children(xml_children(test_xml)), "root")
    
  }
  return(test_xml)
}


##%######################################################%##
#                                                          #
####                     parameters                     ####
#                                                          #
##%######################################################%##

# create styles and default values in a dataset (some parameter might be eliminated altogether, for example arrow width)
create_page_styles_df <- function() {
  temp_df <- tribble(
    ~name_style,   ~dx,  ~dy,~grid,~gridSize,~guides,~tooltips,~connect,~arrows,~fold,~page,~pageScale,~pageWidth,~pageHeight,~math,~shadow,
    #----------|------|-----|-----|---------|-------|---------|--------|-------|-----|-----|----------|----------|-----------|-----|-------
    "default"  ,"1086","846",  "1",     "10",    "1",      "1",     "1",    "1",  "1",  "1",       "1",     "850",     "1100",  "0",    "0"
  )
}

create_arrow_styles_df <- function() {
  temp_df <- tribble(
       ~name_style,~label,~style,~parent,~vertex,~html,~verticalAlign,~startArrow,~startFill,~endArrow,~startSize,~exitX,~exitY,~exitDx,~exitDy,~entryX,~entryY,~entryDx,~entryDy,           ~edgeStyle,    ~elbow,~curved,
    #-------------|------|------|-------|-------|-----|--------------|-----------|----------|---------|----------|------|------|-------|-------|-------|-------|--------|--------|---------------------|----------|-------
    "circle arrow",    "",    "",    "1",    "1",  "1",      "bottom",     "oval",         1,  "block",         8,     1,   0.5,      0,      0,      0,    0.5,       0,       0,"orthogonalEdgeStyle","vertical",      1
  )
}

create_cell_styles_df <- function() {
  temp_df <- tribble(
    ~name_style,~label,~style,~parent,~edge,~html,~rounded,~whiteSpace,~fillColor,~strokeColor,~strokeWidth,~dashed,
    #----------|------|------|-------|-----|--------|-----------|-----|----------|------------|------------|-------
    "orange"   ,    "",    "",    "1",  "1",  "1",     "0",     "wrap", "#ffcc99",   "#36393d",          "",     "",
    "yellow"   ,    "",    "",    "1",  "1",  "1",     "0",     "wrap", "#ffff88",   "#36393d",          "",     ""
  )
}

get_styles_dfs <- function(variables) {
  
}

test_cell <- create_cell_styles_df()
test_arrow <- create_arrow_styles_df()

test <- suppressWarnings(create_arrow_styles_df() %>%
                           select(-c(name_style, label, style, parent, -one_of("vertex", "edge"))))

paste0(paste0(names(test), "=", test, collapse = ";"), ";")

##%######################################################%##
#                                                          #
####                   script to run                    ####
#                                                          #
##%######################################################%##

test_xml <- create_xml_container(2)

# create a dataset with nodes/arrows relations (or two distinct dfs)
# create a second dataset for the order of "layers" or create the one with relationships map already ordered

# create a dataset containing links, tags, tooltip and other attributes (placeholders, shape, ...)

write_xml(test_xml, "test r.xml")
