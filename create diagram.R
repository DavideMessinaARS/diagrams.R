library(xml2)
library(tibble)
library(dplyr)

# Create vector of ids for the pages
create_id_page <- function() {
  pokemon <- rcorpora::corpora("games/pokemon")$pokemon
  pokemon <- tolower(pokemon[pokemon$generation_id == "1",]$name)
  moods <- tolower(rcorpora::corpora("humans/moods")$moods)
  return(unique(ids::ids(200, moods, pokemon))[0:100])
}

# Create new document and root. Number of pages is a variable defined in the parameters.
create_xml_container <- function(pages, df_tags2){
  test_xml <- xml_new_root("mxfile", host="Electron", modified="2021-02-12T20:24:28.529Z",
                           agent="5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) draw.io/14.1.8 Chrome/87.0.4280.88 Electron/11.1.1 Safari/537.36",
                           etag="fHMIuIajccZ_3DrzuGlE", version="14.1.8", type="device", pages = as.character(1))
  id_page <- create_id_page()
  vect_vars_diagram <- c("id", "name")

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

## parameters

# create styles and default values in a dataset (some parameter might be eliminated altogether, for example arrow width)
create_page_styles_df <- function() {
  temp_df <- tribble(
    ~name_style,  ~dx,  ~dy, ~grid,~gridSize,~guides,~tooltips,~connect,~arrows,~fold,~page,~pageScale,~pageWidth,~pageHeight,~math,~shadow,
    #----------|------|-----|-----|---------|-------|---------|--------|-------|-----|-----|----------|----------|-----------|-----|-------
    "default"  ,"1086","846",  "1",     "10",    "1",      "1",     "1",    "1",  "1",  "1",       "1",     "850",     "1100",  "0",    "0"
  )
}

page_styles <- create_page_styles_df()

create_vector_param_page <- function(df_styles) {
  temp_df <- df_styles %>%
    filter(name_style == "default") %>%
    select(-name_style)
  temp_vect <- as.character(as.vector(temp_df))
  names(temp_vect) <- names(temp_df)
  return(temp_vect)
}

test_xml <- create_xml_container(1, create_vector_param_page(page_styles))

# create a dataset with nodes/arrows relations (or two distinct dfs)
# create a second dataset for the order of "layers" or create the one with relationships map already ordered

# create a dataset containing links, tags, tooltip and other attributes (placeholders, shape, ...)

write_xml(test_xml, "C:\\Users\\Davide\\Downloads\\test r.xml")
