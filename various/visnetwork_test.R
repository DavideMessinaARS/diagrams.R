require(visNetwork, quietly = TRUE)
library("xtable")
# minimal example

options(xtable.sanitize.text.function = line_break_function)

line_break_function <- function(x){
  gsub("$<$br$>$","<br>",x)
}

html_tbl <- function(x){
  return(print(xtable(x), type="html", include.rownames = F, print.results=FALSE))
}

dfs_names <- tibble::tribble(
  ~id,                           ~title,            ~level, ~url_test,
  "covid_registry",              html_tbl(table1),       1, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts',
  "D3_events_DEATH",             html_tbl(table2),       1, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts',
  "D3_PERSONS",                  html_tbl(table3),       1, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts',
  "D3_output_spells_category",   html_tbl(table4),       1, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts',
  "CONCEPT",                     html_tbl(table5),       1, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts',
  "selected_doses",              html_tbl(table1),       2, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts',
  "Flowchart_QC_criteria",       html_tbl(table2),       2, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts',
  "D3_concepts_QC_criteria",     html_tbl(table3),       2, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts',
  "D3_selection_criteria_doses", html_tbl(table4),       3, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts',
  "persons_doses",               html_tbl(table5),       3, 'https://github.com/ARS-toscana/ECVM/wiki/Data-Model#doses_birthcohorts'
)
dfs_names <- dfs_names %>% dplyr::mutate(shape = "box", label = id, level = level * 2, group = as.character(level),
                                         x = 2, shapeProperties.borderRadius = 1)
steps_names <- tibble::tribble(
  ~id,          ~level,
  "step 01_1",  1,
  "step 01_2",  1,
  "step 01_3",  1,
  "step 01_4",  1,
  "step 02_1",  2,
  "step 02_2",  2,
  "step 03_1",  3,
  "step 03_2",  3
)
steps_names <- steps_names %>% dplyr::mutate(shape = "circle", label = id, level = level * 2 - 1, group = as.character(level), x = 2)
nodes <- dplyr::bind_rows(nodes_init, dfs_names, steps_names)

edges <- tibble::tribble(
  ~from,                         ~to,       
  "SURVEY_ID",                   "step 01_4",
  "SURVEY_OBSERVATIONS",         "step 01_4",
  "VACCINES",                    "step 01_1",
  "OBSERVATION_PERIODS",         "step 01_2",
  "OBSERVATION_PERIODS",         "step 01_3",
  "OBSERVATION_PERIODS",         "step 03_1",
  "PERSONS",                     "step 01_3",
  "step 01_1",                   "CONCEPT",
  "CONCEPT",                     "step 02_1",
  "step 01_2",                   "D3_output_spells_category",
  "D3_output_spells_category",   "step 03_1",
  "step 01_3",                   "D3_events_DEATH",
  "step 01_3",                   "D3_PERSONS",
  "D3_PERSONS",                  "step 03_1",
  "step 01_4",                   "covid_registry",
  "step 02_1",                   "D3_concepts_QC_criteria",
  "D3_concepts_QC_criteria",     "step 02_2",
  "step 02_2",                   "Flowchart_QC_criteria",
  "step 02_2",                   "selected_doses",
  "step 03_1",                   "D3_selection_criteria_doses",
  "D3_selection_criteria_doses", "step 03_2",
  "step 03_2",                   "persons_doses",
  
  "SURVEY_ID_invisible",                   "step 01_4",
  "SURVEY_OBSERVATIONS_invisible",         "step 01_4",
  "VACCINES_invisible",                    "step 01_1",
  "OBSERVATION_PERIODS_invisible",         "step 01_2",
  "OBSERVATION_PERIODS_invisible",         "step 01_3",
  "OBSERVATION_PERIODS_invisible",         "step 03_1",
  "PERSONS_invisible",                     "step 01_3"
)
edges <- dplyr::bind_rows(edges_init, edges)
# edges <- dplyr::bind_rows(edges_init, edges, data.frame(from = 100, to = 1, length = 1000))

all_groups <- unique(nodes$group)
all_groups <- all_groups[!all_groups %in% c("0", "0_invisible", "2", "4", "6")]

visNetwork(nodes, edges, main = "A really simple example") %>%
  visPhysics(stabilization = T) %>%
  visEdges(smooth = F, shadow = TRUE, arrows = "to") %>% 
  visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = TRUE
             # ,collapse = list(enabled = TRUE, clusterOptions = list(shape = "square"))
             ) %>% 
  visHierarchicalLayout() %>%
  visClusteringByGroup(groups = all_groups) %>%
  # visEdges(smooth = FALSE) %>%
  visInteraction(dragNodes = FALSE, hover = T) %>%
  visEvents(selectNode = "function(properties) {
      window.open(this.body.data.nodes.get(properties.nodes[0]).url_test);}") %>%
  visLegend() %>%
  visSave(file = "index.html")
